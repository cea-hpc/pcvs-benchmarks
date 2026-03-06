//----------------------------------*-C++-*----------------------------------//
// Copyright 2009 Lawrence Livermore National Security, LLC
// All rights reserved.
//---------------------------------------------------------------------------//

// This work performed under the auspices of the U.S. Department of Energy by
// Lawrence Livermore National Laboratory under Contract DE-AC52-07NA27344

//  DISCLAIMER
//  This work was prepared as an account of work sponsored by an agency of the
//  United States Government. Neither the United States Government nor the
//  Lawrence Livermore National Security, LLC, nor any of their employees,
//  makes any warranty, express or implied, including the warranties of
//  merchantability and fitness for a particular purpose, or assumes any
//  legal liability or responsibility for the accuracy, completeness, or
//  usefulness of any information, apparatus, product, or process disclosed,
//  or represents that its use would not infringe privately owned rights.

#include "Domain_organizer.hh"
#include "printGlobalInfo.hh"

#include <mpi.h>
#include <string>
#include <iostream>

using namespace std;

namespace IMC_namespace
{

//---------------------------------------------------------------------------//

template<typename Mesh_type, typename particle_type>
Domain_organizer<Mesh_type,particle_type>::
Domain_organizer( const Mesh_type& Mesh_in, unsigned int buffer_size )
    : Mesh( Mesh_in )
{ 
//    Make sure vectors are empty
    ASSERT( Domain_BC_list.empty() );

    // Call the private method to initialize the object.
    initialize( Mesh, buffer_size );
}
      
//---------------------------------------------------------------------//

template<typename Mesh_type, typename particle_type>
void Domain_organizer<Mesh_type,particle_type>::
setUpDomainDecompositionBCs( BC_vector& MonteCarlo_BCs )
{
//    Add BCs to the list of BC's passed into this function and used by the
//    Monte Carlo code using this class
    typedef Domain_BC<Mesh_type,particle_type> Domain_BC_type;
    for( typename vector<Domain_BC_type*>::iterator i = Domain_BC_list.begin(); 
         i != Domain_BC_list.end(); ++i )
    {
        MonteCarlo_BCs.push_back( *i );
    }

}

//---------------------------------------------------------------------//

template<typename Mesh_type, typename particle_type>
void Domain_organizer<Mesh_type,particle_type>::
reinitialize( const Mesh_type& Mesh, BC_vector& MonteCarlo_BCs, 
              unsigned int buffer_size )
{
   // Call the private method to initialize the object.
   initialize( Mesh, buffer_size );
   
   // Update the list of boundary conditions.
   setUpDomainDecompositionBCs( MonteCarlo_BCs );
}

//---------------------------------------------------------------------//

template<typename Mesh_type, typename particle_type>
void Domain_organizer<Mesh_type,particle_type>::
initialize( const Mesh_type& Mesh, unsigned int buffer_size )
{
//    Get the MPI process ID.
    thisMPITask=0;
#ifdef USE_MPI
#pragma omp master 
    {
        MPI_Comm_rank(Mesh.getWorldCommunicator(), &thisMPITask);
    }
    
#endif

    ASSERT( thisMPITask >= 0 );

//    Free any required memory and resize lists to zero length.
    typedef Domain_exchange<Mesh_type,particle_type> Domain_exchange_type;
    typedef typename vector<Domain_exchange_type*>::iterator DE_iterator;
    if (!DE_list.empty())
    {
       for( DE_iterator i = DE_list.begin(); i != DE_list.end(); ++i )
       {
          delete *i;
       }
       DE_list.resize(0);
    }
    typedef Domain_BC<Mesh_type,particle_type> Domain_BC_type;
    typedef typename vector<Domain_BC_type*>::iterator DBC_iterator;
    if (!Domain_BC_list.empty())
    {
       for( DBC_iterator i = Domain_BC_list.begin(); 
            i != Domain_BC_list.end(); ++i )
       {
          delete *i;
       }
       Domain_BC_list.resize(0);
    }

//    list of neighboring domains  
    vector<int> neighborDomains = Mesh.getNeighborDomains();

//    Make a Domain_BC for each neghboring domain, and give the Domain_BC one
//    Domain_exchange object for each MPI task simulating that domain (one
//    Domain_exchange for each replica of the neghboring domain.)
    for( vector<int>::const_iterator domainItr = neighborDomains.begin();
        domainItr != neighborDomains.end(); ++domainItr )
    {
    //    get faces bordering neighboring domain
        vector<mesh_face_ID_type> Domain_face_list = 
            Mesh.getBorderFacesFromDomain( *domainItr );

    //    get all MPI tasks simulating neighbor domain
        vector<int> neighborMPIs = Mesh.getNeighborMPITasks( *domainItr );

    //    make a DE for each neighbor MPI task
        vector<Domain_exchange_type*> DEsForCurrentDomain(0);
        for( vector<int>::iterator nMPITaskItr = neighborMPIs.begin();
             nMPITaskItr != neighborMPIs.end(); ++nMPITaskItr )
        {
            Domain_exchange_type* DE;
            DE = new Domain_exchange_type( thisMPITask, *nMPITaskItr, Mesh, buffer_size );
            
            DEsForCurrentDomain.push_back( DE );
            
        //    this list is of all DEs created - used in dtor to delete them
            DE_list.push_back( DE );
        }
        
    //    Make the Domain_BC that uses the faces and Domain_exchange objects
       Domain_BC_type* DBC;
       DBC = new Domain_BC_type( Domain_face_list, DEsForCurrentDomain, Mesh );
       
       Domain_BC_list.push_back( DBC );
       
    } //  end loop over neighboring domains
}

//---------------------------------------------------------------------//

template<typename Mesh_type, typename particle_type>
void Domain_organizer<Mesh_type,particle_type>::
diagnosticOutput() const
{
   // Assumes DE_list will either be empty on all processors or 
   // not empty on all.  Some processors can't be empty and other are.
   if( !DE_list.empty() )
   {
#ifdef USE_MPI
      MPI_Barrier( Mesh.getWorldCommunicator() );
#endif
      std::vector<unsigned long long> tallies(4);
      std::vector<std::string> names(4);

      names[0] = "N_buffered";
      names[1] = "N_received";
      names[2] = "N_buffers_sent";
      names[3] = "N_buffers_received";

      for( DE_const_it i = DE_list.begin(); i != DE_list.end(); ++i )
      {
         tallies[0] += (*i)->get_N_buffered();
         tallies[1] += (*i)->get_N_received();
         tallies[2] += (*i)->get_N_buffers_sent();
         tallies[3] += (*i)->get_N_buffers_received();
      }

      int root = 0;
      if(thisMPITask == root)
      {
         std::cout << "Domain_organizer particle exchange information:\n";
      }

      printGlobalInfo( names, tallies, root, false );

#ifdef USE_MPI
      MPI_Barrier( Mesh.getWorldCommunicator() );
#endif
   }

}

//---------------------------------------------------------------------------//

//    destructor calls delete on memory that was allocated by calls to new in
//    the function setUpDomainDecompositionBCs

template<typename Mesh_type, typename particle_type>
Domain_organizer<Mesh_type,particle_type>::
~Domain_organizer()
{
    typedef Domain_exchange<Mesh_type,particle_type> Domain_exchange_type;
    typedef typename vector<Domain_exchange_type*>::iterator DE_iterator;
    for( DE_iterator i = DE_list.begin(); i != DE_list.end(); ++i )
    {
        delete *i;        
    }

    typedef Domain_BC<Mesh_type,particle_type> Domain_BC_type;
    typedef typename vector<Domain_BC_type*>::iterator DBC_iterator;
    for( DBC_iterator i = Domain_BC_list.begin(); 
         i != Domain_BC_list.end(); ++i )
    {
        delete *i;        
    }
}

//---------------------------------------------------------------------------//

}    //    namespace IMC_namespace

