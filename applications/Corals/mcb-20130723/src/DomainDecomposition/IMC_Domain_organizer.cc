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

#include <mpi.h>

#include "IMC_Domain_organizer.hh"

namespace IMC_namespace
{

//---------------------------------------------------------------------------//

template<typename mesh_types, typename mat_types, typename Source_DB_type>
IMC_Domain_organizer<mesh_types, mat_types, Source_DB_type>::
IMC_Domain_organizer( const Mesh_type& Mesh_in, unsigned int buffer_size )
    : Domain_organizer<Mesh_type,photon_type>( Mesh_in, buffer_size ),
      Domain_source_ptr( 0 )
{ }
      
//---------------------------------------------------------------------//

//    Calls Domain_organizer::setUpDomainDecompositionBCs to set up

//    Boundary Conditions, and then sets up the Domain_source

template<typename mesh_types, typename mat_types, typename Source_DB_type>
void IMC_Domain_organizer<mesh_types, mat_types, Source_DB_type>::
setUpDomainDecomposition( Source_DB_type& Source_DB,
                          BC_vector& IMC_BCs )
{
//    Make sure pointer is NULL;
    ASSERT( Domain_source_ptr == 0 );

//    create BCs and add them to IMC BCs
    setUpDomainDecompositionBCs( IMC_BCs );
    
//    Make Domain_source
    typedef Domain_photon_source<mesh_types, mat_types> D_Source_type;
    Domain_source_ptr = new D_Source_type( this->thisMPITask,
                                           this->DE_list,
                                           this->Mesh );

//    Give Domain_source to the Source_data_base
    Source_DB.add_external_source( Domain_source_ptr );
}

//---------------------------------------------------------------------------//

//    destructor calls delete on memory that was allocated by call to new in
//    the function setUpDomainDecomposition

template<typename mesh_types, typename mat_types, typename Source_DB_type>
IMC_Domain_organizer<mesh_types, mat_types, Source_DB_type>::
~IMC_Domain_organizer()
{
    delete Domain_source_ptr;
}

//---------------------------------------------------------------------------//

}    //    namespace IMC_namespace


