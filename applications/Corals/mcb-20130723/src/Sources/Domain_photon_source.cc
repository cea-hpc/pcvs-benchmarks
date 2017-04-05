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

#include "Domain_photon_source.hh"
#include <cstdio>

using namespace std;

namespace IMC_namespace
{

//---------------------------------------------------------------------------//

//    constructor

template<typename mesh_types, typename mat_types>
Domain_photon_source<mesh_types, mat_types>::
Domain_photon_source( int my_proc,
                      const vector<DE_type*>& DE_list_in,
                      const Mesh_type& Mesh_in )
  : DE_list( DE_list_in ),
    requests( DE_list.size() ),
    Mesh( Mesh_in )
{ }

//---------------------------------------------------------------------------//

//    function that adds photons from the Domain_exchange object to the
//    photon_list of the Source_data_base which owns this object.

template<typename mesh_types, typename mat_types>
void Domain_photon_source<mesh_types, mat_types>::
add_source_photons( list<photon_type>& photon_list, 
                    double& E_entered,
                    bool flush_buffer,
                    unsigned long long& N_entered )
{    
   // Todd and Tom said to check for particles, if nothing,
   // flush buffer, then check for particles again.
   // I think this will happen soon enough anyway,
   // when this function is called again.

   const unsigned int size = DE_list.size();
   ASSERT( size == requests.size() );

   std::vector<int> done_indices(size);
   std::vector<MPI_Status> stats(size);

   int num_done;

   MPI_Testsome( size, &requests[0], &num_done, &done_indices[0], &stats[0] );

   for(int d = 0; d<num_done; ++d)
      DE_list[done_indices[d]]->get_and_add_particles( photon_list );

   //    Number of photons from other domain
   N_entered += photon_list.size();
   
   //    energy of photons from other domain
   for( typename list<photon_type>::const_iterator photonItr =
        photon_list.begin(); 
        photonItr != photon_list.end(); ++photonItr)
   {
      E_entered += photonItr->Energy;
   }

   // Only flush buffer in no new photons from any processors.
   if(flush_buffer && photon_list.empty())
      for( unsigned int d = 0; d < DE_list.size(); ++d )
      {
          DE_list[d]->send_particles();
      }

}

//---------------------------------------------------------------------------//

template<typename mesh_types, typename mat_types>
void Domain_photon_source<mesh_types, mat_types>::
setUp()
{    
   
   ASSERT( DE_list.size() == requests.size() );
   for( unsigned int d = 0; d < DE_list.size(); ++d){

      DE_list[d]->set_request( &requests[d] );
      DE_list[d]->setUp();
   }
 
}

//---------------------------------------------------------------------------//

template<typename mesh_types, typename mat_types>
void Domain_photon_source<mesh_types, mat_types>::
finalize()
{    
   
   typename vector< Domain_exchange<Mesh_type,photon_type>* >::iterator DE;
   for( DE = DE_list.begin(); DE != DE_list.end(); ++DE )
      (*DE)->finalize();
 
}


}    //    namespace IMC_namespace


