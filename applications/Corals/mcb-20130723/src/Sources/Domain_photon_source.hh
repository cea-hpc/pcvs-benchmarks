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

#ifndef __Domain_photon_source_hh__
#define __Domain_photon_source_hh__

//===========================================================================//
//
//    class Domain_photon_source - 
//    This class adds photons coming from another domain to an IMC 
//    It does NOT change positions - that is, it assumes that the domains AND 
//    the faces between them are are contiguous.
//
//===========================================================================//

#include <iostream>
#include <cmath>
#include <vector>

#include "external_source.hh"
#include "Domain_exchange.hh"
#include "ASSERT.hh"

//    Need to template on mesh, so we can have a member of class
//    Mesh_type::photon_type. A Mesh object is passed into the constructor,
//    but there is no reference to it kept in the DD object. The Mesh
//    reference is only used in the constructor to check on zone values in an
//    assertion.

namespace IMC_namespace
{

template<typename mesh_types, typename mat_types>
class Domain_photon_source 
    : public external_source<mesh_types, mat_types>
{
  public:
    typedef typename mesh_types::Mesh_type Mesh_type;
    typedef typename mesh_types::Vector3d Vector3d;
    typedef typename mesh_types::particle_zone_ID_type particle_zone_ID_type;
    typedef photon<particle_zone_ID_type, Vector3d> photon_type;

//    constructor that takes Meshes and figures out info it needs
    typedef Domain_exchange<Mesh_type,photon_type> DE_type;
    Domain_photon_source( int my_proc,
                          const std::vector<DE_type*>& DE_list_in,
                          const Mesh_type& Mesh_in );

//    function that adds photons from the Domain_exchange object to the
//    photon_list of the Source_data_base which owns this object.
    void add_source_photons( std::list<photon_type>& photon_list,
                             double& E_entered,
                             bool flush_buffer,
                             unsigned long long& N_entered );      
                                               
    void setUp();
    void finalize();
    
  private:          
//    Domain_exchange objects used to get photons from other domains.
    std::vector<DE_type*> DE_list;
//    List of receive requests.
    std::vector<MPI_Request> requests;
    
//    Mesh this Domain is on
    const Mesh_type& Mesh;
    
//    make these private so they can't be used
    Domain_photon_source();
    Domain_photon_source( const Domain_photon_source& );
    Domain_photon_source& operator=( const Domain_photon_source& );
}; 

}

#endif    //    __Domain_photon_source_hh__


