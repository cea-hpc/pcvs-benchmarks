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

//===========================================================================//
//
//    class external_source - a source of photons injected
//    into an IMC simulation from outside, for example, from another domain.
//    This is different than the sources described by the photon_source class,
//    which are subject to russian rouletting, generate photons one at a time,
//    and represent physical sources with a given energy that is a known function
//    of parameters known at the begining of the timestep. These sources can 
//    produce photons many times during a step, as (for example) other
//    domains generate photons.
//
//===========================================================================//

#ifndef __external_source_hh__
#define __external_source_hh__

#include <iostream>
#include <cmath>
#include <list>

#include "photon.hh"

//    need to template on mesh, so we can get hold of mesh attributes
//    like face areas and zone volumes

namespace IMC_namespace
{

template<typename mesh_types, typename mat_types>
class external_source
{
  public:
    typedef typename mesh_types::Mesh_type Mesh_type;
    typedef typename mesh_types::Vector3d Vector3d;
    typedef typename mesh_types::particle_zone_ID_type particle_zone_ID_type;
    typedef photon<particle_zone_ID_type, Vector3d> photon_type;
    
//    put photons onto photon list of IMC
    virtual void add_source_photons( std::list<photon_type>& source_photons,
                                     double& E_entered, 
                                     bool flush_buffer,
                                     unsigned long long& N_entered ) = 0;
                                           
    virtual void setUp() = 0;
    virtual void finalize() = 0;

    external_source()  { }

    virtual ~external_source() { }

  private:
//    make these private so they can't be used
    external_source( const external_source& ) {}
    external_source& operator=( const external_source& ) { return *this; }
};

}    //    namespace IMC_namespace

#endif


