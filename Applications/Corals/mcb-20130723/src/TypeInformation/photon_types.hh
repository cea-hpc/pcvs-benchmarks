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

#ifndef __photon_types_hh__
#define __photon_types_hh__

#include "photon.hh"

namespace IMC_namespace
{

template<typename mesh_types, typename mat_types>
class photon_types
{
  public:

    typedef typename mesh_types::Mesh_type Mesh_type;
    typedef typename mesh_types::particle_zone_ID_type particle_zone_ID_type;
    typedef typename mesh_types::Vector3d Vector3d;

    typedef photon<particle_zone_ID_type,
                   Vector3d> photon_type;

   

};

}     //    namespace IMC_namespace

#endif    //    __photon_types_hh__
