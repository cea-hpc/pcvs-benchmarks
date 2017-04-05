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

#ifndef __stand_alone_mesh_types_hh__
#define __stand_alone_mesh_types_hh__

//===========================================================================//
//
//    class stand_alone_mesh_types - collects types related to the
//    stand-alone meshes
//
//===========================================================================//

#include "zcf.hh"
#include "fcf.hh"
#include "Vector3d.hh"

namespace IMC_namespace
{

template<typename stand_alone_mesh_type>
class stand_alone_mesh_types
{
  public:
    typedef stand_alone_mesh_type Mesh_type;
    
    typedef typename stand_alone_mesh_type::ZoneIterator ZoneIterator;
    typedef typename stand_alone_mesh_type::mesh_zone_ID_type mesh_zone_ID_type;
    typedef typename stand_alone_mesh_type::mesh_face_ID_type mesh_face_ID_type;
    typedef typename stand_alone_mesh_type::particle_zone_ID_type particle_zone_ID_type;
    typedef typename stand_alone_mesh_type::particle_face_ID_type particle_face_ID_type;
    typedef Vector3d_namespace::Vector3d Vector3d;

    typedef zcf<stand_alone_mesh_type, double> zcf_double;
    typedef zcf<stand_alone_mesh_type, Vector3d> zcf_Vector3d;
    typedef zcf<stand_alone_mesh_type, unsigned long long> zcf_UINT64;
    typedef zcf<stand_alone_mesh_type, long long> zcf_SINT64;

    typedef fcf<stand_alone_mesh_type,double> fcf_double;
};

}   //    end of namespace IMC_namespace

//---------------------------------------------------------------------------//

#endif    //    __stand_alone_mesh_types_hh__
