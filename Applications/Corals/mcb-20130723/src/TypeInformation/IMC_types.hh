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

#ifndef __IMC_types_hh__
#define __IMC_types_hh__

//---------------------------------------------------------------------------//

#include "photon_types.hh"
#include "Boundary_Condition.hh"

// #include "fcf.hh"
#include "boost/unordered_map.hpp"

namespace IMC_namespace
{

template<typename mesh_types, typename mat_types>
class IMC_types
{
  public:
    typedef typename mesh_types::Mesh_type Mesh_type;
    typedef typename photon_types<mesh_types,mat_types>::photon_type photon_type;

//     typedef fcf<Mesh_type,
//                 Boundary_Condition<Mesh_type,photon_type>*> fcf_BCPtr_type;
    typedef boost::unordered_map<typename Mesh_type::particle_face_ID_type, 
                                 Boundary_Condition<Mesh_type,photon_type>*> fcf_BCPtr_type;

};

}     //    namespace IMC_namespace

//---------------------------------------------------------------------------//

#endif    //    __IMC_types_hh__

