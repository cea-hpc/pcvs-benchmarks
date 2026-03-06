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

#ifndef __stand_alone_mat_types_hh__
#define __stand_alone_mat_types_hh__

//===========================================================================//
//
//    class stand_alone_mat_types - collects types related to stand-alone
//    version of the Material_data_base type
//
//===========================================================================//

#include "Material_data_base.hh"

namespace IMC_namespace
{

template<typename stand_alone_Mesh_type>
class stand_alone_mat_types
{
  public:
    
    typedef Material_data_base<stand_alone_Mesh_type> Material_data_base_type;

};

}     //    namespace IMC_namespace

//---------------------------------------------------------------------------//

#endif    //    __stand_alone_mat_types_hh__
