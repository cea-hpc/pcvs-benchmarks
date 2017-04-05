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

//    this class impliments opacity functions

#include <cmath>
#include <iostream>

#include "Opacity_data_base.hh"
#include "ASSERT.hh"

using namespace std;

namespace IMC_namespace
{

//---------------------------------------------------------------------------//

//    constructor 

template<typename mesh_types, typename mat_types>
Opacity_data_base<mesh_types, mat_types>::
Opacity_data_base( const Mesh_type& Mesh_in,
                   const Mat_DB_type& Mat_DB_in )
    : Mesh( Mesh_in ),
      Mat_DB( Mat_DB_in )
{ }

//---------------------------------------------------------------------------//

}    //    namespace IMC_namespace

