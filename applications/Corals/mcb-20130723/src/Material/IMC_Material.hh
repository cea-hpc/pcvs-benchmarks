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

#ifndef __Material_hh__
#define __Material_hh__

// Material class for IMC

#include <list>
#include <cmath>

#include "ASSERT.hh"

namespace IMC_namespace
{

class Material
{
  public:
   
    // Used in Material_data_base constructor
    Material& operator=( const Material& rhs );

    // used in assertions
    bool Material_consistent() const;
};

//---------------------------------------------------------------------------//
//    definitions of inlined functions
//---------------------------------------------------------------------------//

//    need = operator in Material_data_base constructor

inline Material& Material::
operator=( const Material& rhs )
{
    if( this == &rhs )
        return *this;
                    
    return *this;
}

//---------------------------------------------------------------------------//
//    used in assertions
  
inline bool Material::Material_consistent() const
{
   return true;
}
  
//---------------------------------------------------------------------------//

}  //    namespace IMC_namespace

#endif                          // __Material_hh__



