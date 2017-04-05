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

//  this class is the first cut at a virtual class to handle absorption opacity
//  functions. Since more sophisticated models may need more functions,
//  there could conceivably be an absorption_opacity2 class, etc.

#ifndef __absorption_opacity_base_hh__
#define __absorption_opacity_base_hh__

#include "rng.hh"
#include "zcf.hh"

namespace IMC_namespace
{

class absorption_opacity_base
{
  public:
    absorption_opacity_base() {}

    virtual double Planck_opacity() const = 0;

    virtual double absorption_opacity() const = 0;

    virtual ~absorption_opacity_base( ) { }
    
  private:
//    make these private so they can't be used
    absorption_opacity_base( const absorption_opacity_base& ); 
    absorption_opacity_base& operator=( const absorption_opacity_base& );
};

}    //    namespace IMC_namespace

#endif




