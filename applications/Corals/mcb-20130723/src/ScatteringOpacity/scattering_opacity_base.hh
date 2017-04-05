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

//  This class is the virtual base class for scattering opacity classes.

#ifndef __scattering_opacity_base_hh__
#define __scattering_opacity_base_hh__

#include "rng.hh"

namespace IMC_namespace
{

class scattering_opacity_base
{
  public:
    scattering_opacity_base() { };

    virtual double scattering_opacity() const = 0;

//    returns angle relative to photon's current direction
    virtual void scattering_results( rng& rand,
                                     double& new_cos_theta ) const = 0;
    
    virtual ~scattering_opacity_base( ) { };

  private:
//    make these private so they can't be used
    scattering_opacity_base( const scattering_opacity_base& ) {}
    scattering_opacity_base& operator=( const scattering_opacity_base& ) { return *this; }

};

}    //    namespace IMC_namespace

#endif



