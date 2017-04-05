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

//  this class has data and functions needed for simulating isotropic sacttering

#ifndef __isotropic_scattering_hh__
#define __isotropic_scattering_hh__

#include "ASSERT.hh"
#include "scattering_opacity_base.hh"
#include "rng.hh"

namespace IMC_namespace
{

class isotropic_scattering : public scattering_opacity_base
{
  public:
    explicit isotropic_scattering( double sigma_in );

    double scattering_opacity() const;

//    returns angle relative to photon's current direction
    void scattering_results( rng& rand,
                             double& new_cos_theta ) const;
    
  private:

    double sigma_isotropic;

//    make these private so they can't be used
    isotropic_scattering( );
    isotropic_scattering( const isotropic_scattering& opacity_in );
    isotropic_scattering& operator=( const isotropic_scattering& );
};

}    //    namespace IMC_namespace

#endif
