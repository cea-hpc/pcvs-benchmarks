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

#ifndef __SET_ISOTROPIC_COSINES_HH
#define __SET_ISOTROPIC_COSINES_HH

#include <cmath>
#include "ASSERT.hh"

namespace IMC_namespace
{

/*! \brief Change vector Omega so that it is some new random direction.

  Given 2 random numbers, gives 3 direction cosines corresponding to a
  randomly distributed direction

  An example where this is used is if scattering is isotropic. Then, we
  don't want to go through all of the angle conversions - just generate
  direction cosines directly
 */

template<typename Vector3d>
inline void set_isotropic_cosines( double r1, double r2, Vector3d& Omega)
{
    ASSERT( r1 > 0.0 );
    ASSERT( r1 <= 1.0 );
    ASSERT( r2 > 0.0 );
    ASSERT( r2 <= 1.0 );

    double cos_theta = 2.0*r1 - 1.0;

    //    NOTE - is loss of accuracy for small cos_theta a problem?
    double sin_theta = std::sqrt( 1.0 - cos_theta*cos_theta );

    double phi = (2.0*r2 - 1.0)*3.1415926535897931;

    Omega.SetX( sin_theta*std::cos(phi) );
    Omega.SetY( sin_theta*std::sin(phi) );
    Omega.SetZ( cos_theta );

    ASSERT( std::fabs(Omega.magnitude() - 1.) < 1.e-6 );
}

}    //    end of IMC_namespace

#endif // __SET_ISOTROPIC_COSINES_HH


