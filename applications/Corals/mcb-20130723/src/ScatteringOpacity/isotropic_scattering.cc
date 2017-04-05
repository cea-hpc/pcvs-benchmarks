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

//  this class has data and functions needed for simulating isotropic scattering

#include <cmath>
#include <iostream>

#include "isotropic_scattering.hh"

using namespace std;

namespace IMC_namespace
{

//---------------------------------------------------------------------------//

//    constructor

isotropic_scattering::isotropic_scattering( double sigma_in )
    : scattering_opacity_base( )
{
   sigma_isotropic = sigma_in;
}   

//---------------------------------------------------------------------------//

double isotropic_scattering::scattering_opacity() const
{
    ASSERT( sigma_isotropic >= 0.0 );
    
    return sigma_isotropic;
}

//---------------------------------------------------------------------------//

void isotropic_scattering::
scattering_results( rng& rand,
                    double& new_cos_theta ) const
{
//    direction of scatter is random, so cos_theta is randomly distributed
    double r = rand.random_number();
    new_cos_theta = 1.0 - 2.0*r;

    ASSERT( new_cos_theta >= -1.0 );
    ASSERT( new_cos_theta <= 1.0 );
}  

//---------------------------------------------------------------------------//

}    //    namespace IMC_namespace


    





