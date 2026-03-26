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

//    The following routine takes as input the 2 relative angles above and
//    the direction cosines of the normal to a face, and outputs the new
//    direction cosines.

//    It is called by particle source routines and to get new angles after
//    scattering in advance_particle

#include <cmath>

#include "rotate_angles.hh"
#include "ASSERT.hh"

using namespace std;

namespace IMC_namespace
{

template<typename Vector3d>
Vector3d rotate_angles( double cos_theta_z_rel, double phi,
                        const Vector3d& pole)
{

    ASSERT( cos_theta_z_rel >= -1.0 );
    ASSERT( cos_theta_z_rel <= 1.0 );
    ASSERT( std::fabs(pole.magnitude() - 1.) < 1.e-6 );

    double sin_theta_z_rel = sqrt( 1.0 - cos_theta_z_rel*cos_theta_z_rel );
    double cos_theta_x_rel = sin_theta_z_rel*cos(phi);
    double cos_theta_y_rel = sin_theta_z_rel*sin(phi);

    double xy_term = pole.GetX()*cos_theta_x_rel + pole.GetY()*cos_theta_y_rel;

    Vector3d Omega;

    Omega.SetZ( pole.GetZ()*cos_theta_z_rel - xy_term );

    double cc;    //    same name as in newcos
    if( pole.GetZ() > 0.0 )
    {
        cc = cos_theta_z_rel - xy_term/( 1.0 + pole.GetZ() );

        Omega.SetX( cos_theta_x_rel + cc*pole.GetX() );
        Omega.SetY( cos_theta_y_rel + cc*pole.GetY() );
    }
    else
    {
        cc = cos_theta_z_rel + xy_term/( 1.0 - pole.GetZ() );

        Omega.SetX( -cos_theta_x_rel + cc*pole.GetX() );
        Omega.SetY( -cos_theta_y_rel + cc*pole.GetY() );
    }

    ASSERT( std::fabs(Omega.magnitude() - 1.) < 1.e-6 );
    return Omega;
}

}    //    end of IMC_namespace

