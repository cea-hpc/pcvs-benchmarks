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


//    need this to get printf for unsigned long long
#include <cstdio>
#include <iostream>
#include <iomanip>
#include <cmath>

#include "photon.hh"

namespace IMC_namespace
{

//---------------------------------------------------------------------------//

//    used in Assertions to make sure photon isn't flawed

template<typename particle_zone_ID_type, 
         typename Vector3d>
bool photon<particle_zone_ID_type, Vector3d>::
consistent() const
{
    if( Energy <= 0.0 )
        return false;

    if( Energy0 < 0.0 )
        return false;

    if( !rand.good_state() )
        return false;

    if( !( std::fabs(Omega.magnitude() - 1.) < 1.e-6 ) )
        return false;

// if no problems above, it's consistent, so return true
    return true;
}

//---------------------------------------------------------------------------//

template<typename particle_zone_ID_type, 
         typename Vector3d>
void photon<particle_zone_ID_type, Vector3d>::
print() const
{
    using std::endl;
    using std::cout;
    using std::printf;
    using namespace std;

    //cout << setiosflags(std::ios::scientific);
    //cout << setprecision(18);

    cout << "  XYZ-position: " << X << endl;
    cout << "  direction: " << Omega << endl;
    cout << "  zone: " << zone << endl;

    cout << "  photon_time: " << t << endl;

    cout << "  justEmitted: " << justEmitted << endl;

    cout << "  Energy: " << Energy 
         << ",  Energy0: " << Energy0 << endl;

    printf("  state: %lluULL\n", state() );
    printf("  initial state: %lluULL\n",  initial_state() );
}

//---------------------------------------------------------------------------//

} // end IMC_namespace

//---------------------------------------------------------------------------//
//                              end of photon.cc
//---------------------------------------------------------------------------//


