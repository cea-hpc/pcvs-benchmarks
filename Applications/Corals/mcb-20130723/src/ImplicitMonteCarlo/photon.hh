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

//    This class describes a photon used by the IMC class. It is
//    templated on the type used to identify zones. 
//
//---------------------------------------------------------------------------//

#ifndef __photon_hh__
#define __photon_hh__

#include <iostream>
#include "ASSERT.hh"
#include "rng.hh"

namespace IMC_namespace
{

template<typename particle_zone_ID_type, 
         typename Vector3d>
class photon
{
  public:
    explicit photon( unsigned long long state = 1 );

    bool justEmitted;

// how much energy photon carries
    double Energy;

// initial energy - used in russian roullette
    double Energy0;

//    position - 3D Cartesian
    Vector3d X;

//    direction cosines - keep 3 to avoid computations succeptable to roundoff
    Vector3d Omega;

// current photon time
    double t;

//    zone the photon is in - used to get geometry info
    particle_zone_ID_type zone;

//    move the photon d_path and advance the photon time
//    pass in const double& so compiler knows it can inline away the
//    temporary variables it might otherwise have to create for function
//    arguments that are passed by value
    inline void move( const double& d_path );

//    used in Assertions to make sure photon isn't flawed
    bool consistent() const;

// Get a random number.
    double random_number(){return rand.random_number();}
// Get the generator.  Use sparingly.
    rng& get_rng(){return rand;}

// get RNG state
    unsigned long long state() const { return rand.get_state(); }
//    needed when we create or split photons; then we need to set new seed
    void set_state( unsigned long long state_in ){
        rand.set_state(state_in);
    //    NOTE - for debugging
        _initial_state = state_in;
    }

    void print() const;    //    print out photon info; replaced op<<
//void printToFile(std::ostream&) const;    // print photon info to file

//    NOTE - for debugging
    unsigned long long initial_state() const { return _initial_state; }
// For restart
    void set_initial_state( unsigned long long initial_state_in ) {
        _initial_state = initial_state_in;
    }

//    for use in Boundary condition classes
    typedef Vector3d Vector_type;

  private:

    rng rand;
//    NOTE - for debugging
    unsigned long long _initial_state;

};

//---------------------------------------------------------------------------//
//    definitions of inlined functions
//---------------------------------------------------------------------------//

template <typename particle_zone_ID_type, 
          typename Vector3d>
inline photon<particle_zone_ID_type, Vector3d>::
photon( unsigned long long state )
    : justEmitted(false),
      Energy(0.0),
      Energy0(0.0),
      t(0.0),
      zone(),
      rand(state),
      _initial_state(state)  //    NOTE - for debugging
{ }

//---------------------------------------------------------------------------//

template <typename particle_zone_ID_type, 
          typename Vector3d>
inline void 
photon<particle_zone_ID_type, Vector3d>::
move( const double& d_path )
{
    using namespace std;

    X += Omega*d_path;
   
    t += d_path;

    //std::cout << std::setiosflags(std::ios::scientific);
    //std::cout << std::setprecision(15);

    //std::cout << "photon::move():" 
    //   << "\n\tinit\t" << _initial_state  << ' '
    //   << "\n\tstate\t" << rand.get_state() << ' '
    //   << "\n\tEnergy\t" << Energy << ' '
    //   << "\n\tEnergy0\t" << Energy0 << ' '
    //   << "\n\tX\t" << X.GetX() << ' '
    //   << "\n\tY\t" << X.GetY() << ' '
    //   << "\n\tZ\t" << X.GetZ() << ' '
    //   << "\n\tO_x\t" << Omega.GetX() << ' '
    //   << "\n\tO_y\t" << Omega.GetY() << ' '
    //   << "\n\tO_z\t" << Omega.GetZ() << ' '
    //   << "\n\tt\t" << t << ' '
    //   << "\n\td_path\t" << d_path << std::endl;
}

//---------------------------------------------------------------------------//

}    //    namespace IMC_namespace

#endif




