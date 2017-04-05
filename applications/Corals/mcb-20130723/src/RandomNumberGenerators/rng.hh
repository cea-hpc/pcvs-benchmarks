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

//    This class impliments rng random number generator

#ifndef __rng_hh__
#define __rng_hh__

#include <iostream>
#include "ASSERT.hh"

namespace IMC_namespace
{

namespace rng_tools{

//    used to hash a 64 bit int to get an initial state
    unsigned long long hash_state( unsigned long long initial_number );

}

class rng
{
  public:

//    state for this rng is a 64 bit int
    typedef unsigned long long rng_state_type;

    explicit rng(rng_state_type state_in = 0):_state(state_in){}

    double random_number( rng_state_type state ) const;
    double random_number();

    void set_state( rng_state_type state );

    rng_state_type get_state() const { return _state; }

    bool good_state() const { return _state != 0; }

  private:

// state of RNG.
    rng_state_type _state;
};

//---------------------------------------------------------------------------//

inline
double rng::random_number()
{
    _state = 2862933555777941757ULL*_state + 3037000493ULL;

    ASSERT( 5.4210108624275222e-20*_state > 0 );
    ASSERT( 5.4210108624275222e-20*_state <= 1 );

//    map int in (0,2**64) to (0,1) by multiplying
//    by 1/(2**64 - 1) = 1/18446744073709551615
    return 5.4210108624275222e-20*_state;
}

//---------------------------------------------------------------------------//

inline
double rng::random_number( rng_state_type state ) const
{
    rng_state_type result = 2862933555777941757ULL*state + 3037000493ULL;

    EXPENSIVE_ASSERT( 5.4210108624275222e-20*result > 0 );
    EXPENSIVE_ASSERT( 5.4210108624275222e-20*result <= 1 );

//    map int in (0,2**64) to (0,1) by multiplying
//    by 1/(2**64 - 1) = 1/18446744073709551615
    return 5.4210108624275222e-20*result;
}

//---------------------------------------------------------------------------//

inline
void rng::set_state( rng_state_type state )
{
    EXPENSIVE_ASSERT( state != 0 );

    _state = state;

    ASSERT( _state != 0 );
}

//---------------------------------------------------------------------------//

}    //    namespace IMC_namespace

#endif




