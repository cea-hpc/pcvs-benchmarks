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

// This class impliments functions for rng random number generator

#include "rng.hh"

namespace IMC_namespace
{

//---------------------------------------------------------------------------//

//  Break a 64 bit state into 2 32 bit ints.
inline void break_up_ull( unsigned long long ull_in,
                          unsigned int& front_bits, unsigned int& back_bits )
{
// decompose it
    front_bits = static_cast<unsigned int>( ull_in >> 32 );
    back_bits = static_cast<unsigned int>( ull_in & 0xffffffff );
}

//---------------------------------------------------------------------------//

//    Used to hash a 64 bit int into another, unrelated one. It does this in 2
//    32 bit chuncks. This function uses the algorithm from Numerical Recipies in C,
//    2nd edition: psdes, p302.
//    This is used to make 64 bit numbers for use as initial states for the 64 bit
//    lcg random number generator.

inline void pseudo_des( unsigned int& lword, unsigned int& irword )
{
// This random number generator assumes that type unsigned int is a 32 bit int
// = 1/2 of a 64 bit int. The sizeof operator returns the size in bytes = 8 bits.

    EXPENSIVE_ASSERT( sizeof(unsigned int) == 4 );


    const int NITER = 2;
    ASSERT( (NITER % 2) == 0 );
    const unsigned int c1[] = { 0xbaa96887L, 0x1e17d32cL, 0x03bcdc3cL, 0x0f33d1b2L };
    const unsigned int c2[] = { 0x4b0f3b58L, 0xe874f0c3L, 0x6955c5a6L, 0x55a7ca46L};
    ASSERT( NITER < 5 );

    unsigned int ia,ib,iswap,itmph=0,itmpl=0;

    for( int i = 0; i < NITER; i++)
    {
        ia = ( iswap = irword ) ^ c1[i];
        itmpl = ia & 0xffff;
        itmph = ia >> 16;
        ib = itmpl*itmpl+ ~(itmph*itmph);

        irword = lword ^ (((ia = (ib >> 16) |
                            ((ib & 0xffff) << 16)) ^ c2[i])+itmpl*itmph);

        lword=iswap;
    }
}

//---------------------------------------------------------------------------//

//    Function used to reconstruct  a 64 bit from 2 32 bit ints.

inline unsigned long long reconstruct_ull(
    unsigned int front_bits, unsigned int back_bits )
{
    unsigned long long reconstructed, temp;
    reconstructed = static_cast<unsigned long long>( front_bits );
    temp = static_cast<unsigned long long>( back_bits );

// shift first bits 32 bits to left
    reconstructed = reconstructed << 32;

// temp must be masked to kill leading 1's.  Then 'or' with reconstructed
// to get the last bits in
    reconstructed |= (temp & 0x00000000ffffffff);

    return reconstructed;
}

//---------------------------------------------------------------------------//

//    used to hash a 64 bit int to get an initial state

unsigned long long rng_tools::hash_state( unsigned long long initial_number )
{
//    pull initial number apart into 2 32 bit ints
    unsigned int front_bits, back_bits;
    break_up_ull( initial_number, front_bits, back_bits );

//    hash the bits
    pseudo_des( front_bits, back_bits );

//    put the hashed parts together into 1 64 bit int
    return reconstruct_ull( front_bits, back_bits );
}

//---------------------------------------------------------------------------//

}    //    namespace IMC_namespace

