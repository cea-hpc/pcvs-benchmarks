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

#include <cmath>

#include "constant_absorp_opac.hh"

namespace IMC_namespace
{

//---------------------------------------------------------------------------//

//     constructor

constant_absorp_opac::
constant_absorp_opac( double sigma_in )
    : absorption_opacity_base( )
{
    using namespace std;

    sigma = sigma_in;

    if( sigma <= 0.0 )
    {
        cout << "Error in constant_absorp_opac ctor !" << endl;
        cout << "absorption opacity must be > 0 " << endl;
        cout << "but was set to value " << sigma << endl;
        exit(-1);
    }
    
    ASSERT( sigma > 0.0 );
}

//---------------------------------------------------------------------------//

double constant_absorp_opac::
Planck_opacity() const
{
    ASSERT( sigma >= 0.0 );
    
    return sigma;
}

//---------------------------------------------------------------------------//

double constant_absorp_opac::
absorption_opacity() const
{
    ASSERT( sigma >= 0.0 );
    
    return sigma;
}

//---------------------------------------------------------------------------//

}    //    namespace IMC_namespace



