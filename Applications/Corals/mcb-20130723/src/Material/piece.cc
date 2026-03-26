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

#include "piece.hh"

using namespace std;

namespace IMC_namespace
{

//---------------------------------------------------------------------------//

piece::piece( const std::vector<unsigned int>& zone_list_in )
    : zone_list( zone_list_in.size() )
{
//    Load values into arrays
    for( unsigned int local_zone = 0; local_zone < zone_list.size(); local_zone++ )
    {
        zone_list[local_zone] = zone_list_in[local_zone];
     }
}

//---------------------------------------------------------------------------//

}    //    namespace IMC_namespace
