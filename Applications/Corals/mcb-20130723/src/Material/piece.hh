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

#ifndef __piece_hh__
#define __piece_hh__

//===========================================================================//
//
//    class piece - used to provide a list of zones 
//    to the Material_data_base class. 
// 
//===========================================================================//

#include <list>
#include <vector>
#include <cmath>

#include "IMC_Material.hh"

namespace IMC_namespace
{

class piece
{
  public:
         
    piece( const std::vector<unsigned int>& zone_list );      
    
//    access functions
    int N_local_zones() const { return static_cast<int>(zone_list.size()); }

    unsigned int zone( unsigned int local_zone ) const { return zone_list[local_zone]; }
    
    const  Material& piece_Material() const { return Material_IC; }
   
  private:   
    std::vector<unsigned int> zone_list;
              
    Material Material_IC;
};

//---------------------------------------------------------------------------//

}    //    namespace IMC_namespace

#endif
       


