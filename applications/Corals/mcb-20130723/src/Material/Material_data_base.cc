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

//===========================================================================//
//
//    class Material_data_base - holds material data needed by IMC class
//    and source functions
//
//===========================================================================//

#include <iostream>
#include "Material_data_base.hh"
#include "printGlobalInfo.hh"
#ifdef USE_MPI
#include <mpi.h>
#endif

using namespace std;
using Vector3d_namespace::Vector3d;

namespace IMC_namespace
{

//---------------------------------------------------------------------------//

//    Constructor for Material_data_base objects with only one piece
//    The IC data comes in through a single piece object

template<class Mesh_type>
Material_data_base<Mesh_type>::
Material_data_base( const Mesh_type& Mesh_in,
                    const piece& piece )
    : mMesh( Mesh_in ),
      N_regions( 1 ),
      region_list( Mesh_in ),
      Material_list( 1 ),
      mIsSetUp( false )
{
//    Since there is one piece, every zone is in region 0
    for( unsigned int zone = mMesh.first_real_zone();
         zone <= mMesh.last_real_zone();
         zone = mMesh.next_real_zone(zone) )
     {
         ASSERT( zone < region_list.size() );
         region_list[zone] = 0;
     }

//    Get IC information from the piece
//    loop over all the local_zones of the piece
    for( unsigned int local_zone = 0; local_zone < piece.N_local_zones();
         ++local_zone )
    {
    //    get zone from the piece
        unsigned int zone = piece.zone(local_zone);
        ASSERT( zone >= mMesh.first_real_zone() );
        ASSERT( zone <= mMesh.last_real_zone() );
        ASSERT( zone < region_list.size() );

        ASSERT( piece.piece_Material().Material_consistent() );
        Material_list[region(zone)] = piece.piece_Material();
        ASSERT( Material_list[region(zone)].Material_consistent() );
    }

#ifdef USE_MPI
    replica_comm = MPI_COMM_WORLD;
#endif
}

//---------------------------------------------------------------------------//

//    Constructor that takes explicit inputs to construct all the IC data
//    The IC data comes in through a list of pieces, indexed by region

template<class Mesh_type>
Material_data_base<Mesh_type>::
Material_data_base( const Mesh_type& Mesh_in,
                    const vector<piece>& piece_list )
    : mMesh( Mesh_in ),
      N_regions( static_cast<int>(piece_list.size()) ),
      region_list( Mesh_in ),
      Material_list( static_cast<int>(piece_list.size()) ),
      mIsSetUp( false )
{
    ASSERT( &mMesh );
    ASSERT( N_regions > 0 );

//    Initialize region[zone], so that we can at least be sure that
//    every zone is identified with a valid region. Any zone not
//    listed in a piece gets dumped into region 0
    for( unsigned int zone = mMesh.first_real_zone();
         zone <= mMesh.last_real_zone();
         zone = mMesh.next_real_zone(zone) )
     {
         region_list[zone] = 0;
     }

//    loop over all parts, and get information info from each
    for( unsigned int piece_index = 0; piece_index < piece_list.size(); ++piece_index )
    {
    //    loop over all the local_zones of the piece
        for( unsigned int local_zone = 0;
             local_zone < piece_list[piece_index].N_local_zones();
             ++local_zone )
        {
        //    get zone from the piece
            unsigned int zone = piece_list[piece_index].zone(local_zone);
            ASSERT( zone >= mMesh.first_real_zone() );
            ASSERT( zone <= mMesh.last_real_zone() );
            ASSERT( zone < region_list.size() );

        //    region is the piece index
            int region = piece_index;

            region_list[zone] = region;

            Material_list[region] = piece_list[piece_index].piece_Material();
        }
    }


#ifdef USE_MPI
    replica_comm = MPI_COMM_WORLD;
#endif
}

//---------------------------------------------------------------------------//

//    calculate things needed for a timestep

template<class Mesh_type>
void Material_data_base<Mesh_type>::
setUp()
{
    mIsSetUp = true;
}

//---------------------------------------------------------------------------//

}    //    namespace IMC_namespace



