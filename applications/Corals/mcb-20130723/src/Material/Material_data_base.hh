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

#ifndef __Material_data_base_hh__
#define __Material_data_base_hh__

//===========================================================================//
//
//    class Material_data_base - holds material data needed by IMC class
//    and source functions.
//    This class is the link between the IMC package and the main code. In
//    the stand-alone version of the IMC code, it in effect is the rest of
//    the code, i.e., IMC tells it how much energy was added per zone, it
//    determines energy balance, etc.
//
//===========================================================================//


#include <list>
#include <cmath>

#ifdef USE_MPI
#include <mpi.h>
#endif

#include "ASSERT.hh"
#include "IMC_Material.hh"
#include "piece.hh"
#include "rng.hh"
#include "zcf.hh"
#include "Vector3d.hh"

namespace IMC_namespace
{

template<typename Mesh_type>
class Material_data_base
{
  public:
//    Constructor that takes explicit inputs to construct all the IC data
//    The IC data comes in through a list of pieces, indexed by region
    Material_data_base( const Mesh_type& Mesh_in,
                        const std::vector<piece>& piece_list );

//    Constructor for problems with only one piece
    Material_data_base( const Mesh_type& Mesh_in, const piece& piece_in );

//    access functions 
    unsigned int region( unsigned int zone ) const;
    unsigned int material_index( unsigned int zone ) const;

//    calculate things needed by for a timestep
    void setUp();
           
//    allows IMC to check that setUp has been called before it tries to use this object
    bool isSetUp() const { return mIsSetUp; }

//    needed for multigroup field mesh replicated runs
#ifdef USE_MPI
    const MPI_Comm &getDomainGroupCommunicator() const;
#endif

  private:
//    reference to the mesh we are running on
    const Mesh_type& mMesh;

//    regions are numbered 0 to N_regions-1
    unsigned int N_regions;
    zcf<Mesh_type,unsigned int> region_list;

    std::vector<Material> Material_list;    //    material properties in each zone

//    allows IMC to check that setUp has been called before it tries to use this object
    bool mIsSetUp;
  
//    needed for mesh rep runs
#ifdef USE_MPI
    MPI_Comm replica_comm;
#endif

//    don't want these called, so make them private
    Material_data_base();
    Material_data_base( const Material_data_base& );
    Material_data_base& operator=( const Material_data_base& );
};

//---------------------------------------------------------------------------//
//    definitions of inlined functions
//---------------------------------------------------------------------------//

template<class Mesh_type>
inline unsigned int Material_data_base<Mesh_type>::
region( unsigned int zone ) const
{
    ASSERT( zone < region_list.size() );

    return region_list[zone];
}

//---------------------------------------------------------------------------//

//    Used by MG_opacity to get the index into the material_opacities vector 
//    from matID.

template<class Mesh_type>
unsigned int Material_data_base<Mesh_type>::
material_index( unsigned int zone ) const
{
    return region( zone );
}

//---------------------------------------------------------------------------//

#ifdef USE_MPI

//    MPI communicator that encompasses all the procs running a replica of the
//    mesh. Since this mesh can only do mesh rep, that is all procs, so the
//    communicator is MPI_COMM_WORLD

template<class Mesh_type>
inline const MPI_Comm& 
Material_data_base<Mesh_type>::
getDomainGroupCommunicator() const
{
    return replica_comm;
}

#endif

//---------------------------------------------------------------------------//

}    //    namespace IMC_namespace

#endif  //  Material_data_base.hh

//---------------------------------------------------------------------------//
//                              end of Material_data_base.hh
//---------------------------------------------------------------------------//
