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

#ifndef __BC_list_hh__
#define __BC_list_hh__

#include <iostream>
#include <iomanip>
#include <vector>
#include <cmath>

#include "Boundary_Condition.hh"
#include "transmitting_BC.hh"

//    This class returns a pointer to a BC given an int face member
//    For meshes with face_type class not int, this class has 
//    to be template specialized.

namespace IMC_namespace
{

template<typename Mesh_type, typename Particle_type, 
         typename Field_BCPtr_type>
class BC_list
{
  public:
    typedef typename Mesh_type::particle_face_ID_type particle_face_ID_type;
    typedef typename Mesh_type::mesh_face_ID_type mesh_face_ID_type;

    explicit BC_list( const Mesh_type& Mesh );

    Boundary_Condition<Mesh_type,Particle_type>*
    operator() ( const particle_face_ID_type& face );

    void set_BC( const mesh_face_ID_type& face,
                 Boundary_Condition<Mesh_type,Particle_type>* BC_in );

  protected:
//    indexed by face
    Field_BCPtr_type _BCs;

//    default Boundary_Condition object: transmitting BC's
//    all of _BCs points to this when it is created
//    IMC overrides this if user has specified BC's other than transmitting
    transmitting_BC<Mesh_type,Particle_type> _BC_default;

//    make these private so they can't be used
    BC_list();
    BC_list( const BC_list& );
    BC_list& operator=( const BC_list& );
};

//---------------------------------------------------------------------------//
//    definitions of inlined functions
//---------------------------------------------------------------------------//

template<typename Mesh_type, typename Particle_type, 
         typename Field_BCPtr_type>
inline BC_list<Mesh_type,Particle_type,Field_BCPtr_type>::
BC_list( const Mesh_type& Mesh_in )
    : _BCs(),
      _BC_default()
{
//    now assign all members to the default transmitting BCs
//     _BCs = &_BC_default;
    
    for(typename Mesh_type::particle_face_ID_type faceID= Mesh_in.first_real_face();
        faceID != Mesh_in.last_real_face(); faceID = Mesh_in.next_real_face(faceID) )
    {
        _BCs[faceID] = &_BC_default;
    }
    
}

//---------------------------------------------------------------------------//

template<typename Mesh_type, typename Particle_type, 
         typename Field_BCPtr_type>
inline Boundary_Condition<Mesh_type,Particle_type>* 
BC_list<Mesh_type,Particle_type,Field_BCPtr_type>::
operator() ( const particle_face_ID_type& face )
{
    return _BCs[face];    
}

//---------------------------------------------------------------------------//

template<typename Mesh_type, typename Particle_type, 
         typename Field_BCPtr_type>
inline void 
BC_list<Mesh_type,Particle_type,Field_BCPtr_type>::
set_BC( const mesh_face_ID_type& face,
        Boundary_Condition<Mesh_type,Particle_type>* BC_in )
{
    _BCs[face] = BC_in;
}

}    //    namespace IMC_namespace

#endif    //    of __BC_list_hh__
