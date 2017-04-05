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

#ifndef __Boundary_Condition_hh__
#define __Boundary_Condition_hh__

//===========================================================================//
//
//    class Boundary_Condition - changes a particle's attributes (usually the
//    direction cosines) when it crosses a face. This class is the base class
//    for a family of boundary conditions, which work on internal boundaries
//    also.
//    The IMC sets all boundaries to open as the default
//
//===========================================================================//

#include <vector>
#include <cmath>

//    Need to template on mesh, so we can get hold of mesh attributes
//    like face areas and zone volumes.
//    Need to template on Particle_type so that we can try to use this file
//    for both IMC and PMC

namespace IMC_namespace
{

template <class Mesh_type, class Particle_type>
class Boundary_Condition
{
  public:
    typedef typename Mesh_type::mesh_face_ID_type mesh_face_ID_type;
    typedef typename Mesh_type::particle_face_ID_type particle_face_ID_type;
    typedef typename Mesh_type::particle_zone_ID_type particle_zone_ID_type;

//    constructors
    explicit Boundary_Condition( const std::vector<mesh_face_ID_type>& mesh_faces_in );
    Boundary_Condition();

    virtual ~Boundary_Condition(){}

//    function that alters particle in the appropriate way as it crosses boundary
    virtual void impose_BC( const particle_face_ID_type& face, 
                            Particle_type& crossing_particle,
                            particle_zone_ID_type& next_zone,
                            bool& enters_new_zone,
                            bool& exits_domain,
                            bool& exits_problem ) const = 0;

//    function used by the IMC constructor to discard memory in mesh_faces
    void erase_mesh_faces();

//    IMC uses these to iterate over the face_list
    typedef typename std::vector<mesh_face_ID_type>::const_iterator face_iterator;
    face_iterator face_begin() const;
    face_iterator face_end() const;
        
  private:  
//    std::vector of faces this BC is to be applied to. Only used in IMC
//    constructor.  After that use, it's memory is discarded by the IMC
//    constructor.
    std::vector<mesh_face_ID_type> mesh_faces;

//    make these private so they can't be used
    Boundary_Condition( const Boundary_Condition& );
    Boundary_Condition& operator=( const Boundary_Condition& );
};

//---------------------------------------------------------------------------//
//    definitions of inlined functions
//---------------------------------------------------------------------------//

template <class Mesh_type, class Particle_type>
inline Boundary_Condition<Mesh_type, Particle_type>::
Boundary_Condition( const std::vector<mesh_face_ID_type>& mesh_faces_in )
    : mesh_faces( mesh_faces_in )
{ }

//---------------------------------------------------------------------------//

//    This constructor is used by BC_list when it creates a transmitting_BC
//    object using that classes default constructor.

template <class Mesh_type, class Particle_type>
inline Boundary_Condition<Mesh_type, Particle_type>::
Boundary_Condition()
{ }

//---------------------------------------------------------------------------//

//    function used by the IMC constructor to discard memory in face_list

template <class Mesh_type, class Particle_type>
inline void Boundary_Condition<Mesh_type, Particle_type>::
erase_mesh_faces() 
{
    mesh_faces.clear(); 
}

//---------------------------------------------------------------------------//

//    IMC uses these to iterate over the face_list

template <class Mesh_type, class Particle_type>
inline typename Boundary_Condition<Mesh_type, Particle_type>::face_iterator 
Boundary_Condition<Mesh_type, Particle_type>::face_begin() const 
{
    return mesh_faces.begin(); 
}

//---------------------------------------------------------------------------//

template <class Mesh_type, class Particle_type>
inline typename Boundary_Condition<Mesh_type, Particle_type>::face_iterator 
Boundary_Condition<Mesh_type, Particle_type>::face_end() const 
{
    return mesh_faces.end();
}

//---------------------------------------------------------------------------//

}    //    namespace IMC_namespace

#endif




