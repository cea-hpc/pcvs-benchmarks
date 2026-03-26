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

#ifndef __reflecting_BC_hh__
#define __reflecting_BC_hh__

//===========================================================================//
//
//    class reflecting_BC - changes a particle's direction cosines
//    when it crosses a face. This class works on internal boundaries
//    also.) This class provides reflecting boundary conditions,
//    i.e., closed boundary conditions.
//    The IMC sets all boundaries to open as the default. std::vectors of this class
//    are sent to the IMC constructor to set reflecting BCs.
//
//===========================================================================//

#include <cmath>

#include "Boundary_Condition.hh"

//    need to template on mesh, so we can get hold of mesh attributes
//    like face areas and zone volumes

namespace IMC_namespace
{

template <class Mesh_type, class Particle_type>
class reflecting_BC : public Boundary_Condition<Mesh_type, Particle_type>
{
  public:
    typedef typename Mesh_type::mesh_face_ID_type mesh_face_ID_type;
    typedef typename Mesh_type::particle_face_ID_type particle_face_ID_type;
    typedef typename Mesh_type::particle_zone_ID_type particle_zone_ID_type;

//    reflecting BC object needs a copy of the Mesh
    reflecting_BC( const Mesh_type& Mesh_in,
                   const std::vector<mesh_face_ID_type>& mesh_faces );

//    function that reflects particle at the face as it crosses
    void impose_BC( const particle_face_ID_type& face, 
                    Particle_type& crossing_particle,
                    particle_zone_ID_type& next_zone,
                    bool& enters_new_zone,
                    bool& exits_domain,        
                    bool& exits_problem ) const;

    ~reflecting_BC() { }
    
  private:
//    reference to Mesh that this BC applies to
    const Mesh_type& Mesh;

//    don't want these called, so make them private
    reflecting_BC();
    reflecting_BC( const reflecting_BC& );
    reflecting_BC& operator=( const reflecting_BC& );
};

}    //    namespace IMC_namespace

#endif
