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

#ifndef __transmitting_BC_hh__
#define __transmitting_BC_hh__

//===========================================================================//
//
//    class transmitting_BC - This class provides the default behavior, 
//    which is to do nothing, i.e., transmitting, or open, boundary condition.
//    The IMC sets all boundaries to open, using this class, as the default
//
//===========================================================================//

#include <vector>
#include <cmath>

#include "Boundary_Condition.hh"

//    need to template on mesh, so we can get hold of mesh attributes
//    like face areas and zone volumes

namespace IMC_namespace
{

template <class Mesh_type, class Particle_type>
class transmitting_BC : public Boundary_Condition<Mesh_type, Particle_type>
{
  public:    
    typedef typename Mesh_type::mesh_face_ID_type mesh_face_ID_type;
    typedef typename Mesh_type::particle_face_ID_type particle_face_ID_type;
    typedef typename Mesh_type::particle_zone_ID_type particle_zone_ID_type;

    explicit transmitting_BC( const std::vector<mesh_face_ID_type>& mesh_faces );

    transmitting_BC();    //    used to make default BC in BC_list

//    function that alters particle in the appropriate way as it crosses boundary
    virtual void impose_BC( const particle_face_ID_type& face, 
                            Particle_type& crossing_particle,
                            particle_zone_ID_type& next_zone,
                            bool& enters_new__zone,
                            bool& exits_domain,
                            bool& exits_problem ) const;
 
  private:
//    make these private so they can't be used
    transmitting_BC( const transmitting_BC& );
    transmitting_BC& operator=( const transmitting_BC& );
};

}    //    namespace IMC_namespace

#endif
