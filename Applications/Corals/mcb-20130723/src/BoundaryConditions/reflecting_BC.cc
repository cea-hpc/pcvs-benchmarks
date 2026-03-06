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

#include "reflecting_BC.hh"
#include "ASSERT.hh"

namespace IMC_namespace
{

//    constructor

template<class Mesh_type, class Particle_type>
reflecting_BC<Mesh_type, Particle_type>::
reflecting_BC( const Mesh_type& Mesh_in,
               const std::vector<mesh_face_ID_type>& mesh_faces )
    : Boundary_Condition<Mesh_type, Particle_type>( mesh_faces ),
      Mesh( Mesh_in )
{

}

//---------------------------------------------------------------------------//

//    Reflect the particle as it goes through the face.
//    Sign of normal doesn't matter: since the 2 terms involving
//    it are multiplied together, sign changes cancel.

template<class Mesh_type, class Particle_type>
void reflecting_BC<Mesh_type, Particle_type>::
impose_BC( const particle_face_ID_type& face,
           Particle_type& particle,
           particle_zone_ID_type& next_zone,
           bool& enters_new_zone,
           bool& exits_domain, 
           bool& exits_problem ) const
{
    ASSERT( std::fabs(particle.Omega.magnitude() - 1.) < 1.e-6 );

//    Since particle is reflected, it does not enter new zone, and particle
//    does not exit domain or problem. So set flags to false, and set
//    next_zone == the current zone
    enters_new_zone = false;
    exits_domain = false;
    exits_problem = false;

    next_zone = particle.zone;

    typedef typename Particle_type::Vector_type Vector3d;
    Vector3d n = Mesh.face_normal( face, particle.X );

    particle.Omega -= 2.0*particle.Omega.dot(n)*n;

    ASSERT( std::fabs(particle.Omega.magnitude() - 1.) < 1.e-6 );
}

//---------------------------------------------------------------------------//

}    //    namespace IMC_namespace
