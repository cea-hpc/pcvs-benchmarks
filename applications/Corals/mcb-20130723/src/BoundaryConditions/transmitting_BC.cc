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

#include "transmitting_BC.hh"

namespace IMC_namespace
{

//---------------------------------------------------------------------------//

template<class Mesh_type, class Particle_type>
transmitting_BC<Mesh_type, Particle_type>::
transmitting_BC( const std::vector<mesh_face_ID_type>& mesh_faces )
    : Boundary_Condition<Mesh_type, Particle_type>( mesh_faces )
{ }

//---------------------------------------------------------------------------//

template<class Mesh_type, class Particle_type>
transmitting_BC<Mesh_type, Particle_type>::
transmitting_BC()
    : Boundary_Condition<Mesh_type, Particle_type>()
{ }

//---------------------------------------------------------------------------//

//    the behavior of transmitting boundary condition is to do nothing - let
//    the particle go through without change.

template<class Mesh_type, class Particle_type>
void transmitting_BC<Mesh_type, Particle_type>::
impose_BC( const particle_face_ID_type& face, 
           Particle_type& crossing_particle,
           particle_zone_ID_type& next_zone,
           bool& enters_new_zone,
           bool& exits_domain, 
           bool& exits_problem ) const
{
//    if photon has reached a transmitting BC, it enters a new zone and does
//    not exit the domain. (The photon would have hit a domainBC if it was
//    going to do that.) The value of exits_problem was set by a call to
//    Mesh.dBoundary, and so that is not set here - it's value is unchanged

    enters_new_zone = true;
    exits_domain = false;
}

//---------------------------------------------------------------------------//

}    //    namespace IMC_namespace
