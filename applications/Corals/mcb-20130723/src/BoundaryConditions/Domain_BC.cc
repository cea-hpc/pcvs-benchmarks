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

#include "Domain_BC.hh"

using namespace std;

namespace IMC_namespace
{

//---------------------------------------------------------------------------//

//    constructor

template<class Mesh_type, class Particle_type>
Domain_BC<Mesh_type,Particle_type>::
Domain_BC( const vector<mesh_face_ID_type>& mesh_faces,
           const std::vector<Domain_exchange_type*>& DEsForThisBC_in,
           const Mesh_type& Mesh_in )
    : Boundary_Condition<Mesh_type,Particle_type>( mesh_faces ),
      Mesh( Mesh_in ),
      DEsForThisBC( DEsForThisBC_in ),
      NDEs( static_cast<unsigned int>(DEsForThisBC_in.size()) ),
      NParticlesSent( 0 )
{ }

//---------------------------------------------------------------------------//

//    buffer a particle in one of the the Domain_exchange objects

template<class Mesh_type, class Particle_type>
void Domain_BC<Mesh_type,Particle_type>::
impose_BC( const particle_face_ID_type& face, 
           Particle_type& crossing_particle,
           particle_zone_ID_type& next_zone,
           bool& enters_new_zone,
           bool& exits_domain, 
           bool& exits_problem ) const
{
//    particle is going to a new proc, so it's leaving the IMC object that
//    this BC object is on
    exits_problem = false;
    exits_domain = true;
    enters_new_zone = false;

    NParticlesSent += 1;

//   decide which DE (which proccessor simulating the neighboring domain) to
//   send particle to - round-robin them
    unsigned int DEindex = static_cast<unsigned int>(NParticlesSent) % NDEs;
    ASSERT( DEindex < DEsForThisBC.size() );

//    add this particle to the list of new particles which will
//    be sent to the appropriate Domain    
    DEsForThisBC[DEindex]->buffer_particle( face,
                                            next_zone,
                                            crossing_particle );
}

//---------------------------------------------------------------------------//

}    //    namespace IMC_namespace
