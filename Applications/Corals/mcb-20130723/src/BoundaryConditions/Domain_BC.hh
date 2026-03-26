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

#ifndef __Domain_BC_hh__
#define __Domain_BC_hh__

//===========================================================================//
//
//    class Domain_BC - This class boundary condition  handles transfer of 
//    particles between different domains.
//
//===========================================================================//

#include <fstream>
#include <vector>
#include <cmath>

#include "Boundary_Condition.hh"
#include "Domain_exchange.hh"

//    need to template on mesh, so we can get hold of mesh attributes
//    like face areas and zone volumes

namespace IMC_namespace
{

template <class Mesh_type, class Particle_type>
class Domain_BC 
    : public Boundary_Condition<Mesh_type, Particle_type>
{
  public: 
    typedef typename Mesh_type::mesh_face_ID_type mesh_face_ID_type;
    typedef typename Mesh_type::particle_face_ID_type particle_face_ID_type;
    typedef typename Mesh_type::particle_zone_ID_type particle_zone_ID_type;
    typedef Domain_exchange<Mesh_type,Particle_type> Domain_exchange_type;
    
    Domain_BC( const std::vector<mesh_face_ID_type>& mesh_faces,
               const std::vector<Domain_exchange_type*>& DEsForThisBC,
               const Mesh_type& Mesh_in );
            
//    function that alters particle in the appropriate way as it crosses boundary
    virtual void impose_BC( const particle_face_ID_type& face, 
                            Particle_type& crossing_particle,
                            particle_zone_ID_type& next_zone,
                            bool& enters_new_zone,
                            bool& exits_domain,
                            bool& exits_problem ) const;
    
  private:
//    Mesh that this Domain_exchange object is on
    const Mesh_type& Mesh; 
    
//    Domain_exchange objects that this BC sends particles to
    std::vector<Domain_exchange_type*> DEsForThisBC;

//    number of different DE objects this BC sends particles to - is the same
//    as the size of DEsForThisBC
    unsigned int NDEs;

//    number of particles sent - used to round-robin the particles among the
//    DEs in DEsForThisBC. Mutable so that const function impose_BC cna
//    increment it
    mutable unsigned long long NParticlesSent;

//    make these private so they can't be used
    Domain_BC();
    Domain_BC( const Domain_BC& );
    Domain_BC& operator=( const Domain_BC& );
};

}    //    namespace IMC_namespace

#endif
