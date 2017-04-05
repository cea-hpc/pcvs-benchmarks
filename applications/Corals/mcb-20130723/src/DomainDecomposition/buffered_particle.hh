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
//    class buffered_particle - adds a variable used to identify the zone on
//    another domain to the particle class it is templated on.

//    When a particle moves to another domain, it needs to store some
//    information so that the data member that stores the particles zone can
//    be set to the correct value when it gets there. This data member is of
//    type MeshType::particle_zone_ID_type. If the zone is described by an
//    int (i.e., particle_zone_ID_type is an int), we could just change that
//    int value before sending the particle. But for meshes which use
//    pointers or references, this won't work. So this class inherits from
//    Particle_type and ands a data member of type newDomainZoneIDType. This
//    is a template parameter and defalts to int. The value of this variable
//    is used by the mesh on the receiving domain to set the
//    particle_zone_ID_type data member of the particle to the correct value.
//    The mesh function setNewDomainZoneInfo sets its value, and
//    the mesh function correctNewDomainZoneInfo uses it to change the
//    particles particle_zone_ID_type data member.
//
//===========================================================================//

#ifndef __buffered_particle_hh__
#define __buffered_particle_hh__

namespace IMC_namespace
{

template <typename Particle_type, typename newDomainZoneIDType>
class buffered_particle : public Particle_type
{
  public: 
//    constructors
    buffered_particle();
    buffered_particle( Particle_type& particle,
                       const newDomainZoneIDType& newDomainZoneID );

//    data member that holds info on the zone particle will be in when it crosses to
//    a new domain
    newDomainZoneIDType newDomainZoneInfo;

  private:
};

//---------------------------------------------------------------------------//
//    definitions of inlined functions
//---------------------------------------------------------------------------//

template <typename Particle_type, typename newDomainZoneIDType>
buffered_particle<Particle_type, newDomainZoneIDType>::
buffered_particle( )
    : Particle_type()
{}

//---------------------------------------------------------------------------//

template <typename Particle_type, typename newDomainZoneIDType>
buffered_particle<Particle_type, newDomainZoneIDType>::
buffered_particle( Particle_type& particle,
                   const newDomainZoneIDType& newDomainZoneID )
   : Particle_type( particle ), 
     newDomainZoneInfo( newDomainZoneID )
{}

//---------------------------------------------------------------------------//

}    //    namespace IMC_namespace

#endif    //    __buffered_particle_hh__
