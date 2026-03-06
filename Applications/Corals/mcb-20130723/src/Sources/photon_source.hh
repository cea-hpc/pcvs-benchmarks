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
//    class photon_source - calculates the chracteristics of photons injected
//    into an IMC simulation at each time step
//
//===========================================================================//

#ifndef __photon_source_hh__
#define __photon_source_hh__

#include <iostream>
#include <cmath>

#include "photon.hh"
#include "rng.hh"

namespace IMC_namespace
{

template <typename mesh_types, typename mat_types>
class photon_source
{
  public:
    typedef typename mesh_types::Mesh_type Mesh_type;
    typedef typename mesh_types::mesh_face_ID_type mesh_face_ID_type;
    typedef typename mesh_types::mesh_zone_ID_type mesh_zone_ID_type;
    typedef typename mesh_types::particle_zone_ID_type particle_zone_ID_type;
    typedef typename mesh_types::particle_face_ID_type particle_face_ID_type;
    typedef typename mesh_types::Vector3d Vector3d;
    typedef typename mesh_types::zcf_double zcf_double;
    typedef typename mat_types::Material_data_base_type Material_data_base_type;
    typedef photon<particle_zone_ID_type, Vector3d> photon_type;

    explicit photon_source() { }

    virtual ~photon_source() { }
        
//    energy emitted by each particular element - so we can decide how many
//    photons to cause to be emitted from an element
    virtual double element_energy_emitted( unsigned int element, 
                                           double tn, double dt ) const = 0;
    
//    total energy radiated by all the elements to which this source applies
//    needed to figure out the energy/photon for new photons 
//    at each time step
    virtual double total_energy_emitted( double tn, double dt ) const = 0;

//    function used to account for energy of all elements of a source by
//    changing either E_photons or E_emitted
    virtual void get_source_energy( double tn, double dt, 
                                    double& E_photons,
                                    zcf_double& E_emitted ) const = 0;

//    makes a source photon with the given energy with the correct properties
    virtual void generate_source_photon( unsigned int element,
                                         photon_type& new_source_photon,
                                         double photon_energy,
                                         double tn,
                                         double dt ) const = 0;
                                                                                   
//    functions allowing loops over elements
    virtual unsigned int elementBegin() const = 0;
    virtual unsigned int elementEnd() const = 0;

//    what zone a photon will be in when created in element
    virtual mesh_zone_ID_type zone( unsigned int element ) const = 0;
    
//    called by Source_data_base at the beginning of each time step
    virtual void setUp( double t, double dt ) = 0;
    
//    output for debugging
    virtual void diagnostic_output() const = 0;
    
//    output for debugging
    virtual void threaded_diagnostic_output() = 0;

  private:
//    make these private so they can't be used
    photon_source( const photon_source& );
    photon_source& operator=( const photon_source& );
};

}    //    namespace IMC_namespace

#endif



