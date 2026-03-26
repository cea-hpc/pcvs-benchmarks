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

#ifndef __isotropic_source_hh__
#define __isotropic_source_hh__

#include <vector>
#include <string>

#include "photon_source.hh"
#include "Opacity_data_base.hh"
#include "rng.hh"

namespace IMC_namespace
{

template<typename mesh_types, typename mat_types>
class isotropic_source : public photon_source<mesh_types, mat_types>
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
    
//    source is sourceStrength * Sx(x) * sy(y) * T(t) with
//    Sx(x) = (x-xRoot)**3 * (x+xRoot)**3
//    Sy(y) = (y-yRoot)**3 * (y+yRoot)**3
//    T(t) = 0.5 * [1.0 + sin( omega * t + phi )]
//    This holds for x < xRoot, y < yRoot

    isotropic_source( double sourceStrength,
                      double xRoot, double yRoot,
                      double omega, double phi,
                      bool perfectlyLoadBalanced,
                      const std::vector<mesh_zone_ID_type>& mesh_zones,
                      const Mesh_type& Mesh_in,
                      const Material_data_base_type& Material_DB_in,
                      const Opacity_data_base<mesh_types, mat_types>& Opacity_DB_in );
                                            
//    energy emitted by each particular element - so we can decide how many
//    photons to cause to be radiated from an element
    double element_energy_emitted( unsigned int element, 
                                   double tn, double dt ) const;
                                       
//    total energy radiated by all the elements to which this source applies
//    needed to figure out the energy/photon for new photons 
//    at each time step
    double total_energy_emitted( double tn, double dt ) const;

//    energy emitted by each particular element - so we can decide how many
//    photons to cause to be radiated from an element
    void get_source_energy( double tn, double dt,
                            double& E_photons,
                            zcf_double& E_emitted ) const;

    void generate_source_photon( unsigned int element,
                                 photon_type& new_source_photon,
                                 double photon_energy,
                                 double tn,
                                 double dt ) const;
                        
//    functions allowing looping over elements in the IMC source routines
    unsigned int elementBegin() const { return 0; }
    unsigned int elementEnd() const { return number_of_source_zones; }
    
//    what zone a photon will be in when created in element
    mesh_zone_ID_type zone( unsigned int element ) const;
                                        
//    called at begining of time step; for this class, no setUp required    
    void setUp( double t, double dt ) { };
    
//    output for debugging
    void diagnostic_output() const;
        
//    output for debugging
    void threaded_diagnostic_output();
    
#ifdef USE_OPENMP
    void createThreadDiagnosticBuffers()
        {
            std::vector<unsigned long long>& theULLBuffer = mULLThreadedDiagnosticData();
            std::vector<double>& theDoubleBuffer = mDoubleThreadedDiagnosticData();
        }
#endif

  private:
//     energy emitted from zone during a time step
    double loadImbalancedSourceIntegral( unsigned int zone, 
                                         double t, double dt ) const;

    double loadBalancedSourceIntegral( unsigned int zone, 
                                       double t, double dt ) const;

    double sourceIntegral( unsigned int zone, 
                           double t, double dt ) const;

//    the rate at which energy is emitted
    const double sourceStrength;

    const double xRoot, yRoot;
    const double omega, phi;
    
//    remove spatial dependence from source to eliminate load imbalance
    const bool mPerfectlyLoadBalanced;

//    number of zones that this particular source applies to
    unsigned int number_of_source_zones;

//    vector mesh_zone_IDs this source applies to
    std::vector<mesh_zone_ID_type> source_zones;
    
    const Mesh_type& Mesh;
    
    const Material_data_base_type& Material_DB;

//    base class reference to object providing opacity information;
    const Opacity_data_base<mesh_types, mat_types>& Opacity_DB;          
    
//    used in diagnostic_output
    mutable unsigned int N_photons_created;
    mutable double E_in_physical, E_in_photons;
    const std::string source_name;
        
#ifdef USE_OPENMP
    std::vector<unsigned long long>& mULLThreadedDiagnosticData();
    std::vector<double>& mDoubleThreadedDiagnosticData();
#endif

//    don't want these called, so make them private
    isotropic_source();
    isotropic_source( const isotropic_source& );
    isotropic_source& operator=( const isotropic_source& );
};

}    //    namespace IMC_namespace

#endif


