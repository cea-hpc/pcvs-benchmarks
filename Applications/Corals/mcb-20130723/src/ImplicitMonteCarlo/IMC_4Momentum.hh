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

#ifndef __IMC_4Momentum_hh__
#define __IMC_4Momentum_hh__

//===========================================================================//
//
// class IMC_4Momentum - store energy variables used by IMC
//
//===========================================================================//

#include <iostream>
#include <fstream>
#include <iomanip>
#include <list>
#include <utility>
#include <vector>
#include <string>
#include <cmath>


#include "Source_data_base.hh"
#include "photon.hh"

namespace IMC_namespace
{

template <typename mesh_types, typename mat_types>
class IMC_4Momentum
{
  public:
    typedef typename mesh_types::Mesh_type Mesh_type;
    typedef typename mesh_types::Vector3d Vector3d;
    typedef typename mesh_types::mesh_zone_ID_type mesh_zone_ID_type;
    typedef typename mesh_types::zcf_double zcf_double;
  //    typedef typename mesh_types::zcf_Vector3d zcf_Vector3d;
    typedef typename mesh_types::zcf_UINT64 zcf_UINT64;
    typedef typename mesh_types::zcf_SINT64 zcf_SINT64;
    typedef typename mesh_types::fcf_double fcf_double;
    typedef typename mesh_types::ZoneIterator ZoneIterator;
    typedef typename mesh_types::particle_zone_ID_type particle_zone_ID_type ;
    typedef typename mat_types::Material_data_base_type Material_data_base_type;
    typedef photon<particle_zone_ID_type, Vector3d> photon_type;

    IMC_4Momentum( const Mesh_type& Mesh_in,
                   const Material_data_base_type& Material_DB,
                   Source_data_base<mesh_types, mat_types>& Source_DB_in,
                   double RR_threshold );

//    access functions
    zcf_double& get_E_absorbed() { return E_absorbed; }
    double get_E_rad( const mesh_zone_ID_type& zone ) const;
    double get_E_out() const { return E_out; }
    double get_E_census() const { return E_census; }
    double get_E_census( const mesh_zone_ID_type& zone ) const;
    double get_E_total() const { return E_total; }
    double getRRThreshold() const { return RR_threshold; }
    
//    called in IMC ctor after initial photon population is made
    void set_initial_E_census( double E_initial_photons, 
                               const zcf_double& E_initial_photons_zone );
    
//    called at the begining of each time step
    void setUp( double dt );

    void RR_comb_census( std::list<photon_type>& census_photons,
                         unsigned long long N_cycle_photons,
                         unsigned long long& N_RR_split, 
                         unsigned long long& N_split_added,
                         unsigned long long& N_RRed_up, 
                         unsigned long long& N_RR_destroyed,
                         double tn, double dt );

//    Called by IMC::advance_photon
    void Russian_Roulette( photon_type& photon,
                           bool& terminated,
                           unsigned long long& N_RRed_up,
                           unsigned long long& N_RR_destroyed);
                            
    void tallyRadiationEnergy( double sigma_absorb, double d_path,
                               const photon_type& photon, double dt );
                                          
    void attenuate( double pathLength,
                    double sigmaAbsorb,
                    photon_type& photon,
                    const Vector3d& initialOmega );

    void HandleBC( double Energy_old,
                   const Vector3d& Omega_old,
                   const photon_type& photon);

    void HandleExit( const photon_type& photon);

    void do_census_tally( const photon_type& photon);

//    calculate info for energy balance
    void EPBookkeeping();
           
    double energyBalance();

    void diagnosticOutput();
    void threadedDiagnosticOutput();

    void RRDiagnostics( const mesh_zone_ID_type& zone,
                        unsigned long long& nCensusRRCreatedRef,
                        unsigned long long& nCensusRRDestroyedRef,
                        double& eCensusRRCreatedRef,
                        double& eCensusRRDestroyedRef,
                        unsigned long long& nSmallPhotonRRCreatedRef,
                        unsigned long long& nSmallPhotonRRDestroyedRef,
                        double& eSmallPhotonRRCreatedRef,
                        double& eSmallPhotonRRDestroyedRef,
                        double& eRRTotalRef ) const;

//    add up data across all replicated meshes in parallel
    void sum_over_replicated_meshes();

  private:

//    Used to convert energy from double to INT64 representation
    void set_INT64_conversion();

    unsigned long long E_to_EINT64( double E, const mesh_zone_ID_type& zone ) const;

    long long E_to_ESINT64( double E, const mesh_zone_ID_type& zone ) const;

    double EINT64_to_E( unsigned long long EINT64, const mesh_zone_ID_type& zone ) const;

    double ESINT64_to_E( long long ESINT64, const mesh_zone_ID_type& zone ) const;

//    called by RR_comb_census
    void pick_census_photons_to_keep( const std::list<photon_type>& census_photons, 
                                      zcf_UINT64& largest_state_census,
                                      zcf_double& E_largest_state_census,
                                      unsigned long long& N_census_no_RR,
                                      double& E_census_no_RR );
    void calculate_zonal_census_info( const mesh_zone_ID_type& zone,
                                      double E_census_desired,
                                      const zcf_double& E_largest_state_census,
                                      double& E_census_zone,
                                      unsigned long long& N_census_zone ) const;
    double get_E_census_desired( double tn, double dt,
                                 const zcf_UINT64& largest_state_census,
                                 const zcf_double& E_largest_state_census,
                                 unsigned long long N_cycle_photons,
                                 unsigned long long N_census_no_RR,
                                 double E_census_no_RR );
    void RR_comb_destroy( const photon_type& photon );
    void RR_comb_create( double delta_E, const photon_type& photon );
    void RR_renormalize_census_energy( std::list<photon_type>& census_photons,
                                       unsigned long long& N_RR_destroyed );

//    reference to the mesh we are running on
    const Mesh_type& Mesh;

//    material properties stored in data base
    const Material_data_base_type& Material_DB;

//    Object that creates source photons for the IMC
    Source_data_base<mesh_types, mat_types>& Source_DB;

//    If photon energy falls to below RR_threshold*energy when it comes out
//    of census, perform Russian_Roulette on it.
    double RR_threshold;

//    Cumulative total absorption
    double E_absorbed_total;

//    Energy balance variables used in checking energy balance.
//    Values are cumulative.
    double E_out;
//    cumulative energy created and destroyed by Russian Roulette processes
    double E_census_RR_created, E_census_RR_destroyed;
    double E_small_photon_RR_created, E_small_photon_RR_destroyed;

    zcf_double E_census_RR_created_zone, E_census_RR_destroyed_zone;
    zcf_double E_small_photon_RR_created_zone, E_small_photon_RR_destroyed_zone;
    zcf_UINT64 N_census_RR_created_zone, N_census_RR_destroyed_zone;
    zcf_UINT64 N_small_photon_RR_created_zone, N_small_photon_RR_destroyed_zone;
    zcf_UINT64 EINT64_after_census_RR, N_after_census_RR;
    zcf_double E_after_census_RR;

//    Photon energy variables.
//    these variables are updated each time step - they are not cummulative
//    energy of photons reaching census
    zcf_UINT64 E_census_in_zones_INT64;
    zcf_double E_census_in_zones;

    double E_source;  //  photon energy from sources in a time step
    double E_census;  //  energy in census in a time step
    double E_census_initial;  //  needed for energy balance
    
//    Energies summed over all domains
    double E_source_total, E_census_total, E_total;
//    path-length weighted average photon energy in a timestep
    zcf_UINT64 E_rad_INT64;
    zcf_double E_rad;

//    Matter energy variables.
//    Values of these variables are updated each time step - not cumulative.
    zcf_double E_absorbed;   //  energy absorbed by matter in
                                        //  material (units:E)
//    absorption tallyed into int array so order of addition doesn't matter
    zcf_SINT64 E_absorbed_INT64;  

//    scale doubles added to _E_absorbed_INT64 by this amount, picked to lose
//    as little precision as possible
    zcf_double scale_to_INT64;
    const double Two_to_64, Two_to_63;

//    for multiple copies of the domain running on different mpi processes
    bool domain_decomposed;
    unsigned int N_domains;
    unsigned int domainID;
    bool mesh_replicated;
    unsigned int N_replicas;
    unsigned int replicaID;
    unsigned int nProcs;
    unsigned int procID;

//     for multiple IMC instances in OpenMP environment
    bool mIsOpenMPCoarseThreaded;
    unsigned int numOmpThreads;
    unsigned int ompThreadID;

#ifdef USE_OPENMP
    std::vector<double>& mThreadedDiagnosticData();
    std::vector<unsigned long long >& mThreadedUllDiagnosticData();
#endif
};

//---------------------------------------------------------------------------//
//    definitions of inlined functions
//---------------------------------------------------------------------------//

template <typename mesh_types, typename mat_types>
inline double 
IMC_4Momentum<mesh_types, mat_types>::
get_E_rad( const mesh_zone_ID_type& zone ) const
{
    return E_rad[zone];
}

//---------------------------------------------------------------------------//

template <typename mesh_types, typename mat_types>
inline double IMC_4Momentum<mesh_types, mat_types>::
get_E_census( const mesh_zone_ID_type& zone ) const
{
    return E_census_in_zones[zone];
}

//---------------------------------------------------------------------------//

//    Used to convert energy from double to INT64 representation

template <typename mesh_types, typename mat_types>
inline unsigned long long IMC_4Momentum<mesh_types, mat_types>::
E_to_EINT64( double E, const mesh_zone_ID_type& zone ) const
{
    EXPENSIVE_ASSERT( E < E_total );
    EXPENSIVE_ASSERT( static_cast<unsigned long long>(E*scale_to_INT64[zone] + 0.5) <= Two_to_64 );

//    add .5 so we round up instead of shaving off decimal part of energy
    return static_cast<unsigned long long>( E*scale_to_INT64[zone] + 0.5 );
}

//---------------------------------------------------------------------------//

//    Used to convert energy from double to INT64 representation

template <typename mesh_types, typename mat_types>
inline long long IMC_4Momentum<mesh_types, mat_types>::
E_to_ESINT64( double E, const mesh_zone_ID_type& zone ) const
{
    EXPENSIVE_ASSERT( E < E_total );
    EXPENSIVE_ASSERT( static_cast<long long>(E*scale_to_INT64[zone] + 0.5) <= Two_to_64 );

//    add .5 so we round up instead of shaving off decimal part of energy
    return static_cast<long long>( E*scale_to_INT64[zone] + 0.5 );
}

//---------------------------------------------------------------------------//

template <typename mesh_types, typename mat_types>
inline double IMC_4Momentum<mesh_types, mat_types>::
EINT64_to_E( unsigned long long EINT64, const mesh_zone_ID_type& zone ) const
{
    EXPENSIVE_ASSERT( static_cast<double>(EINT64) < Two_to_64 );
//    Slack of 1.5 because E_total doesn't include RRing
    EXPENSIVE_ASSERT( static_cast<double>(EINT64)/scale_to_INT64[zone] < 1.5*E_total ||
            EINT64 == 0 );

    return static_cast<double>(EINT64)/scale_to_INT64[zone];
}

//---------------------------------------------------------------------------//

template <typename mesh_types, typename mat_types>
inline double IMC_4Momentum<mesh_types, mat_types>::
ESINT64_to_E( long long ESINT64, const mesh_zone_ID_type& zone ) const
{
    EXPENSIVE_ASSERT( static_cast<double>(ESINT64) < Two_to_64 );
//    Slack of 1.5 because E_total doesn't include RRing
    EXPENSIVE_ASSERT( static_cast<double>(ESINT64)/scale_to_INT64[zone] < 1.5*E_total ||
            ESINT64 == 0 );

    return static_cast<double>(ESINT64)/scale_to_INT64[zone];
}

//---------------------------------------------------------------------------//

template <typename mesh_types, typename mat_types>
inline void IMC_4Momentum<mesh_types, mat_types>::
RR_comb_destroy( const photon_type& photon)
{
    E_census_RR_destroyed += photon.Energy;
    E_census_RR_destroyed_zone[photon.zone] += photon.Energy;
    ++N_census_RR_destroyed_zone[photon.zone];
}

//---------------------------------------------------------------------------//

template <typename mesh_types, typename mat_types>
inline void IMC_4Momentum<mesh_types, mat_types>::
RR_comb_create( double delta_E, const photon_type& photon)
{
    E_census_RR_created += delta_E;
    E_census_RR_created_zone[photon.zone] += delta_E;
    ++N_census_RR_created_zone[photon.zone];
}

//---------------------------------------------------------------------------//

template <typename mesh_types, typename mat_types>
inline void IMC_4Momentum<mesh_types, mat_types>::
HandleBC( double Energy_old, const Vector3d& Omega_old, const photon_type& photon)
{ }

//---------------------------------------------------------------------------//

template <typename mesh_types, typename mat_types>
inline void IMC_4Momentum<mesh_types, mat_types>::
HandleExit( const photon_type& photon)
{
    E_out += photon.Energy;
}

//---------------------------------------------------------------------------//

template <typename mesh_types, typename mat_types>
inline void IMC_4Momentum<mesh_types, mat_types>::
do_census_tally( const photon_type& photon )
{
    unsigned long long E_INT64 = E_to_EINT64( photon.Energy, photon.zone );

//    make sure small photon counts for at least a little energy
    if( photon.Energy > 0.0 && E_INT64 == 0 )
        E_INT64 = 1;

    EXPENSIVE_ASSERT( E_census_in_zones_INT64[photon.zone]  + E_INT64 < Two_to_64 );
    E_census_in_zones_INT64[photon.zone] += E_INT64;
}

//---------------------------------------------------------------------------//

}  // namespace IMC_namespace

#endif                          // __IMC_4Momentum_hh__

