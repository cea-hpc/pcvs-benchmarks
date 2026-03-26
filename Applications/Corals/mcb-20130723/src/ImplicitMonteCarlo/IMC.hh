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

#ifndef __IMC_hh__
#define __IMC_hh__

//===========================================================================//
//
// class IMC
//
//===========================================================================//

#include <iostream>
#include <fstream>
#include <iomanip>
#include <list>
#include <utility>
#include <vector>
#include <deque>
#include <string>
#include <cmath>

#include "BC_list.hh"
#include "Source_data_base.hh"
#include "Opacity_data_base.hh"
#include "IMC_4Momentum.hh"
#include "photon.hh"
#include "IMC_types.hh"
#include "MCAnswer.hh"

#ifdef USE_MPI
#include "mpi.h"

#endif

namespace IMC_namespace
{

const double minDistanceToCensus = -1.0e-10;

template<typename mesh_types, typename mat_types>
class IMC
{
  public:
    typedef typename mesh_types::Mesh_type Mesh_type;
    typedef typename mesh_types::particle_zone_ID_type particle_zone_ID_type;
    typedef typename mesh_types::particle_face_ID_type particle_face_ID_type;
    typedef typename mesh_types::Vector3d Vector3d;
    typedef typename mesh_types::mesh_zone_ID_type mesh_zone_ID_type;
    typedef typename mesh_types::zcf_double zcf_double;
    typedef typename mesh_types::zcf_Vector3d zcf_Vector3d;
    typedef typename mesh_types::zcf_UINT64 zcf_UINT64;
    typedef typename mesh_types::mesh_face_ID_type mesh_face_ID_type;
    typedef typename mesh_types::fcf_double fcf_double;
    typedef typename mesh_types::ZoneIterator ZoneIterator;
    typedef typename mat_types::Material_data_base_type Material_data_base_type;
    typedef photon<particle_zone_ID_type, Vector3d> photon_type;
    typedef typename IMC_types<mesh_types,mat_types>::fcf_BCPtr_type fcf_BCPtr_type;

    IMC( const Mesh_type& Mesh_in,
         const Material_data_base_type& Material_DB_in,
         const Opacity_data_base<mesh_types, mat_types>& Opacity_DB_in,
         const std::vector<Boundary_Condition<Mesh_type,photon_type>*>& user_BC_list,
         Source_data_base<mesh_types, mat_types>& Source_DB_in,
         MCAnswer& analyticAnswer,
         unsigned long long N_cycle_photons_in,
         double initial_time = 0.0,
         std::string mFileNameHeader = "IMC",
         double RR_threshold = 0.01,
         unsigned int message_check_period_in = 16386 );

//    add initial census photons and energy accounting in ctor
    void set_initial_census_photons( double dt );
    
    void advance( double dt );

    void temporalOutput( double dt );

    void spatialOutput();

    void diagnosticOutput();

    void threadedDiagnosticOutput();

    double computeMaxDeviationFromAnalytic() const;

    void RRDiagnostics( const mesh_zone_ID_type& zone,
                        unsigned long long& nSourceRRCreatedRef,
                        unsigned long long& nSourceRRDestroyedRef,
                        double& eSourceCreatedRef,
                        double& eSourceDestroyedRef,
                        unsigned long long& nCensusRRCreatedRef,
                        unsigned long long& nCensusRRDestroyedRef,
                        double& eCensusRRCreatedRef,
                        double& eCensusRRDestroyedRef,
                        unsigned long long& nSmallPhotonRRCreatedRef,
                        unsigned long long& nSmallPhotonRRDestroyedRef,
                        double& eSmallPhotonRRCreatedRef,
                        double& eSmallPhotonRRDestroyedRef,
                        double& eRRTotalRef ) const;

    double energyBalance();
    long long photonNumberBalance() const;

//    access functions for radiation properties
    double eOut() const { return m4Momentum.get_E_out(); }
    double Tnp1() const { return tnp1; }
    double eRad( const mesh_zone_ID_type& zone ) const;
    double eCensus( const mesh_zone_ID_type& zone ) const;

    unsigned long long nPaths( const mesh_zone_ID_type& zone ) const;
    
//    access functions provided for use by Material_data_base class
    zcf_double& get_E_absorbed() { return m4Momentum.get_E_absorbed(); }

//    access function for cumulative energy that passed through face
    double faceE( const mesh_face_ID_type& face ) const;
//    energy that passed through face in current timestep
    double timestepFaceE( const mesh_face_ID_type& face ) const;

//    to change number of photons run in a cycle
    unsigned long long get_N_cycle_photons() const { return N_cycle_photons; }
    void set_N_cycle_photons( unsigned long long N_cycle_photons );

//    to make new files the first time output functions are called
    bool create_temporal_output_file, create_spatial_output_file;

  private:

    double distanceToCollision( double sigmaScat,
                                double rand ) const;

    void setOpacities( photon_type& photon,
                       double& sigmaAbsorb, double& sigmaScat );

    void processCollision( photon_type& photon,
                           double& sigmaScat, double& sigmaAbsorb );

//    stuff to be done each time step before we advance photons
    void setUp( double dt );

    void advancePhotonList( std::list<photon_type>& photon_list,
                            double dt );

    //! Used for the domain decomposed version
    void advancePhotonListDD( std::list<photon_type>& photon_list,
                              double dt );

#ifdef USE_MPI
    //! Perform a blocking MPI_Reduce to verify that all processors have finished.
    //! Only the root processor gets the global count.
    void verify_all_done( unsigned long long local_created,
                          unsigned long long local_completed,
                          unsigned long long& global_created,
                          unsigned long long& global_completed,
                          unsigned int root,
                          MPI_Comm comm ) const;

#endif

    //! Used for the mesh replicated and serial versions of the code.
    void advancePhotonListSimple( std::list<photon_type>& photon_list,
                                     double dt );

    void advancePhoton( photon_type& current_photon,
                        bool& exitsDomain,
                        bool& exitsProblem,
                        bool& terminated,
                        double dt );

    void advanceBoundary( photon_type& photon, 
                          double distanceToBndry,
                          particle_face_ID_type& crossingFace,
                          particle_zone_ID_type& nextZone,
                          bool& exitsDomain,
                          bool& exitsProblem,
                          bool& terminated, double dt,
                          double& sigmaAbsorb, double& sigmaScat );

    void advanceCollision( photon_type& photon, 
                           double distToCollision,
                           bool& terminated,
                           double dt,
                           double& sigmaAbsorb, double& sigmaScat );

    void fixUpCensusZone( photon_type& photon, double distToCensus,
                          const particle_face_ID_type& crossingFace,
                          bool terminated );

    void advanceCensus( photon_type& photon, 
                        double distanceToCensus,
                        const particle_face_ID_type& crossingFace,
                        bool& terminated,
                        bool& reachedCensus,
                        double dt,
                        double sigmaAbsorb, double sigmaScat );

    double min3( double x1, double x2, double x3 );  
    
//    reference to the mesh we are running on
    const Mesh_type& mMesh;

//    material properties stored in data base
    const Material_data_base_type& mMaterialDB;

//    base class reference to object providing opacity information;
    const Opacity_data_base<mesh_types, mat_types>& mOpacityDB;
    
//    Object that creates source photons for the IMC
    Source_data_base<mesh_types, mat_types>& mSourceDB;

//    Object that calculates analytic answer
    MCAnswer& mAnalyticAnswer;

//    Object that holds energy variables
    IMC_4Momentum<mesh_types, mat_types> m4Momentum;
    double RR_threshold;  //  used to decide to do Russian Roullette

//    Numbers of different photon events
//    variables whose values are cumulative from begining of problem:
    unsigned long long N_collisions, N_zone_crossings, N_census;
    unsigned long long N_entered;    //    number entering from other domains
    unsigned long long N_IMC_scatters, N_physical_scatters;
    unsigned long long N_RWAccelRadiusCalcs, N_RWAccelSegs;
    unsigned long long N_escaped, N_lost_track, N_RRed_up, N_RR_destroyed;
    unsigned long long N_split_added;    //    cumulative number added by spliting
    //! Number of blocking global counts to determine that all particles really have finished.
    unsigned long long N_global_count_checks;
//    total distance traveled by all photons - used to get mean free path
    double d_total;

//    number of paths taken by photons in each zone
    zcf_UINT64 N_paths;

//    How many photons have been created from sources, RR, etc. 
//    Frequently reset by parallel algorithm.
    unsigned long long N_temp_local_created;

//    variables whose values are updated each timestep:
    unsigned long long N_advanced;    //    Number advanced in current cycle
    unsigned long long N_RR_split;    //    Number split by RR_comb (NOT number created by split!)

    double tn, tnp1;
    unsigned int cycleNumber;

//    face indexed std::vector of pointers to Boundary_Condition classes
//    these modify photon properties when a face is crossed
    BC_list<Mesh_type,photon_type,fcf_BCPtr_type> mBCs;

//    cumulative energy through a face in units of E
    fcf_double mFaceE;
//    energy through a face in units of E in current timestep
    fcf_double mTimestepFaceE;

//    first part of names of output files
    std::string mFileNameHeader;

//    list of photons from census
    std::list<photon_type> census_photons;
    unsigned long long N_cycle_photons;     //    number we want after RR_comb

//    if we try to printoutput before setUp() is called, it core dumps.
    bool mIsSetUp;

//    for multiple copies of the domain running on different mpi processes
    bool mIsDomainDecomposed;
    unsigned int N_domains;
    unsigned int domainID;
    bool mIsMeshReplicated;
    unsigned int N_replicas;
    unsigned int replicaID;
    unsigned int nProcs;
    unsigned int procID;

//     for multiple IMC instances in OpenMP environment
    bool mIsOpenMPCoarseThreaded;
    unsigned int numOmpThreads;
    unsigned int ompThreadID;

    unsigned int message_check_period;

    std::vector<unsigned long long>& mULLThreadedDiagnosticData();
    std::vector<double>& mDoubleThreadedDiagnosticData();
    std::vector<photon_type>& mSharedPhotonList();
    bool& mGetSharedBool();

//    don't want these called, so make them private
    IMC();
    IMC( const IMC& );
    IMC& operator=( const IMC& );
};

//---------------------------------------------------------------------------//
//    definitions of inlined functions
//---------------------------------------------------------------------------//

template<typename mesh_types, typename mat_types>
inline double IMC<mesh_types, mat_types>::
eRad( const mesh_zone_ID_type& zone ) const
{
    ASSERT( m4Momentum.get_E_rad(zone) >= 0.0 );

    return m4Momentum.get_E_rad(zone)/mMesh.mesh_zone_volume(zone);
}

//---------------------------------------------------------------------------//

//    access function for number of paths taken by photon in zone

template<typename mesh_types, typename mat_types>
inline unsigned long long IMC<mesh_types, mat_types>::
nPaths( const mesh_zone_ID_type& zone ) const
{
    return N_paths[zone];
}

//---------------------------------------------------------------------------//

//    access function for cumulative energy that passed through face

template<typename mesh_types, typename mat_types>
inline double IMC<mesh_types, mat_types>::
faceE( const mesh_face_ID_type& face ) const
{
    return mFaceE[face];
}

//---------------------------------------------------------------------------//

//    access function for energy that passed through face in current timestep

template<typename mesh_types, typename mat_types>
inline double IMC<mesh_types, mat_types>::
timestepFaceE( const mesh_face_ID_type& face ) const
{
    return mTimestepFaceE[face];
}

//---------------------------------------------------------------------------//

template<typename mesh_types, typename mat_types>
inline double IMC<mesh_types, mat_types>::
min3( double x1, double x2, double x3 )
{
    using std::min;

    return min(x1, min(x2,x3) );
}

//---------------------------------------------------------------------------//

template<typename mesh_types, typename mat_types>
inline double IMC<mesh_types, mat_types>::
eCensus( const mesh_zone_ID_type& zone ) const
{
    ASSERT( mMesh.mesh_zone_volume(zone) > 0.0 );

    return m4Momentum.get_E_census(zone) / mMesh.mesh_zone_volume(zone);
}

//---------------------------------------------------------------------------//

template<typename mesh_types, typename mat_types>
inline void IMC<mesh_types, mat_types>::
RRDiagnostics( const mesh_zone_ID_type& zone,
               unsigned long long& nSourceRRCreated,
               unsigned long long& nSourceRRDestroyed,
               double& eSourceCreated,
               double& eSourceDestroyed,
               unsigned long long& nCensusRRCreated,
               unsigned long long& nCensusRRDestroyed,
               double& eCensusRRCreated,
               double& eCensusRRDestroyed,
               unsigned long long& nSmallPhotonRRCreated,
               unsigned long long& nSmallPhotonRRDestroyed,
               double& eSmallPhotonRRCreated,
               double& eSmallPhotonRRDestroyed,
               double& eRRTotal ) const
{
    ASSERT( mMesh.zone_is_real(zone) );

    mSourceDB.RRDiagnostics( zone,
                             nSourceRRCreated,
                             nSourceRRDestroyed,
                             eSourceCreated,
                             eSourceDestroyed );

    m4Momentum.RRDiagnostics( zone,
                              nCensusRRCreated, nCensusRRDestroyed,
                              eCensusRRCreated, eCensusRRDestroyed,
                              nSmallPhotonRRCreated,
                              nSmallPhotonRRDestroyed,
                              eSmallPhotonRRCreated,
                              eSmallPhotonRRDestroyed,
                              eRRTotal );
}


//---------------------------------------------------------------------------//

}    //    namespace IMC_namespace

#endif  // __IMC_hh__
