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


#include <cstdio>
#include "IMC.hh"
#include "shave.hh"
#include "ASSERT.hh"
#include "set_isotropic_cosines.hh"
#include "rotate_angles.hh"
#include "mfpsToCollision.hh"
#include "setDomainInformation.hh"

#ifdef USE_MPI
#include <mpi.h>
#include "Nonblocking_Sync.hh"
#include "Nonblocking_Gather.hh"
#endif

#include "printGlobalInfo.hh"

#ifdef USE_OPENMP
#include <omp.h>
#endif
#include "OpenMP_ReductionFunctors.hh"

#ifdef USE_TAU_EVENT_COUNTERS
#include "TAU.h"
#endif

using namespace std;

namespace IMC_namespace
{

//---------------------------------------------------------------------------//

template<typename mesh_types, typename mat_types>
IMC<mesh_types, mat_types>::
IMC( const Mesh_type& Mesh_in,
     const Material_data_base_type& Material_DB_in,
     const Opacity_data_base<mesh_types, mat_types>& Opacity_DB_in,
     const std::vector<Boundary_Condition<Mesh_type,photon_type>*>& user_BC_list,
     Source_data_base<mesh_types, mat_types>& Source_DB_in,
     MCAnswer& analyticAnswer,
     unsigned long long N_cycle_photons_in,
     double initial_time,
     string mFileNameHeader_in,
     double RR_thresholdIn,
     unsigned int message_check_period_in )
    : mMesh( Mesh_in ),
      mMaterialDB( Material_DB_in ),
      mOpacityDB( Opacity_DB_in ),
      mSourceDB( Source_DB_in ),
      mAnalyticAnswer( analyticAnswer ),
      m4Momentum(Mesh_in,
                 Material_DB_in,
                 Source_DB_in, 
                 RR_thresholdIn),
      RR_threshold( RR_thresholdIn ),
      N_paths( Mesh_in ),
      tn( initial_time ),
      mBCs( Mesh_in ),
      mFaceE( Mesh_in ),
      mTimestepFaceE( Mesh_in ),
      mFileNameHeader( mFileNameHeader_in ),
      mIsSetUp( false ),
      message_check_period( message_check_period_in )
{
#pragma omp single
    {
        std::vector<unsigned long long> &theULLBuffer = mULLThreadedDiagnosticData();
        std::vector<double> &theDoubleBuffer = mDoubleThreadedDiagnosticData();
        std::vector<photon_type> &theSharedPhotonList = mSharedPhotonList();
        bool &theSharedBool = mGetSharedBool();
        theSharedBool=false;
    }

// check that unsigned long long type is big enough to tally into;
// it needs to be a 64 bit int
    if( sizeof(unsigned long long) < 8 )
    {
        cout << "size of unsigned long long is less than 64 bit ";
        cout << "on this machine. exiting..."<< endl;
        exit(0);
    }

    N_cycle_photons = N_cycle_photons_in;
    N_collisions = 0;
    N_zone_crossings = 0;
    N_census = 0;
    N_entered = 0;
    N_physical_scatters = 0;
    N_RWAccelRadiusCalcs = 0;
    N_RWAccelSegs = 0;
    N_escaped = 0;
    N_lost_track = 0;
    N_RRed_up = 0;
    N_RR_destroyed = 0;
    N_split_added = 0;
    N_global_count_checks = 0;
    d_total = 0.0;
    N_advanced = 0;
    N_RR_split = 0;
    N_paths = 0;

// cycleNumber is incremented at the start of advance; it is equal to 1
// during the first cycle
    cycleNumber = 0;

    mFaceE = 0.0;

// these will be set to false if output functions are ever called
    create_temporal_output_file = true;
    create_spatial_output_file = true;

// Default transmitting BC's have been set in constructor of BC_list.
// Now set user defined BC's. Set Boundary_Condition pointers in BC_list
    typename std::vector<Boundary_Condition<Mesh_type,photon_type>*>::const_iterator BC;
    for( BC = user_BC_list.begin(); BC != user_BC_list.end(); ++BC )
    {
    // Go over every face of BC and set the BC_list[face] pointer to BC
        typename Boundary_Condition<Mesh_type,photon_type>::face_iterator f;
        for( f = (**BC).face_begin(); f != (**BC).face_end(); ++f )
        {
        //    Only apply BC if face abuts physical zone
            if (mMesh.face_is_real(*f))
                mBCs.set_BC(*f, *BC);
        }
    }

    setDomainInformation( mMesh,
                          mIsDomainDecomposed,
                          N_domains,
                          domainID,
                          mIsMeshReplicated,
                          N_replicas,
                          replicaID,
                          nProcs,
                          procID,
                          mIsOpenMPCoarseThreaded,
                          numOmpThreads,
                          ompThreadID);
}

//---------------------------------------------------------------------------//

template<typename mesh_types, typename mat_types>    
std::vector<unsigned long long>& 
IMC<mesh_types, mat_types>::
mULLThreadedDiagnosticData()
{
    static std::vector<unsigned long long> sULLThreadedDiagnosticData(1);
    return sULLThreadedDiagnosticData;
}

//---------------------------------------------------------------------------//

template<typename mesh_types, typename mat_types>
std::vector<double>& 
IMC<mesh_types, mat_types>::
mDoubleThreadedDiagnosticData()
{
    static std::vector<double> sDoubleThreadedDiagnosticData(1);
    return sDoubleThreadedDiagnosticData;
}

//---------------------------------------------------------------------------//

template<typename mesh_types, typename mat_types> 
std::vector<photon<typename mesh_types::particle_zone_ID_type, typename mesh_types::Vector3d> >& 
IMC<mesh_types, mat_types>::
mSharedPhotonList()
{
    static std::vector<photon_type> sSharedPhotonList;
    return sSharedPhotonList;
}

//---------------------------------------------------------------------------//

template<typename mesh_types, typename mat_types> 
bool& 
IMC<mesh_types, mat_types>::
mGetSharedBool()
{
    static bool sBoolData;
    return sBoolData;
}


//---------------------------------------------------------------------------//

//    add initial census photons and energy accounting in IMC4_Momentum

template<typename mesh_types, typename mat_types> void
IMC<mesh_types, mat_types>::
set_initial_census_photons( double t_initial )
{
    ASSERT( census_photons.empty() );
                                           
//    There are no initial photons
    double E_initial_photons = 0.0;
    zcf_double E_initial_photons_zone( mMesh );
    E_initial_photons_zone = 0.0;
    
    m4Momentum.set_initial_E_census( E_initial_photons, E_initial_photons_zone );
}

//---------------------------------------------------------------------------//


// to change number of photons run in a cycle

template<typename mesh_types, typename mat_types>
void IMC<mesh_types, mat_types>::
set_N_cycle_photons( unsigned long long N_cycle_photons_in )
{
    N_cycle_photons = N_cycle_photons_in;
}

//---------------------------------------------------------------------------//

// Function that calculates distance to collision.

template<typename mesh_types, typename mat_types>
double IMC<mesh_types, mat_types>::
distanceToCollision( double sigmaScat, double r ) const
{
    REQUIRE( sigmaScat >= 0.0 );

    double d_col = mfpsToCollision(r)/(sigmaScat + 1.e-99);
    // double d_col = -log(r)/(sigmaScat + 1.e-99);
    ENSURE( d_col >= 0.0 );

    return d_col;
}

//---------------------------------------------------------------------------//

template<typename mesh_types, typename mat_types>
void IMC<mesh_types, mat_types>::
advanceBoundary( photon_type& photon,
                 double distToBoundary,
                 particle_face_ID_type& crossingFace,
                 particle_zone_ID_type& nextZone,
                 bool& exitsDomain,
                 bool& exitsProblem,
                 bool& terminated, double dt,
                 double& sigmaAbsorb, double& sigmaScat )
{
    REQUIRE( mMesh.zone_is_real(photon.zone) );
    REQUIRE( photon.consistent() );

    photon.move(distToBoundary);

    Vector3d movedX = photon.X;

    d_total += distToBoundary;

    m4Momentum.tallyRadiationEnergy( sigmaAbsorb, distToBoundary,
                                     photon, dt );

    m4Momentum.attenuate( distToBoundary, sigmaAbsorb, photon, photon.Omega );
                          
    m4Momentum.Russian_Roulette( photon, terminated, 
                                 N_RRed_up, N_RR_destroyed );
        
    if( terminated )
    {
       return;
    }

    ++N_zone_crossings;

// photon is now at face; apply BC to it. This will determine if
// it has exited the problem, been reflected back in, or been
// refracted. We only test if photon actually went into another
// zone => enters_new_zone == true. Need to see if photon is
// actually crossing the face in the PolyMesh. Otherwise, BC gets
// imposed on crossings of side planes within zone.

    double E_old = photon.Energy;
    Vector3d Omega_old = photon.Omega;

    bool enters_new_zone = true;

    mBCs(crossingFace)->impose_BC( crossingFace,
                                   photon,
                                   nextZone,
                                   enters_new_zone,
                                   exitsDomain,
                                   exitsProblem );

    if ( exitsProblem )
    {
       // only tally exit for global boundaries, not processor boundaries.
       // You get one result for all processor decompositions
        m4Momentum.HandleExit( photon );
    }
    m4Momentum.HandleBC( E_old, Omega_old, photon);

//    since it's really at a face, tally the face energy
    mFaceE[crossingFace] += photon.Energy;
    mTimestepFaceE[crossingFace] += photon.Energy;


// photon goes out of problem boundary (not just to other domain)
    if( exitsProblem || exitsDomain )  
    {
        ++N_escaped;
        terminated = true;
        return;
    }

    // change zone and reset the local zone data
    ASSERT( mMesh.zone_is_real(nextZone) );
    ASSERT( mMesh.particle_zone_volume(nextZone) >= 0. );

    bool reflected = false;
    if (photon.zone == nextZone)
       reflected = true;
    else
       photon.zone = nextZone;
    
    if ( mMesh.d_min(photon.X, photon.zone) <= -mMesh.getTrackingTolerance() )
    {
       cerr << "IMC::advanceBoundary:  Put photon outside next zone,"
            << " but photon not exiting problem."  << endl << flush;
       cerr << "  distToBoundary: " << distToBoundary << endl << flush;
       cerr << "  Terminating photon." << endl << flush;  
       terminated = true;
       return;
    }

    ASSERT( mMesh.d_min( photon.X, photon.zone) > -mMesh.getTrackingTolerance() );

    // photon enters new zone unless BC reflects it back and changes
    // enters_new_zone to false. I am checking it here because I think
    // the check is faster than the calculations below.
    if( enters_new_zone )
    {
        size_t count = 0;
        size_t maxCount = 100;

        if (count == maxCount)
        {
           cerr << "IMC::advanceBoundary:  Photon left mesh zone.  No good next zone."
                << endl;
           cerr << "  Terminating photon." << endl << flush;  
           terminated = true;
           return;
        }

        if (!mMesh.zone_is_real(photon.zone) )
        {
           cerr << "IMC::advanceBoundary:  Photon mesh zone is not real"
                 << endl << flush; 
           cerr << "  Terminating photon." << endl << flush;  
           terminated = true;
        }
    }

    setOpacities( photon, sigmaAbsorb, sigmaScat );

    ENSURE( photon.consistent() );
}

//---------------------------------------------------------------------------//

template<typename mesh_types, typename mat_types>
void IMC<mesh_types, mat_types>::
advanceCollision( photon_type& photon,
                  double distToCollision,
                  bool& terminated,
                  double dt,
                  double& sigmaAbsorb,
                  double& sigmaScat )
{
    REQUIRE( mMesh.zone_is_real(photon.zone) );

    photon.move( distToCollision );

    d_total += distToCollision;

    m4Momentum.tallyRadiationEnergy( sigmaAbsorb, distToCollision,
                                     photon, dt );

    m4Momentum.attenuate( distToCollision, sigmaAbsorb, photon, photon.Omega );

    m4Momentum.Russian_Roulette( photon, terminated, 
                                 N_RRed_up, N_RR_destroyed );
                                     
    if ( terminated ) 
    {
       return;
    }

    // In RZ, photon motion within a zone can cause a change of group
    setOpacities( photon, sigmaAbsorb, sigmaScat );

    processCollision( photon, sigmaScat, sigmaAbsorb ); 

    ENSURE( mMesh.zone_is_real(photon.zone) );
    ENSURE( photon.consistent() );
}

//---------------------------------------------------------------------------//

template<typename mesh_types, typename mat_types>
void IMC<mesh_types, mat_types>::
advanceCensus( photon_type& photon,
               double distToCensus,
               const particle_face_ID_type& crossingFace,
               bool& terminated, bool& reachedCensus,
               double dt,
               double sigmaAbsorb, double sigmaScat )
{    
    REQUIRE( mMesh.zone_is_real(photon.zone) );

    if( terminated )
       return;

    if ( !mMesh.zone_is_real(photon.zone) )
    {
       cerr << "IMC::advanceCensus: Photon in unowned zone.";
       cerr << "  Terminating photon." << endl << flush;  
       terminated = true;
       return;
    }

    ASSERT( mMesh.particle_zone_volume(photon.zone) >= 0. );
    ASSERT( mMesh.zone_is_real(photon.zone) );

    photon.move( distToCensus );

    ASSERT( mMesh.d_min(photon.X, photon.zone) > -mMesh.getTrackingTolerance() ); 

    d_total += distToCensus;
    
    m4Momentum.tallyRadiationEnergy( sigmaAbsorb, distToCensus,
                                     photon, dt );

    m4Momentum.attenuate( distToCensus, sigmaAbsorb, photon, photon.Omega );
                          
    m4Momentum.Russian_Roulette( photon, terminated, 
                                 N_RRed_up, N_RR_destroyed );
//    if photon not killed by attenuation, it gets put in census
    if( !terminated )
    {
        reachedCensus = true;

        ENSURE( mMesh.zone_is_real(photon.zone) );
        ENSURE( photon.consistent() );
    }
}

//---------------------------------------------------------------------------//

template<typename mesh_types, typename mat_types>
void IMC<mesh_types, mat_types>::
advancePhoton( photon_type& photon, 
               bool& exitsDomain, 
               bool& exitsProblem, 
               bool& terminated, 
               double dt )
{
    REQUIRE( photon.consistent() );
    REQUIRE( photon.t - tn > minDistanceToCensus  );
    REQUIRE( tnp1 - photon.t > minDistanceToCensus );
    ASSERT( mMesh.zone_is_real(photon.zone) );
    ASSERT( mMesh.mesh_zone_volume( photon.zone ) >= 0.0 );
    if( mMesh.d_min( photon.X, photon.zone) <= -mMesh.getTrackingTolerance() )
    {
        double minDist = mMesh.d_min( photon.X, photon.zone);        
        ASSERT( mMesh.d_min( photon.X, photon.zone) > -mMesh.getTrackingTolerance() );
    }
    
    exitsDomain = false;
    exitsProblem = false;
    terminated = false;
   
//   local variables that hold scattering info for the zone
    double sigmaAbsorb, sigmaScat;
    setOpacities( photon, sigmaAbsorb, sigmaScat );

    bool reachedCensus = false;

    unsigned long long N_trajectories = 0;

    while( !terminated && !reachedCensus )
    {
        double distToCensus = tnp1 - photon.t;
        ASSERT( distToCensus > minDistanceToCensus );    // allow for roundoff
        ASSERT( dt - distToCensus >= minDistanceToCensus );

        double distToCollision =
          distanceToCollision( sigmaScat, photon.random_number() );

        ASSERT( distToCollision > 0.0 );

    //    physical event is something physical that happens to photon, as
    //    opposed to boundary crossing
        double distToEvent = std::min( distToCensus, distToCollision );
        ASSERT( distToEvent > -mMesh.getTrackingTolerance() );

    //    calculate distance to boundary crossing
        double distToBoundary;
        particle_face_ID_type crossingFace;
        particle_zone_ID_type nextZone;
        bool lostTrack = false;

        distToBoundary = mMesh.distanceToBoundary( photon.X,
                                                   photon.Omega,
                                                   photon.zone,
                                                   crossingFace,
                                                   nextZone,
                                                   exitsProblem,
                                                   lostTrack );

        // check for lost particle track
        if ( lostTrack )
        {
           bool foundZone = false;
           photon.zone = mMesh.particleZoneOfPoint( photon.X, photon.zone, foundZone );

           ++N_lost_track;
           cerr << "IMC::advancePhoton: lostTrack after distanceToBoundary."
                << endl << flush;
           cerr << "  Terminating photon" << endl << flush;
           terminated = true;
           break;
        }

        ASSERT( distToBoundary > -1.e-9 );  //  allow < 0 due to roundoff
        
        N_paths[photon.zone] += 1;

        ASSERT( mMesh.boundary_face(crossingFace) ||
                mMesh.mesh_zone_volume(nextZone) >= 0.0 );


    //    find minimum distance to determine type of event
        if (distToBoundary < distToEvent)
        {
            advanceBoundary( photon,
                             distToBoundary,
                             crossingFace,
                             nextZone,
                             exitsDomain,
                             exitsProblem,
                             terminated, dt,
                             sigmaAbsorb, sigmaScat );
        }
        else if( distToCollision == distToEvent )
        {
            advanceCollision( photon,
                              distToCollision,
                              terminated,
                              dt,
                              sigmaAbsorb, sigmaScat );

            reachedCensus = false;
        }
        else 
        {
            advanceCensus( photon,
                           distToCensus,
                           crossingFace,
                           terminated, reachedCensus, dt,
                           sigmaAbsorb, sigmaScat );
        }

        photon.justEmitted = false; 

        ++N_trajectories;
        if( N_trajectories > 100000 )
        {
           cerr << endl;
           cerr << "NOTE - too many trajectories ";
           cerr << "for a photon in advancePhoton!!!" << endl << flush;
           cerr << "N_trajectories = " << N_trajectories << endl << flush;

           bool foundZone = false;
           photon.zone = mMesh.particleZoneOfPoint(photon.X, photon.zone, foundZone);
           
           cerr << "  sigmaAbsorb = " << sigmaAbsorb << endl;
           cerr << "  sigmaScat = " << sigmaScat << endl;
           cerr << "  Terminating photon" << endl << flush;
           terminated = true;
           break;
        }

    }  //  end of for loop over alternatives for current_photon
}

//---------------------------------------------------------------------------//

//    stuff that has to be done once each time step before we call advance

template<typename mesh_types, typename mat_types>
void IMC<mesh_types, mat_types>::
setUp( double dt )
{
    if( !mOpacityDB.isSetUp() )
        cerr << "need to call mOpacityDB.isSetUp() before IMC::setUp()!" << endl;
    ASSERT( mOpacityDB.isSetUp() );

    if( !mMaterialDB.isSetUp() )
        cerr << "need to call mMaterialDB.isSetUp() before IMC::setUp()!" << endl;

    ASSERT( mMaterialDB.isSetUp() );

//    set the future time
    tnp1 = tn + dt;

//    make sure tallys are zeroed
    m4Momentum.setUp( dt );

//    set these variables to zero before each call to advance
    N_advanced = 0;
    mTimestepFaceE = 0.0;

    mSourceDB.setUp( tn, dt );

//    if it's the first cycle, get initial distribution of photons
//    to serve as census particles at start of problem
    if( cycleNumber == 1 )
    {
        set_initial_census_photons( tn );
    }
    else    //    reset time for already existing census photons
    {
    //    loop over census photons, and set time
        typename list<photon_type>::iterator census_photonItr;
        for( census_photonItr = census_photons.begin();
             census_photonItr != census_photons.end();
             ++census_photonItr )
        {
            ASSERT( mMesh.zone_is_real(census_photonItr->zone) );

            census_photonItr->t = tn;
        }
    }

//    Next, comb the census photons to reduce their number.
    m4Momentum.RR_comb_census( census_photons, N_cycle_photons,
                               N_RR_split, N_split_added, 
                               N_RRed_up, N_RR_destroyed,
                               tn, dt );

//    reset Energy0 in census particles to be their current energy
    typedef typename list<photon_type>::iterator photonItr_type;
    for( photonItr_type photonItr = census_photons.begin();
         photonItr != census_photons.end();
         ++photonItr )
    {
        ASSERT( photonItr->Energy > 0. );
        photonItr->Energy0 = photonItr->Energy;
    }

    mIsSetUp = true;
}

//---------------------------------------------------------------------------//

//    census photons are processed first to save memory. They are finished
//    before the new source photons are created, so that they don't all have
//    to be stored simultaneously

template<typename mesh_types, typename mat_types>
void IMC<mesh_types, mat_types>::
advance( double dt )
{
//    set the current cycle number;
   cycleNumber += 1;

   setUp( dt );
 
//    move census photons onto list of photons to process
   list<photon_type> photon_list;
   photon_list.splice( photon_list.end(), census_photons );
   ASSERT( census_photons.empty() );

//    Get source photons and put them onto photon list
   mSourceDB.get_source_photons( N_cycle_photons,
                                 m4Momentum.get_E_total(),
                                 tn, dt, photon_list );

//    advance all photons
   advancePhotonList( photon_list, dt );

//    update book keeping
//    sum INT64 values before doing matter energy update
   m4Momentum.sum_over_replicated_meshes();

//    get info energy balance check
   m4Momentum.EPBookkeeping();

//    Do all-reduce of face fluxes here.

   if( mIsOpenMPCoarseThreaded )
   {
       mFaceE.threadSum();
       mTimestepFaceE.threadSum();
   }

   
   if( mIsMeshReplicated )
   {
       mFaceE.replicaSum();
       mTimestepFaceE.replicaSum();
   }
   
//    update time
   tn += dt;

}


//---------------------------------------------------------------------------//

template<typename mesh_types, typename mat_types>
void IMC<mesh_types, mat_types>::
advancePhotonList( list<photon_type>& photon_list, double dt )
{
#ifdef USE_MPI

   if( !mIsDomainDecomposed || nProcs == 1 )

   {
      advancePhotonListSimple( photon_list, dt);
   }
   else
   {
      advancePhotonListDD( photon_list, dt);
   }

#else

   advancePhotonListSimple( photon_list, dt);

#endif

}

//---------------------------------------------------------------------------//
   
template<typename mesh_types, typename mat_types>
void IMC<mesh_types, mat_types>::
advancePhotonListSimple( list<photon_type>& photon_list, double dt )
{
    ASSERT( !mIsDomainDecomposed );
    ASSERT( N_domains == 1 );

//    census photons should all have been put onto photon_list argument to
//    this function before it was called
    ASSERT( census_photons.empty() );
    // --------------------------------------------
    // Start processing loop.

    while(!photon_list.empty())
    {
       // Use reference instead of copying it.
       photon_type& photon = photon_list.back();

       ASSERT( photon.consistent() );
       bool exitsDomain, exitsProblem, terminated;
       advancePhoton(photon,  exitsDomain, exitsProblem, terminated, dt);

       N_advanced += 1;

       if (!terminated)
       {
          ASSERT(fabs(photon.t - tnp1) < 1.0e-10 );

          m4Momentum.do_census_tally( photon);
          ++N_census;

          // Store photon in a different list if it's done.
          census_photons.push_back( photon );
       }

       // Remove and delete photon from list.  If it reached census, we
       // already stored it in census_photons.

       photon_list.pop_back();        
    }

    ASSERT( photon_list.empty() );
}

//---------------------------------------------------------------------------//
   
#ifdef USE_MPI
//! Gather global counts to root processor.  Global output variables are
//! Only changed on the root processor.
template<typename mesh_types, typename mat_types>
void IMC<mesh_types, mat_types>::
verify_all_done( unsigned long long local_created,
                 unsigned long long local_completed,
                 unsigned long long& global_created,
                 unsigned long long& global_completed,
                 unsigned int root,
                 MPI_Comm comm ) const

{
   unsigned long long local_counts[2] = { local_created, local_completed };
   unsigned long long global_counts[2] = {0,0};

   MPI_Reduce( local_counts,
               global_counts,
               2,
               MPI_UNSIGNED_LONG_LONG,
               MPI_SUM,
               root,
               comm );

   if( root == procID )
   {
      global_created = global_counts[0];
      global_completed = global_counts[1];
   }
}
#endif


//---------------------------------------------------------------------------//
template<typename mesh_types, typename mat_types>
void IMC<mesh_types, mat_types>::
advancePhotonListDD( list<photon_type>& photon_list, double dt )
{
#ifdef USE_MPI


    int myMpiRank=0,tid=0;
    MPI_Comm_rank(MPI_COMM_WORLD,&myMpiRank);
#ifdef USE_OPENMP
    tid = omp_get_thread_num();
#endif

//    2**32-1 = max value for 32 bit int
    const unsigned long maxGlobalCountChecksPerCycle = 4294967295UL;

//    number of times we try to check all-done in this cycle
    unsigned long long nGlobalCountChecksPerCycle = 0;

    unsigned long long N_completed_this_domain = 0;

    //
    // get the thread-shared bool data
    //
    bool &threadSyncGlobalFinished = mGetSharedBool();
    
//    census photons should all have been put onto photon_list argument to
//    this function before it was called
    ASSERT( census_photons.empty() );

    // Counts of particles created and particles completed (in that order)
    // on this processor and all children processors since the last
    // time we sent the counts to our parent.  We count initial census
    // photons as being "created" for this time step.
    unsigned long long temporary_self_and_children_counts[2] = 
       { 0, 0 };

    // Also keep local only copies of the number created and completed
    // for verification that the time step is actually over.
    unsigned long long local_only_created = 0;
    unsigned long long local_only_completed = 0;

    // These are only used on the root processor.
    unsigned long long global_created = 0;
    unsigned long long global_completed = 0;

    // Set up MPI calls for domain boundaries
    mSourceDB.setUpExternalSourcePhotons();
    
    // Choose some "random" numbers as MPI tags for the messages;
    // any mostly unique numbers should do.  Only master threads participate
    // in the gather / Sync processing.
    Nonblocking_Gather gather( 6432, 2, mMesh.getWorldCommunicator() );
    Nonblocking_Sync maybe_done_sync( 6502, mMesh.getWorldCommunicator() );
    Nonblocking_Sync really_done_sync( 7294, mMesh.getWorldCommunicator() );

    ASSERT( gather.root_rank() == maybe_done_sync.root_rank() );
        
    // --------------------------------------------
    // Start processing loop.

    // count how many are created.
    N_temp_local_created = photon_list.size();

    int global_finished_iter=0;

    long long int tau_process_photon_count=0;
    long long int tau_check_message_count=0;

    bool global_finished = false;
    while(!global_finished)
    {
        global_finished_iter++;

        //
        // first process each threads photon_list
        while(!photon_list.empty())
        {
	  tau_process_photon_count++;

            // Use reference instead of copying it.
            photon_type& photon = photon_list.back();
            
            ASSERT( photon.consistent() );
            
            bool exitsDomain, exitsProblem, terminated;
            advancePhoton(photon, exitsDomain, exitsProblem, terminated, dt);
            
            if( !exitsDomain )
                ++N_completed_this_domain;
            
            N_advanced += 1;
            
            if (!terminated)
            {
                ASSERT(fabs(photon.t - tnp1) < 1.0e-10 );
                
                m4Momentum.do_census_tally( photon);
                ++N_census;                
                
                // Store photon in a different list if it's done.
                census_photons.push_back( photon );
            }
            
            // Remove and delete photon from list.  If it reached census, we already stored it.
            photon_list.pop_back();
            
        }//end of while(!photon_list.empty())

        //
        // accumulate the thread values of N_temp_local_created, N_completed_this_domain
        //
        unsigned long long thread_data[2] = {N_temp_local_created, N_completed_this_domain };
        
        gather.accumulate_local( thread_data );

        //
        // every thread grabs a handle to the empty shared photon list
        //
        std::vector<photon_type> &theSharedPhotonList = mSharedPhotonList();

        
#pragma omp master
        {
            ASSERT( theSharedPhotonList.size() == 0 );
            //
            // update N_temp_local_created, N_completed_this_domain
            // with threaded sum of these values computed above
            //
            N_temp_local_created = thread_data[0];
            N_completed_this_domain = thread_data[1];

            // storage for getting photons from other domains.
            list<photon_type> external_list;

            // Don't check for parallel messages every time if we still have particles to move.
            if( (N_advanced % message_check_period == 0 ) || photon_list.empty() )
            {
	      tau_check_message_count++;

                //    these values aren't currently used
                double E_entered=0.0;

                mSourceDB.get_external_source_photons( external_list,
                                                       E_entered,
                                                       photon_list.empty(),
                                                       N_entered );

                // Check for maybe done message, just in case the root
                // is out of sync even if we are not done yet.
                if(procID != gather.root_rank() && maybe_done_sync.finished() )
                {
                    unsigned long long gcrt = 0;
                    unsigned long long gcmp = 0;
                    verify_all_done( local_only_created,
                                     local_only_completed,
                                     gcrt,
                                     gcmp,
                                     gather.root_rank(),
                                     mMesh.getWorldCommunicator()
                                     );

                    ++N_global_count_checks;

                    maybe_done_sync.reset();
                }

                //
                // photon_list can be empty only if mSourceDB.get_external_source_photons() above resulted 
                // in no additional photons getting stuffed onto my list.
                // If this is the case, I may be done and will add up created, completed and send to parent

                if( photon_list.empty() )
                {
                    // Add up number created
                    temporary_self_and_children_counts[0] += N_temp_local_created;
                    local_only_created += N_temp_local_created;
                    N_temp_local_created = 0;

                    // Add up number completed.
                    temporary_self_and_children_counts[1] += N_completed_this_domain;
                    local_only_completed += N_completed_this_domain;
                    N_completed_this_domain = 0;
            
                    // Get counts from our children, and since there is nothing else to do,
                    // send our local count plus the our children's counts to our parent.
                    // temporary_self_and_children_counts is reset by accumulate_and_send
                    // on all but the root processor.

                    gather.accumulate_and_send( temporary_self_and_children_counts );

                    // Check for being completely done if we're the root processor.
                    if(procID == gather.root_rank())
                    {
                        nGlobalCountChecksPerCycle += 1;
  
                        if(  nGlobalCountChecksPerCycle >= maxGlobalCountChecksPerCycle )
                        {
                            cout << "The number of times the code has checked that the IMC" << endl;
                            cout << "has completed all the particles for this cycle has " << endl;
                            cout << "exceeded the maximum value of " << maxGlobalCountChecksPerCycle << endl;
                            cout << "nGlobalCountChecksPerCycle = " << nGlobalCountChecksPerCycle << endl;
                        }
                        VERIFY( nGlobalCountChecksPerCycle < maxGlobalCountChecksPerCycle );

                        // See if the number created is the same as the number completed
                        if( temporary_self_and_children_counts[0] == temporary_self_and_children_counts[1] )
                        {
                            maybe_done_sync.send_finished();

                            verify_all_done( local_only_created,
                                             local_only_completed,
                                             global_created,
                                             global_completed,
                                             gather.root_rank(),
                                             mMesh.getWorldCommunicator() );

                            ++N_global_count_checks;

                            maybe_done_sync.reset();

                            if( global_created == global_completed )
                            {
                                global_finished = true;

                                // Even if we are all done, it could be that we verified by accident
                                // and that there are some outstanding messages flying around
                                // in the nonblocking gather.  We need to wait until all those
                                // mesages come in so that they do not mess up the count for the
                                // next time step.
                                while( global_created != temporary_self_and_children_counts[0] 
                                       || global_completed != temporary_self_and_children_counts[1] )
                                {
                                    gather.accumulate_and_send( temporary_self_and_children_counts );

                                }

                                really_done_sync.send_finished();
                            }
                        }
                    }
                    else
                    {
                        if( really_done_sync.finished() )
                        {
                            global_finished = true;
                        }
                    }
                }//end of photon_list.empty() condition
                else
                {
                    //
                    // photon_list.empty() is false -- we've got more local photons to process.
                    //
                    // We do check to see if children are done and sent messages to us.  If so, we
                    // accumulate them and store for future use.
                    //

                    // Add up number created
                    temporary_self_and_children_counts[0] += N_temp_local_created;
                    local_only_created += N_temp_local_created;
                    N_temp_local_created = 0;

                    // Add up number completed.
                    temporary_self_and_children_counts[1] += N_completed_this_domain;
                    local_only_completed += N_completed_this_domain;
                    N_completed_this_domain = 0;

                    // Get counts from children, but don't send to parent since
                    // we still have local work to do and we know we'll have to send 
                    // later once we are out of local particles.

                    gather.accumulate( temporary_self_and_children_counts );
                }
            }

            //
            // on the master thread set the global_finished state to the shared bool
            //
            threadSyncGlobalFinished = global_finished;
                    
            //
            // copy master threads photon_list to the shared photon list for
            // distribution to the threads
            //
            std::copy(external_list.begin(), external_list.end(), back_inserter(theSharedPhotonList) );
            
            //
            // clear out my (master thread) external_list
            //
            external_list.clear();
            
        }//end of omp master section

        //
        // need a barrier here to correctly sync all threads global_finished to
        // the master thread's copy.  It is only the master thread which can make
        // the determination if we are globally finished.
        //
#pragma omp barrier

        //
        // copy the master thread's global_finshed state to all other threads
        //
        global_finished = threadSyncGlobalFinished;
        
        //
        // copy portions of the shared photon list to each threads local photon_list.
        // Do this chunk by chunk with the last thread getting the leftovers.
        //
#pragma omp critical (IMC_SplatPhotonsToThreads)
        {
            ASSERT( photon_list.size() == 0 );

            size_t chunkSize = theSharedPhotonList.size()/omp_get_num_threads();
            size_t offset = omp_get_thread_num() * chunkSize;

            std::copy(theSharedPhotonList.begin() + offset, theSharedPhotonList.begin() + offset + chunkSize, back_inserter(photon_list) );

            //
            // the last thread gets the excess photons
            //
            if( tid == omp_get_num_threads() - 1 )
            {
                std::copy(theSharedPhotonList.begin() + offset + chunkSize, theSharedPhotonList.end(), back_inserter(photon_list) );
            }
        }//end of critical section IMC_SplatPhotonsToThreads

        //
        // This barrier is needed to protect theSharedPhotonList from being 
        // erased (see ~20 lines below) before every thread gets a chance to copy out
        // their portion of the particles.
        //
#pragma omp barrier

        //
        // zero out N_temp_local_created, N_completed_this_domain 
        //
        N_temp_local_created=0;
        N_completed_this_domain=0;
        
        //
        // reset the thread-shared accumulation buffer
        //
        gather.reset_shared_buffer();

        //
        // clear out the shared photon list so it'll be ready for use next iteration
        //
#pragma omp single
        {
            theSharedPhotonList.clear();
        }
        

    }// end of while(!global_finished) loop

#ifdef USE_TAU_EVENT_COUNTERS
    TAU_REGISTER_EVENT(ev1,"process photons");
    TAU_EVENT(ev1, tau_process_photon_count);

    TAU_REGISTER_EVENT(ev2,"check all done messages");
    TAU_EVENT(ev2, tau_check_message_count);

    // get some events from the communication utilities

    extern int num_sync_sends;
    TAU_REGISTER_EVENT(ev3,"Nonblocking_Sync sends");
    TAU_EVENT(ev3, num_sync_sends);


    extern int num_gather_sends;
    TAU_REGISTER_EVENT(ev4,"Nonblocking_Gather sends");
    TAU_EVENT(ev4, num_gather_sends);


    extern int num_buffered_mpi_send_sends;
    TAU_REGISTER_EVENT(ev5,"Buffered_MPI_Send sends");
    TAU_EVENT(ev5, num_buffered_mpi_send_sends);


#endif

    mSourceDB.finalize_external_source_photons( );

    ASSERT( photon_list.empty() );
#endif

}

//---------------------------------------------------------------------------//

template<typename mesh_types, typename mat_types>
void IMC<mesh_types, mat_types>::
temporalOutput(double dt)
{
//    Need to make sure memory is allocated, etc, before you try to print
    if( !mMaterialDB.isSetUp() )
        cout << "need to call mMaterialDB.setUp() before IMC::temporalOutput!" << endl;
    if( !mIsSetUp )
        cout << "need to call IMC::setUp() before IMC::temporalOutput!" << endl;

//    if it's the first call of this function create new output file,
//    otherwise append to currently existing one

    string time_output_file_name( mFileNameHeader+"_temporalOutput");
    ofstream time_output_file;
    if (create_temporal_output_file)
    {
    //    create file
        time_output_file.open( time_output_file_name.c_str(), ios::out );

    //    add header
        string header = "#cycle     t      dt     ";
        header += "Nphot           ";
        header += "er        Ecens";
        time_output_file << header << endl;

        create_temporal_output_file = false;
    }
    else
    {
    //    open for appending to already existing file
        time_output_file.open( time_output_file_name.c_str(), ios::app );
    }

    const mesh_zone_ID_type& zone = *(mMesh.PhysicalZoneBegin());

    cout.precision(6);
    cout << "cycle = " << cycleNumber << "  ";
    cout << "tn = " << tn << "  dt = " << dt << endl;
    printf("  Npart = %llu\n", N_advanced );

    cout << endl;

    time_output_file.precision(6);
    const int width = 12;

    time_output_file << setw(width) << cycleNumber;
    time_output_file << setw(width) << tn;
    time_output_file << setw(width) << dt;
    time_output_file << setw(width) << N_advanced;
   
    time_output_file << setw(width) << eRad( zone );

    time_output_file << setw(width) << m4Momentum.get_E_census(zone);

    time_output_file  << endl;
}

//---------------------------------------------------------------------------//

template<typename mesh_types, typename mat_types>
void IMC<mesh_types, mat_types>::
spatialOutput()
{
//    Need to make sure memory is allocated, etc, before you try to print
    if( !mMaterialDB.isSetUp() )
        cout << "need to call mMaterialDB.setUp() before IMC::spatialOutput!" << endl;
    if( !mIsSetUp )
        cout << "need to call IMC::setUp() before IMC::spatialOutput!" << endl;

//    if it's the first call of this function create new output file,
//    otherwise append to currently existing one

    string space_output_file_name( mFileNameHeader+"_spatialOutput");
    ofstream space_output_file;

    if (create_spatial_output_file)
    {
    //    create file
        space_output_file.open( space_output_file_name.c_str(), ios::out );

    //    add header
        space_output_file << "               zone";
        space_output_file << "        x         y";
        space_output_file << "           Er";
        space_output_file << "     Er analytic";
        space_output_file << "     frac error";
        space_output_file << endl;

        create_spatial_output_file = false;
    }
    else
    {
    //    open for appending to already existing file
        space_output_file.open( space_output_file_name.c_str(), ios::app );
        space_output_file << endl;
    }
    space_output_file << "tn = " << tn << endl << endl;

    unsigned int precision = 25;

    space_output_file.precision(precision);
    const int dwidth = precision + 9;
    space_output_file.setf(ios::right,ios::adjustfield);

//   scale factor for relative error is a fraction of x = y = 0 value
    double scaleFactor = 1.0e-4*mAnalyticAnswer.Answer(0.0, 0.0, tn);
                                                     
    for( ZoneIterator zoneItr = mMesh.PhysicalZoneBegin(); 
         zoneItr != mMesh.PhysicalZoneEnd(); ++zoneItr )

    {
        mesh_zone_ID_type zone = *zoneItr;

        space_output_file << setw(dwidth) << mMesh.getGlobalID(zone);

        space_output_file << setiosflags(ios::scientific);

        vector<double> x_zone = mMesh.zone_position(zone);
        for( vector<double>::iterator i = x_zone.begin();
             i != x_zone.end(); ++i )
        {
            space_output_file << setw(dwidth) << *i;
        }

        double eRadCode = eRad(zone);
        space_output_file << setw(dwidth) << eRadCode;

        double eRadAnalytic = mAnalyticAnswer.Answer(x_zone[0],
                                                     x_zone[1],
                                                     tn );

        space_output_file << setw(dwidth) << eRadAnalytic;

        double fracError = (eRadCode - eRadAnalytic);
        fracError /= (eRadAnalytic + scaleFactor);
        
        space_output_file << setw(dwidth) << fracError;

        space_output_file << endl;

    }
//    end of output
}
template<typename mesh_types, typename mat_types>
double IMC<mesh_types, mat_types>::
computeMaxDeviationFromAnalytic() const
{
//    Need to make sure memory is allocated, etc, before you try to print
    if( !mMaterialDB.isSetUp() )
        cout << "need to call mMaterialDB.setUp() before IMC::computeMaxDeviationFromAnalytic!" << endl;
    if( !mIsSetUp )
        cout << "need to call IMC::setUp() before IMC::computeMaxDeviationFromAnalytic!" << endl;

    double maxLocalError=0.0, maxGlobalError=0.0;
    
    for( ZoneIterator zoneItr = mMesh.PhysicalZoneBegin(); 
         zoneItr != mMesh.PhysicalZoneEnd(); ++zoneItr )

    {
        mesh_zone_ID_type zone = *zoneItr;

        vector<double> x_zone = mMesh.zone_position(zone);

        double eRadCode = eRad(zone);
        double eRadAnalytic = mAnalyticAnswer.Answer(x_zone[0],
                                                     x_zone[1],
                                                     tn );

        maxLocalError =max(maxLocalError, fabs(eRadCode - eRadAnalytic) );        
    }
#ifdef USE_MPI
    int result = MPI_Reduce(&maxLocalError, &maxGlobalError, 1, MPI_DOUBLE, MPI_MAX, 0, MPI_COMM_WORLD);
#else
    maxGlobalError = maxLocalError;
#endif
    
    return maxGlobalError;
//    end of error computation
}

//---------------------------------------------------------------------------//

template<typename mesh_types, typename mat_types>
double IMC<mesh_types, mat_types>::
energyBalance()
{
    return m4Momentum.energyBalance();
}

//---------------------------------------------------------------------------//

//    Number of photons in census should = initial number + number created
//    - number that escaped - number destroyed by Russian Roullette

template<typename mesh_types, typename mat_types>
long long IMC<mesh_types, mat_types>::
photonNumberBalance() const
{
    unsigned long long N_initial = mSourceDB.N_initial();
    unsigned long long N_created = mSourceDB.N_created();
    unsigned long long N_remaining = N_initial + N_created;
    N_remaining += N_split_added;
    N_remaining += N_entered;
    N_remaining -= (N_escaped + N_lost_track + N_RR_destroyed);
    
    const unsigned long long N_in_census = census_photons.size();
    
    return N_in_census - N_remaining;
}

//---------------------------------------------------------------------------//

template<typename mesh_types, typename mat_types>
void IMC<mesh_types, mat_types>::
diagnosticOutput()
{                      
    const unsigned int root = 0;

#ifdef USE_MPI
    MPI_Comm comm = mMesh.getWorldCommunicator();
    MPI_Barrier( comm );
#endif

    std::vector< std::string > ull_tally_names;
    std::vector< unsigned long long > ull_tallies;

    unsigned long long N_initial = mSourceDB.N_initial();
    ull_tally_names.push_back("N_initial");
    ull_tallies.push_back(N_initial);

    unsigned long long N_created = mSourceDB.N_created();
    ull_tally_names.push_back("N_created");
    ull_tallies.push_back(N_created);

    ull_tally_names.push_back("N_entered");
    ull_tallies.push_back(N_entered);
    ull_tally_names.push_back("N_escaped");
    ull_tallies.push_back(N_escaped);
    ull_tally_names.push_back("N_lost_track");
    ull_tallies.push_back(N_lost_track);
    ull_tally_names.push_back("N_RRed_up");
    ull_tallies.push_back(N_RRed_up);
    ull_tally_names.push_back("N_RR_destroyed");
    ull_tallies.push_back(N_RR_destroyed);
    ull_tally_names.push_back("N_split_added");
    ull_tallies.push_back(N_split_added);

    unsigned long long N_remaining = N_initial + N_created;
    N_remaining += N_split_added;
    N_remaining += N_entered;
    N_remaining -= (N_escaped + N_lost_track +  N_RR_destroyed);

    ull_tally_names.push_back("N_remaining");
    ull_tallies.push_back(N_remaining);

    const unsigned long long N_in_census = census_photons.size();
    ull_tally_names.push_back("N_in_census");
    ull_tallies.push_back(N_in_census);
    ull_tally_names.push_back("N_collisions");
    ull_tallies.push_back(N_collisions);
    ull_tally_names.push_back("N_zone_crossings");
    ull_tallies.push_back(N_zone_crossings);
    ull_tally_names.push_back("N_physical_scatters");
    ull_tallies.push_back(N_physical_scatters);

    unsigned long long N_events;
    N_events = N_collisions + N_zone_crossings + N_census + N_RR_destroyed;
    ull_tally_names.push_back("N_events");
    ull_tallies.push_back(N_events);
    ull_tally_names.push_back("N_global_count_checks");
    ull_tallies.push_back(N_global_count_checks);

    printGlobalInfo( ull_tally_names,
                     ull_tallies,
                     root,
                     false );

//    number of scatterings per zone
#ifdef USE_MPI
    unsigned long long local[2] = { N_collisions, N_zone_crossings };
    unsigned long long global[2] = {0,0};
    MPI_Reduce( local,
                global,
                2,
                MPI_UNSIGNED_LONG_LONG,
                MPI_SUM,
                root,
                comm );

    unsigned long long gN_collisions = global[0];
    unsigned long long gN_zone_crossings = global[1];
#else
    unsigned long long gN_collisions = N_collisions;
    unsigned long long gN_zone_crossings = N_zone_crossings;
#endif
    if( root == procID )
    {
       double scat = static_cast<double>(gN_collisions)/static_cast<double>(gN_zone_crossings + 1);
       cout << "\nscatters per zone crossing = " << scat << '\n';
       cout << "mean free path = " << shave( d_total/static_cast<double>(gN_collisions+1) );
       cout << '\n' << '\n';
    }
    
#ifdef USE_MPI
    MPI_Barrier( comm );
#endif

    // Strict per-processor test
    if( N_remaining != N_in_census )
    {
        cout << "WARNING - DIAGNOSTIC PROBLEM!" << '\n';
        cout << "Problem with photon number accounting!" << '\n';
        cout << "Number in census = census_photons.size()" << '\n';
        cout << "does not equal the number in - number lost!" << '\n';
        
        double E_actually_in_census = 0.0;
        typename list<photon_type>::const_iterator census_photonItr;
        for( census_photonItr = census_photons.begin();
             census_photonItr != census_photons.end();
             ++census_photonItr )
        {
            E_actually_in_census += census_photonItr->Energy;
        }
        cout << "Energy actually in census photons = " << E_actually_in_census << '\n';
    } 
    if( root == procID )
    {
      cout << '\n';
    }

    //    Info on IMC and total energy balance
    if( root == procID )
    {
      cout << "IMC Energy accounting:\n\n";
    }

    m4Momentum.diagnosticOutput();
    if( root == procID )
    {
      cout << "\n\n";
    }

    // Should do this in parallel nicely too.
    if( nProcs == 1 )
    {
    //   Info on source
        cout << "Source Energy accounting:" << '\n';
        mSourceDB.diagnosticOutput();
    }

}

//---------------------------------------------------------------------------//

template<typename mesh_types, typename mat_types>
void IMC<mesh_types, mat_types>::
threadedDiagnosticOutput()
{                      

    const unsigned int root = 0;

    int tid =0;
#ifdef USE_OPENMP
    tid = omp_get_thread_num();
#endif

#ifdef USE_MPI
    MPI_Comm comm = mMesh.getWorldCommunicator();
#pragma omp master
    {        
        MPI_Barrier( comm );
    }
#endif

    
    std::vector< std::string > ull_tally_names;
    std::vector< unsigned long long > ull_tallies;

    unsigned long long N_initial = mSourceDB.N_initial();
    ull_tally_names.push_back("N_initial");
    ull_tallies.push_back(N_initial);

    unsigned long long N_created = mSourceDB.N_created();
    ull_tally_names.push_back("N_created");
    ull_tallies.push_back(N_created);

    ull_tally_names.push_back("N_entered");
    ull_tallies.push_back(N_entered);
    ull_tally_names.push_back("N_escaped");
    ull_tallies.push_back(N_escaped);
    ull_tally_names.push_back("N_lost_track");
    ull_tallies.push_back(N_lost_track);
    ull_tally_names.push_back("N_RRed_up");
    ull_tallies.push_back(N_RRed_up);
    ull_tally_names.push_back("N_RR_destroyed");
    ull_tallies.push_back(N_RR_destroyed);
    ull_tally_names.push_back("N_split_added");
    ull_tallies.push_back(N_split_added);

    unsigned long long N_remaining = N_initial + N_created;
    N_remaining += N_split_added;
    N_remaining += N_entered;
    N_remaining -= (N_escaped + N_lost_track +  N_RR_destroyed);

    ull_tally_names.push_back("N_remaining");
    ull_tallies.push_back(N_remaining);

    const unsigned long long N_in_census = census_photons.size();
    ull_tally_names.push_back("N_in_census");
    ull_tallies.push_back(N_in_census);
    ull_tally_names.push_back("N_collisions");
    ull_tallies.push_back(N_collisions);
    ull_tally_names.push_back("N_zone_crossings");
    ull_tallies.push_back(N_zone_crossings);
    ull_tally_names.push_back("N_physical_scatters");
    ull_tallies.push_back(N_physical_scatters);

    unsigned long long N_events;
    N_events = N_collisions + N_zone_crossings + N_census + N_RR_destroyed;
    ull_tally_names.push_back("N_events");
    ull_tallies.push_back(N_events);
    ull_tally_names.push_back("N_global_count_checks");
    ull_tallies.push_back(N_global_count_checks);
    

    //
    // next sum up the ull_tallies using the static 
    // mThreadedDiagnosticData to buffer the data
    //

    //
    // a single thread needs to resize the sum buffer to the 
    // correct size.
    //
    std::vector<unsigned long long> &theULLThreadedDiagnosticData = mULLThreadedDiagnosticData();
    std::vector<double> &theDoubleThreadedDiagnosticData = mDoubleThreadedDiagnosticData();
#pragma omp single
    {
        theDoubleThreadedDiagnosticData.resize(1);
        theDoubleThreadedDiagnosticData[0]= 0.0;
        
        theULLThreadedDiagnosticData.resize(ull_tallies.size());
        std::transform(theULLThreadedDiagnosticData.begin(),theULLThreadedDiagnosticData.end(),
                       theULLThreadedDiagnosticData.begin(),Zero_functor<unsigned long long>());
    }
    
    //
    // now do the vector sum reduction

#pragma omp atomic
    theDoubleThreadedDiagnosticData[0] += d_total;

    for(size_t i=0;i<ull_tallies.size(); i++)
    {
#pragma omp atomic 
        theULLThreadedDiagnosticData[i] += ull_tallies[i];
    }
    
    // pause here until all threads are reduced
#pragma omp barrier 
    double d_total_sum;
    
#pragma omp master
    {
        // copy into local ull_tallies vector
        std::copy(theULLThreadedDiagnosticData.begin(),theULLThreadedDiagnosticData.end(), ull_tallies.begin() );

        d_total_sum = theDoubleThreadedDiagnosticData[0];
    
        printGlobalInfoPrintf( ull_tally_names,
                               ull_tallies, 
                               root,
                               false );
    }
    
#pragma omp barrier

#pragma omp master
    {
#ifdef USE_MPI
        unsigned long long local[2] = { ull_tallies[10], ull_tallies[11] };
        unsigned long long global[2] = {0,0};
        MPI_Reduce( local,
                    global,
                    2,
                    MPI_UNSIGNED_LONG_LONG,
                    MPI_SUM,
                    root,
                    comm );
        unsigned long long gN_collisions = global[0];
        unsigned long long gN_zone_crossings = global[1];
#else
        unsigned long long gN_collisions = ull_tallies[10];
        unsigned long long gN_zone_crossings = ull_tallies[11];
#endif

        if( root == procID  )
        {
            double scat = static_cast<double>(gN_collisions)/static_cast<double>(gN_zone_crossings + 1);
            printf( "\nscatters per zone crossing = %f\n", scat);
            printf( "mean free path = %f\n" , shave( d_total_sum/static_cast<double>(gN_collisions+1) ) );
            printf("\n\n");
        }
    }//end of omp master section

    // Strict per-processor test
    // get the thread summed values for N_remaining and N_in_census
    unsigned long long N_remaining_threadSum = theULLThreadedDiagnosticData[8];
    unsigned long long N_in_census_threadSum = theULLThreadedDiagnosticData[9];
    
    if( N_remaining_threadSum != N_in_census_threadSum )
    {
        printf( "mpi: %d, tid: %d, WARNING - DIAGNOSTIC PROBLEM! Problem with photon number accounting!\n", procID, tid);
        printf( "mpi: %d, tid: %d, Number in census = census_photons.size() does not equal the number in - number lost!\n", procID, tid);
        printf( "mpi: %d, tid: %d, N_remaining_threadSum=%llu, N_in_census_threadSum=%llu\n",
                procID, tid, N_remaining_threadSum, N_in_census_threadSum);
        
        double E_actually_in_census = 0.0;
        typename list<photon_type>::const_iterator census_photonItr;
        for( census_photonItr = census_photons.begin();
             census_photonItr != census_photons.end();
             ++census_photonItr )
        {
            E_actually_in_census += census_photonItr->Energy;
        }
        printf( "tid: %d, Energy actually in census photons = %f\n", tid, E_actually_in_census);
    } 
    if(tid==0 && root == procID  )
    {
        printf("\n");
    }

    //    Info on IMC and total energy balance
    if(tid == 0 && root == procID  )
    {
        printf("IMC Energy accounting:\n\n");
    }

    m4Momentum.threadedDiagnosticOutput();
    if( tid == 0 && root == procID  )
    {
        printf("\n\n");
        printf("Source Energy accounting:\n");
    }

    // Should do this in parallel nicely too.
    //   Info on source
    mSourceDB.threadedDiagnosticOutput();
}


//---------------------------------------------------------------------------//

//    Set values of local variables that hold info related to the photon's zone.
//    These are set when the advancePhoton routine is called, and changed
//    only if the photon enters another zone, which happens when
//    it enters a new zone

template<typename mesh_types, typename mat_types>
void IMC<mesh_types, mat_types>::
setOpacities( photon_type& photon,
              double& sigmaAbsorb, double& sigmaScat )
{   
// Make copies of lab frame quantities to shift to fluid frame.
    Vector3d Omega = photon.Omega;

    sigmaAbsorb = mOpacityDB.absorption_opacity( photon.zone );

    sigmaScat = mOpacityDB.scattering_opacity( photon.zone );

    ENSURE( sigmaAbsorb >= 0.0 );
    ENSURE( sigmaScat >= 0.0 );
}

//---------------------------------------------------------------------------//

//    Do stuff necessary to handle photon collisions (i.e. scattering)
//    This includes both real scattering and IMC scatters
//    (absorption/reemmision)

template<typename mesh_types, typename mat_types>
void IMC<mesh_types, mat_types>::
processCollision( photon_type& photon,
                  double& sigmaScat, double& sigmaAbsorb )
{
    REQUIRE( sigmaScat >= 0.0 );
    REQUIRE( sigmaAbsorb >= 0.0 );
    REQUIRE( std::fabs(photon.Omega.magnitude() - 1.) < 1.e-6 );

    ++N_collisions;

    double cosScatteringAngle;
    
    mOpacityDB.physical_scattering( photon.zone,
                                    photon.get_rng(),
                                    cosScatteringAngle );
            
    //    Get phi relative to current direction
    double phi = 3.1415926535897931*(2.0*photon.random_number() - 1.0);
    
    //    now rotate (cosScatteringAngle, phi) to get the new trajectory of the
    //    photon in the problem (x,y,z) coordinates
    photon.Omega = rotate_angles( cosScatteringAngle, phi, photon.Omega);
    
    ++N_physical_scatters;
    
    sigmaAbsorb = mOpacityDB.absorption_opacity( photon.zone );

    sigmaScat = mOpacityDB.scattering_opacity( photon.zone );

    ENSURE( sigmaAbsorb >= 0.0 );
    ENSURE( sigmaScat >= 0.0 );
    ENSURE( photon.consistent() );
}

//---------------------------------------------------------------------------//
}    //    namespace IMC_namespace

//---------------------------------------------------------------------------//
//                              end of IMC.cc
//---------------------------------------------------------------------------//




