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


//---------------------------------------------------------------------------//

#ifdef USE_MPI
#include <mpi.h>
#endif

#include "IMC_4Momentum.hh"
#include "shave.hh"
#include "setDomainInformation.hh"
#include "sumOverDomains.hh"
#include "printGlobalInfo.hh"
#include <iomanip>

#ifdef USE_OPENMP
#include <omp.h>
#include "OpenMP_ReductionFunctors.hh"
#endif

using namespace std;

namespace IMC_namespace
{


//---------------------------------------------------------------------------//

//    ctor

template<typename mesh_types, typename mat_types>
IMC_4Momentum<mesh_types, mat_types>::
IMC_4Momentum( const Mesh_type& Mesh_in,
               const Material_data_base_type& Material_DB_in,
               Source_data_base<mesh_types, mat_types>& Source_DB_in,
               double RR_threshold_in )
    : Mesh( Mesh_in ),
      Material_DB( Material_DB_in ),
      Source_DB( Source_DB_in ),
      RR_threshold( RR_threshold_in ),
      E_absorbed_total( 0.0 ),
      E_out( 0.0 ),
      E_census_RR_created( 0.0 ), E_census_RR_destroyed( 0.0 ),
      E_small_photon_RR_created( 0.0 ), E_small_photon_RR_destroyed( 0.0 ),
      E_census_RR_created_zone( Mesh_in ),
      E_census_RR_destroyed_zone( Mesh_in ),
      E_small_photon_RR_created_zone( Mesh_in ),
      E_small_photon_RR_destroyed_zone( Mesh_in ),
      N_census_RR_created_zone( Mesh_in ),
      N_census_RR_destroyed_zone( Mesh_in ),
      N_small_photon_RR_created_zone( Mesh_in ),
      N_small_photon_RR_destroyed_zone( Mesh_in ),
      EINT64_after_census_RR( Mesh_in ),
      N_after_census_RR( Mesh_in ),
      E_after_census_RR( Mesh_in ),
      E_census_in_zones_INT64( Mesh_in ),
      E_census_in_zones( Mesh_in ),
      E_source( 0.0 ), E_census( 0.0 ), E_census_initial(0.0),
      E_source_total( 0.0 ), E_census_total( 0.0 ), E_total( 0.0 ),
      E_rad_INT64( Mesh_in ),
      E_rad( Mesh_in ),
      E_absorbed( Mesh_in ),
      E_absorbed_INT64( Mesh_in ),
      scale_to_INT64( Mesh ),
      Two_to_64( 1.8446744e19 ),
      Two_to_63( 9.223372e18 )
{
#ifdef USE_OPENMP
#pragma omp single
    {
        std::vector<double> &diagBuff = mThreadedDiagnosticData();
        std::vector<unsigned long long >&diagUllBuff = mThreadedUllDiagnosticData();

    }
#endif

    if( RR_threshold <= 0.0 )
    {
        cout << "in IMC_4Momentum ctor:" << endl;
        cout << "RR_threshold = " << RR_threshold << endl;
        cout << "It has to be > 0.0!" << endl;
    }

    if( RR_threshold > 1.0 )
    {
        cout << "in IMC_4Momentum ctor:" << endl;
        cout << "RR_threshold = " << RR_threshold << endl;
        cout << "It has to be <= 1.0!" << endl;
    }

    ASSERT( RR_threshold > 0.0 );
    ASSERT( RR_threshold <= 1.0 );

    setDomainInformation( Mesh,
                          domain_decomposed,
                          N_domains,
                          domainID,
                          mesh_replicated,
                          N_replicas,
                          replicaID,
                          nProcs,
                          procID,
                          mIsOpenMPCoarseThreaded,
                          numOmpThreads,
                          ompThreadID);
}

#ifdef USE_OPENMP
template<typename mesh_types, typename mat_types>
std::vector<double>& IMC_4Momentum<mesh_types, mat_types>:: 
mThreadedDiagnosticData()
{
    static std::vector<double> sThreadedDiagnosticData(1);
    return sThreadedDiagnosticData;
}


template<typename mesh_types, typename mat_types>
std::vector<unsigned long long>& IMC_4Momentum<mesh_types, mat_types>:: 
mThreadedUllDiagnosticData()
{
    static std::vector<unsigned long long> sThreadedUllDiagnosticData(1);
    return sThreadedUllDiagnosticData;
}
#endif

//---------------------------------------------------------------------------//

//    called in IMC ctor after initial photon population is made

template<typename mesh_types, typename mat_types>
void IMC_4Momentum<mesh_types, mat_types>:: 
set_initial_E_census( double E_initial_photons,
                      const zcf_double& E_initial_photons_zone )
{
    ASSERT( E_initial_photons >= 0.0 );
    E_census = 0.0;
    E_census_initial = 0.0;
    
    for( ZoneIterator zoneItr = Mesh.PhysicalZoneBegin();
         zoneItr != Mesh.PhysicalZoneEnd(); ++zoneItr )
    {
        E_census_in_zones[*zoneItr] = 0.0;

        ASSERT( E_census_in_zones[*zoneItr] >= 0.0 );
    }

#ifdef USE_OPENMP
    if( mIsOpenMPCoarseThreaded )
    {
        E_census_in_zones.threadSum();
    }
#endif

    if( mesh_replicated )
    {
        E_census_in_zones.replicaSum();
    }

    E_census_initial = 0.0;

    ASSERT( ( E_initial_photons > 0.0 && E_census > 0.0) ||
            ( E_initial_photons == 0.0 && E_census == 0.0) );
}

//---------------------------------------------------------------------------//

//    stuff that has to be done once each time step before we call advance

//    INT64 variables cannot be cummulative, because a different conversion
//    factor is used to convert them each time step. So all INT64 variables
//    should be set to zero in this routine.

template<typename mesh_types, typename mat_types>
void IMC_4Momentum<mesh_types, mat_types>::
setUp( double dt )
{
    E_absorbed = 0.0;
    E_absorbed_INT64 = 0;
    E_rad = 0.0;
    E_rad_INT64 = 0;
    E_census_in_zones_INT64 = 0;
}

//---------------------------------------------------------------------------//

//    Want to add energy absorbed into 64 bit int, so scale
//    photon energy into 2**63.

template<typename mesh_types, typename mat_types>
void IMC_4Momentum<mesh_types, mat_types>::
set_INT64_conversion()
{
    for( ZoneIterator zoneItr = Mesh.PhysicalZoneBegin(); 
         zoneItr != Mesh.PhysicalZoneEnd();
         ++zoneItr )
    {
    //    I wanted to make a zone by zone conversion to limit roundoff, but it was hard to
    //    come up with an estimate that didn't overflow. So I am returning to the old
    //    value, using the total source and census energy
        scale_to_INT64[*zoneItr] = Two_to_63/(E_source_total + E_census_total);
    }    //    end loop over zones
}

//---------------------------------------------------------------------------//

//    Get values of source and census energies, to calculate desired energy
//    of each photon. The MPI call to get source and census energy summed
//    over all procs is here.

template<typename mesh_types, typename mat_types>
double IMC_4Momentum<mesh_types, mat_types>::
get_E_census_desired( double tn, double dt,
                      const zcf_UINT64& largest_state_census,
                      const zcf_double& E_largest_state_census,
                      unsigned long long N_cycle_photons,
                      unsigned long long N_census_no_RR,
                      double E_census_no_RR )
{
//    First, figure out how much energy sources radiate this cycle
    E_source = Source_DB.E_photons_cycle( tn, dt );

#ifdef USE_OPENMP
    std::vector<double> &sharedDoubleVec = mThreadedDiagnosticData();
    sumOverDomainsThreaded( Mesh, E_source, E_source_total, sharedDoubleVec );
    sumOverDomainsThreaded( Mesh, E_census, E_census_total, sharedDoubleVec );
#else
    sumOverDomains( Mesh, E_source, E_source_total );
    sumOverDomains( Mesh, E_census, E_census_total );
#endif

//    NOTE - can we replace this with a summation over ints which will be
//    reproducible?  These numbers won't be added reproducibly over different
//    numbers of processors by the MPI_Allreduce in sumOverDomains, so shave
//    off lower order bits (which are going to be different because of
//    roundoff) so that the numbers are reproducible
    E_source_total = shave( E_source_total );
    E_census_total = shave( E_census_total ); 

    ASSERT( E_source_total >= 0.0 );
    ASSERT( E_census_total >= 0.0 );

    E_total = E_source_total + E_census_total;
    ASSERT( E_total >= 0.0 );

//    energy we desire for photons that are participating in RR comb, so we
//    base it on numbers and energies from which we subtract values for the
//    one photon per zone we are keeping out of RR
    double E_census_do_RR = E_census_total - E_census_no_RR;
    ASSERT( E_census_do_RR >= 0.0 );

    unsigned long long N_census_desired = 0;
    if( N_cycle_photons > N_census_no_RR )
    {
        double ratio = E_census_do_RR/(E_census_do_RR + E_source_total);
        ASSERT( ratio >= 0.0 );
        ASSERT( ratio <= 1.0 );

        N_census_desired = static_cast<unsigned long long>((N_cycle_photons - N_census_no_RR)*ratio);
    }

    ASSERT( N_census_desired <= 3 * N_cycle_photons );

    double E_census_desired;
    if( N_census_desired > 0 )
    {
        E_census_desired = E_census_do_RR/N_census_desired;
    }
    else
    {
    //    if N_census_desired = 0, we will compare census photons to a huge
    //    desired value so that all will be killed
        E_census_desired = 1.0e100;
    }

    ASSERT( E_census_desired >= 0.0 ||
            (E_source_total == 0.0 && E_census_do_RR == 0.0) );

//    Want to add energy absorbed into 64 bit int, so scale total possible
//    photon energy into 2**63, leaving a factor of 2 safety factor
    set_INT64_conversion();

    return E_census_desired;
}

//---------------------------------------------------------------------------//

//    Enforce energy conservation by renormalizing the energy to make up for
//    the change though RR process.

template<typename mesh_types, typename mat_types>
void IMC_4Momentum<mesh_types, mat_types>::
RR_renormalize_census_energy( list<photon_type>& census_photons,
                              unsigned long long& N_RR_destroyed )
{
#ifdef USE_OPENMP
    if( mIsOpenMPCoarseThreaded )
    {
        EINT64_after_census_RR.threadSum();
        N_after_census_RR.threadSum();
    }
#endif
    
    if( mesh_replicated )
    {
        EINT64_after_census_RR.replicaSum();
        N_after_census_RR.replicaSum();
    }

//    get E after RR from the INT64 value
    for( ZoneIterator zoneItr = Mesh.PhysicalZoneBegin(); 
         zoneItr != Mesh.PhysicalZoneEnd();
         ++zoneItr )
    {
        E_after_census_RR[*zoneItr] = 
            EINT64_to_E( EINT64_after_census_RR[*zoneItr], *zoneItr );
    }

//    loop over photons and change energy so that it is conserved zone by zone
//    If there are very small photons, the INT64 conversion may make
//    E_after_census_RR could be 0.0 for a zone even though there are photons
//    in it. In that case, kill the photon.
    typedef typename list<photon_type>::iterator photonIterType;
    for( photonIterType photonItr = census_photons.begin();
         photonItr != census_photons.end(); )
    {
        if( E_after_census_RR[photonItr->zone] != 0.0 )
        {
            ASSERT( E_after_census_RR[photonItr->zone] > 0.0 );
            ASSERT( N_after_census_RR[photonItr->zone] > 0 );
            ASSERT( E_census_in_zones[photonItr->zone] > 0.0 );

            double ratio = E_census_in_zones[photonItr->zone];
            ratio /= E_after_census_RR[photonItr->zone];
            ASSERT( ratio > 0.0 );

            photonItr->Energy *= ratio;
            ASSERT( photonItr->Energy > 0. );

            ++photonItr;
        }
        else  //  if energy in zone = 0, kill photon
        {
            photonItr = census_photons.erase( photonItr );
            N_RR_destroyed += 1;
        }
    }
}

//---------------------------------------------------------------------------//

//    Calculate the number of census photons we want in a zone, and their
//    energy.

template <typename mesh_types, typename mat_types>
void IMC_4Momentum<mesh_types, mat_types>::
calculate_zonal_census_info( const mesh_zone_ID_type& zone,
                             double E_census_desired,
                             const zcf_double& E_largest_state_census,
                             double& E_census_zone,
                             unsigned long long& N_census_zone ) const
{
//    if there is a photon in the zone, there should be some energy in it,
//    but there might not be due to roundoff
    EXPENSIVE_ASSERT( E_census_in_zones[zone] >= 0.0 );
    EXPENSIVE_ASSERT( E_census_desired > 0.0 );
        
//    total number we want in census in this zone, besides the one we are not
//    sending through RR_comb.
    double E_RR = E_census_in_zones[zone] - E_largest_state_census[zone];

//    Because E_census_in_zones is shaved and E_largest_state_census isn't
//    we could have E_RR <0 if there is one photon in the zone
    if( E_RR < 0.0 )
        E_RR = 0.0;

    N_census_zone = 
        static_cast<unsigned long long>( E_RR/E_census_desired + 0.5 );

//    Energy we want census photons in this zone to have
    if( N_census_zone > 0 )
        E_census_zone = E_census_in_zones[zone]/N_census_zone;
    else
        E_census_zone = 0.0;

    EXPENSIVE_ASSERT( E_census_zone >= 0.0 );
}

//---------------------------------------------------------------------------//

//    Pick one photon in each zone that will not be subject to RR comb. This
//    ensures that there will be at least one census photon in each zone
//    after RR comb is completed. Also get energy of that photon in the zone,
//    and 

template<typename mesh_types, typename mat_types>
void IMC_4Momentum<mesh_types, mat_types>::
pick_census_photons_to_keep( const list<photon_type>& census_photons,
                             zcf_UINT64& largest_state_census,
                             zcf_double& E_largest_state_census,
                             unsigned long long& N_census_no_RR,
                             double& E_census_no_RR )
{
    largest_state_census = 0;
    E_largest_state_census = 0.0;
    zcf_UINT64 FoundMaxPhotonCount( Mesh );

    //    pick photon by comparing initial rng state - this is essentially
    //    picking one at random
    typedef typename list<photon_type>::const_iterator photonIterType;
    for( photonIterType photonItr = census_photons.begin();
         photonItr != census_photons.end(); ++photonItr )
    {
        if( photonItr->initial_state() > largest_state_census[photonItr->zone] )
        {
            largest_state_census[photonItr->zone] = photonItr->initial_state();
        }
    }
  
    //    now we get photon with largest state on all threads and 
    //procs with copy of this mesh
#ifdef USE_OPENMP
    if( mIsOpenMPCoarseThreaded )
        largest_state_census.threadMax();
#endif

    if( mesh_replicated )
    {
        largest_state_census.replicaMax();
    }

//    Need to walk the list again to find the energy of the one we picked, if
//    it is on this proccess. If it is not on this processor, the E value
//    will be zero. This is so we can do an MPI_MAX on the values without
//    needing to check the value of largest_state on each processor. There
//    might be a better way to do this with MPI commands. Then we could avoid
//    walking over the photon list twice
    for( photonIterType photonItr = census_photons.begin();
         photonItr != census_photons.end(); ++photonItr )
    {
        if( photonItr->initial_state() == largest_state_census[photonItr->zone] )
        {
            FoundMaxPhotonCount[photonItr->zone]++;
            
            if( FoundMaxPhotonCount[photonItr->zone] > 1 )
            {
                // whoa! there are >1 photons in this zone with the same initial_state!
                ASSERT( FoundMaxPhotonCount[photonItr->zone] < 2 );
            }
            
            if(  E_largest_state_census[photonItr->zone] != 0 )
            {
                unsigned long long thisZonesMaxPhotonCount = FoundMaxPhotonCount[photonItr->zone];
                photon_type thisPhoton = *photonItr;
                ASSERT( E_largest_state_census[photonItr->zone] == 0.0 );
            }
            
            ASSERT( photonItr->Energy > 0. );
            E_largest_state_census[photonItr->zone] = photonItr->Energy;
        }
     }
   
//    now get energy of the photon we are saving across all OpenMP threads
#ifdef USE_OPENMP
    if( mIsOpenMPCoarseThreaded )
    {
        E_largest_state_census.threadMax();
    }
#endif    
//    now get energy of the photon we are saving across all procs
    E_largest_state_census.replicaMax();

//    now get total number you saved and the total energy
    N_census_no_RR = 0;
    E_census_no_RR = 0.0;
    for( ZoneIterator zoneItr = Mesh.PhysicalZoneBegin();
         zoneItr != Mesh.PhysicalZoneEnd(); ++zoneItr )
    {
        if( largest_state_census[*zoneItr] > 0 )
            N_census_no_RR += 1;

        E_census_no_RR += E_largest_state_census[*zoneItr];

    //    Because census energy is calculated with an INT64 value,
    //    E_census_in_zones could get rounded to 0 if small photons reach
    //    census in a zone. So E_census_in_zones could be 0 even though there
    //    are census photons in the zone.  So we will set E_census_in_zones =
    //    E_largest_state_census if it is less than that
        if( E_census_in_zones[*zoneItr] < E_largest_state_census[*zoneItr] )
        {
            E_census_in_zones[*zoneItr] = E_largest_state_census[*zoneItr];
        }
    }

//    This vould be 0 because of roundoff
    if( E_census == 0.0 )
        E_census = E_census_no_RR;

//    add these up over all domains
    unsigned long long N_total;
#ifdef USE_OPENMP
    std::vector<unsigned long long> &sharedUllVec = mThreadedUllDiagnosticData();
    sumOverDomainsThreaded( Mesh, N_census_no_RR, N_total, sharedUllVec );
#else
    sumOverDomains( Mesh, N_census_no_RR, N_total );
#endif
    N_census_no_RR = N_total;

    double E_total;
#ifdef USE_OPENMP
    std::vector<double> &sharedDoubleVec = mThreadedDiagnosticData();
    sumOverDomainsThreaded( Mesh, E_census_no_RR, E_total, sharedDoubleVec );
#else
    sumOverDomains( Mesh, E_census_no_RR, E_total );
#endif

    E_census_no_RR = E_total;

//    Need to shave the energy. It can't be added as an int, because the INT64
//    conversion factor hasn't been calculated yet.
    E_census_no_RR = shave( E_census_no_RR );
}
    
//---------------------------------------------------------------------------//

//    Reduce the number of photons run by stochastic combing.  Based on
//    energy of photons, we delete them or raise their energy. This function
//    is called before the source photons have been created, so it combs only
//    the census photons. It is called in the function advance().

template<typename mesh_types, typename mat_types>
void IMC_4Momentum<mesh_types, mat_types>::
RR_comb_census( list<photon_type>& census_photons, 
                unsigned long long N_cycle_photons,
                unsigned long long& N_RR_split, 
                unsigned long long& N_split_added,
                unsigned long long& N_RRed_up, 
                unsigned long long& N_RR_destroyed,
                double tn, double dt )
{
    ASSERT( N_cycle_photons > 0 );
    ASSERT( mesh_replicated ||
            ( (!census_photons.empty() && E_census > 0.0) ||
              (census_photons.empty() && E_census == 0.0) ) );

    
//    loop over photons and pick one census photon to keep at random
    zcf_UINT64 largest_state_census( Mesh );
    zcf_double E_largest_state_census( Mesh );
    unsigned long long N_census_no_RR = 0;
    double E_census_no_RR = 0.0;

    pick_census_photons_to_keep( census_photons,
                                 largest_state_census,
                                 E_largest_state_census,
                                 N_census_no_RR,
                                 E_census_no_RR );

//    get desired energy of photons that survive RR comb
    double E_census_desired = get_E_census_desired( tn,  dt,
                                                    largest_state_census,
                                                    E_largest_state_census,
                                                    N_cycle_photons,
                                                    N_census_no_RR,
                                                    E_census_no_RR );

    unsigned long long N_killed = 0;    //    number killed from census list
    N_RR_split = 0;    //    Number of photons split this cycle

   EINT64_after_census_RR = 0;
   N_after_census_RR = 0;

//    new_photon, to be added to list if we split census photons
   photon_type new_photon;
   
// loop over photons, terminating some and promoting energy of others
    typedef typename list<photon_type>::iterator photonIterType;
    for( photonIterType photonItr = census_photons.begin();
         photonItr != census_photons.end(); )
    {
    //    always keep one selected photon in each zone
        if( photonItr->initial_state() == largest_state_census[photonItr->zone] )
        {
            EINT64_after_census_RR[photonItr->zone] += 
                E_to_EINT64( photonItr->Energy, photonItr->zone );
            N_after_census_RR[photonItr->zone] += 1;
            ++photonItr;
            continue;
        }
       
    //    number of photons we want from this zone, and average energy
    //    we want them to have
        double E_census_zone;
        unsigned long long N_census_zone;
        calculate_zonal_census_info( photonItr->zone,
                                     E_census_desired,
                                     E_largest_state_census,
                                     E_census_zone,
                                     N_census_zone );

    //    Number we want to turn the current photon into; if only a very
    //    small photon is present in a zone, E_census_zone could be 0. In
    //    that case, we set N_new = 0 and kill the photon.
        unsigned long long N_new;
        if( E_census_zone > 0.0 )
        {
            N_new = static_cast<unsigned long long>( photonItr->Energy/E_census_zone +
                                               photonItr->random_number() );
            
            if (N_new == 0)    // photon dies
            {
                RR_comb_destroy( *photonItr );
                
                ++N_RR_destroyed;
                ++N_killed;
                photonItr = census_photons.erase( photonItr );
            }
            else
            {
            // change existing photon energy to desired value
                double deltaE = E_census_zone - photonItr->Energy;
                RR_comb_create( deltaE, *photonItr );
                
                ++N_RRed_up;
                photonItr->Energy = E_census_zone;
                
                ASSERT( photonItr->Energy > 0. );
                EINT64_after_census_RR[photonItr->zone] += 
                    E_to_EINT64( photonItr->Energy, photonItr->zone );
                N_after_census_RR[photonItr->zone] += 1;
                
            //    Add N_new-1 copies of this photon. Add to front of list
            //    so the new ones won't be RRed again
                for( unsigned long long i = 1 ; i <= N_new-1 ; ++i )
                {
                    new_photon = *photonItr;
                    new_photon.set_state( rng_tools::hash_state( photonItr->state() + i ) );
                    
                    RR_comb_create( new_photon.Energy, *photonItr );
                    
                    census_photons.push_front( new_photon );
                    ++N_split_added;
                    
                    EINT64_after_census_RR[new_photon.zone] += 
                        E_to_EINT64( new_photon.Energy, new_photon.zone );
                    N_after_census_RR[new_photon.zone] += 1;
                }
                
                ++photonItr;
            }
            
            if (N_new > 1)
               ++N_RR_split;
        }
        else    //    E_census_zone == 0 - destroy this photon
        {
            ASSERT( N_census_zone == 0 );

            RR_comb_destroy( *photonItr);
                
            ++N_RR_destroyed;
            ++N_killed;
            photonItr = census_photons.erase( photonItr );
        }
    }    //    end of loop over photons
        
    //    Now enforce energy conservation by renormalizing the energy to make up
    //    for the change though RR process.
        RR_renormalize_census_energy( census_photons, N_RR_destroyed );
}

//---------------------------------------------------------------------------//

//    If weight is too small, increase it or remove photon.
//    Energy_RR = RR_threshold*Energy of photon when it was
//    pulled from census.

template<typename mesh_types, typename mat_types>
void IMC_4Momentum<mesh_types, mat_types>::
Russian_Roulette( photon_type& photon,
                  bool& terminated,
                  unsigned long long& N_RRed_up, 
                  unsigned long long& N_RR_destroyed )
{
    REQUIRE( !terminated );
    REQUIRE( photon.Energy >= 0.0 );
    REQUIRE( photon.Energy0 >= 0.0 );

    if (photon.Energy0 == 0. || photon.Energy == 0. )
    {
       terminated = true;
       ++N_RR_destroyed;
       return;
    }

    double Energy_RR = photon.Energy0*RR_threshold;
    
//    Floor Energy_RR by minimum energy increment (from ull conversion) in zone.
//    if (Energy_RR < EINT64_to_E(1, photon.zone))
//       Energy_RR = 1.00001*EINT64_to_E(1, photon.zone);
    
    if( photon.Energy < Energy_RR )    //    if Energy is low enough
    {
        double E_ratio = photon.Energy/Energy_RR;
        double r = photon.random_number();

        if( r > E_ratio )    //  photon dies
        {
            terminated = true;

            E_small_photon_RR_destroyed += photon.Energy;
            E_small_photon_RR_destroyed_zone[photon.zone] += photon.Energy;
            ++N_small_photon_RR_destroyed_zone[photon.zone];

            ++N_RR_destroyed;
        }
        else    //  energy promoted back to Energy_RR
        {
            double deltaE = Energy_RR - photon.Energy;

            E_small_photon_RR_created += deltaE;
            E_small_photon_RR_created_zone[photon.zone] += deltaE;
            ++N_small_photon_RR_created_zone[photon.zone];
            
            ++N_RRed_up;

            photon.Energy = Energy_RR;

            ASSERT( !terminated );
        }
    }

    if ( photon.Energy < 1.e-100 )
    {
       terminated = true;
    }

    ENSURE( photon.consistent() || terminated );
    ENSURE( photon.Energy > 0.0 || (photon.Energy == 0.0 && terminated) );
    //EXPENSIVE_ASSERT( E_to_EINT64(photon.Energy,photon.zone) > 0 || terminated );
}

//---------------------------------------------------------------------------//

//    Tally radiation energy density.

template<typename mesh_types, typename mat_types>
void IMC_4Momentum<mesh_types, mat_types>::
tallyRadiationEnergy( double sigma_absorb, double d_path,
                      const photon_type& photon, double dt )
{    
    REQUIRE( sigma_absorb >= 0.0);
    REQUIRE( d_path > -Mesh.getTrackingTolerance() );
    REQUIRE( photon.Energy > 0.0 );
//    loss_rate could be < 0 if d_path < 0 from roundoff
//    Even if d_path = a very small negative number, sigma_a could be large
//    enough to cause loss_rate to be large and positive, causing a problem.
    if( d_path < 0.0 )
        return;

    double loss_rate = sigma_absorb*d_path;
    EXPENSIVE_ASSERT( loss_rate >= 0.0 );

//    photon energy at begining of d_path
    double E0 = photon.Energy;
    
    double E_tnp12;
    if( loss_rate > 0.01 )
    {
        E_tnp12 = E0*( 1.0 - exp(-loss_rate) )/(sigma_absorb*dt);
    }
    else    //    E_tnp12 -> 0/0 as sigma -> 0, so we need small value approx
    {
        E_tnp12 = E0*(1.0 - 0.5*loss_rate)*d_path/dt;
    }
    ASSERT( E_tnp12 >= 0.0 );
   
//    tally into int array so order of additions doesn't matter
    unsigned long long E_tnp12_INT64 = E_to_EINT64( E_tnp12, photon.zone );
    ASSERT( E_rad_INT64[photon.zone] + E_tnp12_INT64 < Two_to_64 );
    E_rad_INT64[photon.zone] += E_tnp12_INT64;
}

//---------------------------------------------------------------------------//

//    do exponential attenuation; decide if photon should be
//    killed or brought back to initial weight (Russian Roulletting)

template<typename mesh_types, typename mat_types>
void IMC_4Momentum<mesh_types, mat_types>::
attenuate( double pathLength,
           double sigmaAbsorb,
           photon_type& photon,
           const Vector3d& initialOmega )
{
    REQUIRE( photon.Energy > 0.0 );
    REQUIRE( sigmaAbsorb < 1.e12 );

    double opticalDepth = pathLength*sigmaAbsorb;

//    opticalDepth could be < 0 if pathLength < 0 from roundoff
//    Even if distance is a very small negative number, sigmaAbsorb could be large
//    enough to cause opticalDepth to be large and positive, causing a problem.
    if( opticalDepth < 0.0 ) return;

    double initialEnergy = photon.Energy;
    photon.Energy *= exp(-opticalDepth);

//    old - new rather than new - old to make value positive
    double deltaE = initialEnergy - photon.Energy;
    if (opticalDepth < 1.e-5) // do not use difference of two large numbers
    {
       deltaE = initialEnergy*opticalDepth*(1. - 0.5*opticalDepth);
    }

//    tally into int array so order of additions doesn't matter
    long long deltaE_INT64 = E_to_ESINT64( deltaE, photon.zone );
    ASSERT( E_absorbed_INT64[photon.zone] + deltaE_INT64 < Two_to_64 );
    E_absorbed_INT64[photon.zone] += deltaE_INT64;
}

//---------------------------------------------------------------------------//

//    calculate info for energy balance

template<typename mesh_types, typename mat_types>
void IMC_4Momentum<mesh_types, mat_types>::
EPBookkeeping()
{

#ifdef USE_OPENMP
    if( mIsOpenMPCoarseThreaded )
    {
        E_census_in_zones_INT64.threadSum();
    }
#endif    
    if( mesh_replicated )
    {
        E_census_in_zones_INT64.replicaSum();
    }

//    loop over zones and convert census energy in each from ULL to double
    E_census = 0.0;
    for( ZoneIterator zoneItr = Mesh.PhysicalZoneBegin();
         zoneItr != Mesh.PhysicalZoneEnd(); ++zoneItr )
    {
        E_census_in_zones[*zoneItr] = EINT64_to_E( E_census_in_zones_INT64[*zoneItr],
                                                   *zoneItr );

        E_census += E_census_in_zones[*zoneItr];
    }

    for( ZoneIterator zoneItr = Mesh.PhysicalZoneBegin();
         zoneItr != Mesh.PhysicalZoneEnd(); ++zoneItr )
    {
        E_rad[*zoneItr] = EINT64_to_E( E_rad_INT64[*zoneItr], *zoneItr );
    
        E_absorbed[*zoneItr] = ESINT64_to_E( E_absorbed_INT64[*zoneItr], *zoneItr );
        E_absorbed_total += E_absorbed[*zoneItr];
    }
}



//---------------------------------------------------------------------------//

template<typename mesh_types, typename mat_types>
double IMC_4Momentum<mesh_types, mat_types>::
energyBalance()
{
  // WARNING:  This tally is probably broken in parallel.
  // Global sums of these base numbers should be done before 
  // computing deried quantities.
    const double tiny = 1.0e-99;    //    to avoid division by 0
    double Energy_check;
     
#ifdef USE_OPENMP
    std::vector<double> terms(5);
    terms[0] = Source_DB.get_E_photons_total();
    terms[1] = E_out;
    terms[2] = E_census_initial;
    terms[3] = E_small_photon_RR_created;
    terms[4] = E_small_photon_RR_destroyed;

    // pause here until all threads are synchronized
#pragma omp barrier
    
    //
    // a single thread needs to resize the sum buffer to the 
    // correct size.
    //                                          
    std::vector<double> &theThreadedDiagnosticData = mThreadedDiagnosticData();
    
#pragma omp single
    {
        theThreadedDiagnosticData.resize(terms.size());
        std::transform(theThreadedDiagnosticData.begin(),theThreadedDiagnosticData.end(),
                       theThreadedDiagnosticData.begin(),Zero_functor<double>());
    }

    //
    // now do the vector sum reduction
    for(size_t i=0 ;i< terms.size(); i++)
    {
#pragma omp atomic
        theThreadedDiagnosticData[i] += terms[i];
    }
    

    // pause here until all threads are reduced
#pragma omp barrier 

    double sum_E_in_photons =theThreadedDiagnosticData[0];
    double sum_E_out = theThreadedDiagnosticData[1];
    double sum_E_census_initial = theThreadedDiagnosticData[2];
    double sum_E_small_photon_RR_created = theThreadedDiagnosticData[3];
    double sum_E_small_photon_RR_destroyed = theThreadedDiagnosticData[4];
    double sum_E_census = E_census;
    double sum_E_absorbed_total = E_absorbed_total;

    double sum_Net_E_input = sum_E_in_photons - sum_E_out - sum_E_absorbed_total;

    double sum_deltaE = sum_Net_E_input -(sum_E_census - sum_E_census_initial); 

    double sum_Net_RR_input = sum_E_small_photon_RR_created - sum_E_small_photon_RR_destroyed;

    double sum_photon_energy_scale = sum_E_census + sum_E_in_photons + sum_E_absorbed_total;

    double sum_deltaE_RR = sum_deltaE + sum_Net_RR_input;
    
    Energy_check = sum_deltaE_RR/( sum_photon_energy_scale + tiny );
    
#else

//    cummulative energy input as photons after source RR
    double E_in_photons = Source_DB.get_E_photons_total();

//    cummulative net energy input
    double Net_E_input = E_in_photons - E_out - E_absorbed_total;
    
    double deltaE = Net_E_input - (E_census - E_census_initial);  

//    Account for statistical fluctuations from all Russian Roulletting
    double Net_RR_input = 0.0;
    Net_RR_input += E_small_photon_RR_created - E_small_photon_RR_destroyed;

//    Source - sink - Energy currently in photons
    double deltaE_RR = deltaE + Net_RR_input;

//    Net fractional energy loss, with statistical effects removed
    double photon_energy_scale;
    photon_energy_scale = E_census + E_in_photons + E_absorbed_total;
    EXPENSIVE_ASSERT( photon_energy_scale >= 0.0 );
    Energy_check = deltaE_RR/( photon_energy_scale + tiny );
#endif

    return Energy_check;
}

//---------------------------------------------------------------------------//

template<typename mesh_types, typename mat_types>
void IMC_4Momentum<mesh_types, mat_types>::
diagnosticOutput()
{

//    Print energy variables.

  std::vector< std::string > tally_names;
  std::vector< double > tallies;

  tally_names.push_back("E_photons_total");
  tallies.push_back( Source_DB.get_E_photons_total() );
  tally_names.push_back("E_census");
  tallies.push_back( E_census );
  tally_names.push_back("E_census_initial");
  tallies.push_back( E_census_initial );
  tally_names.push_back("E_absorbed_total");
  tallies.push_back( E_absorbed_total );
  tally_names.push_back("E_out");
  tallies.push_back( E_out );
  tally_names.push_back("E_census_RR_created");
  tallies.push_back( E_census_RR_created );
  tally_names.push_back("E_census_RR_destroyed");
  tallies.push_back( E_census_RR_destroyed );
  tally_names.push_back("Net RR census energy");
  tallies.push_back( E_census_RR_created - E_census_RR_destroyed );
  tally_names.push_back("E_small_photon_RR_created");
  tallies.push_back( E_small_photon_RR_created );
  tally_names.push_back("E_small_photon_RR_destroyed");
  tallies.push_back( E_small_photon_RR_destroyed );
  tally_names.push_back("net RR small photon energy");
  tallies.push_back( E_small_photon_RR_created - E_small_photon_RR_destroyed );

  double E_RR_total = 0.0;
  E_RR_total += E_census_RR_created - E_census_RR_destroyed;
  E_RR_total += E_small_photon_RR_created - E_small_photon_RR_destroyed;

  tally_names.push_back("IMC E_RR_total");
  tallies.push_back( E_RR_total );
  // This is probably broken in parallel; it is computed on a per processor
  // basis and not globally.
  tally_names.push_back("IMC Energy_check");
  tallies.push_back( energyBalance() );

  printGlobalInfo( tally_names,
                   tallies,
                   0, // root
                   false );
    
}

//---------------------------------------------------------------------------//

template<typename mesh_types, typename mat_types>
void IMC_4Momentum<mesh_types, mat_types>::
threadedDiagnosticOutput()
{
#ifdef USE_OPENMP
    
    int tid = omp_get_thread_num();
    
    
//    Print energy variables.

  std::vector< std::string > tally_names;
  std::vector< double > tallies;

  tally_names.push_back("E_photons_total");
  tallies.push_back( Source_DB.get_E_photons_total() );

  // E_census is already summed over threads
  // move this block AFTER the thread sum below

  tally_names.push_back("E_census_initial");
  tallies.push_back( E_census_initial );


  // E_absorbed is already summed over threads
  // move this block AFTER the thread sum below

  tally_names.push_back("E_out");
  tallies.push_back( E_out );
  tally_names.push_back("E_census_RR_created");
  tallies.push_back( E_census_RR_created );
  tally_names.push_back("E_census_RR_destroyed");
  tallies.push_back( E_census_RR_destroyed );
  tally_names.push_back("Net RR census energy");
  tallies.push_back( E_census_RR_created - E_census_RR_destroyed );
  tally_names.push_back("E_small_photon_RR_created");
  tallies.push_back( E_small_photon_RR_created );
  tally_names.push_back("E_small_photon_RR_destroyed");
  tallies.push_back( E_small_photon_RR_destroyed );
  tally_names.push_back("net RR small photon energy");
  tallies.push_back( E_small_photon_RR_created - E_small_photon_RR_destroyed );

  double E_RR_total = 0.0;
  E_RR_total += E_census_RR_created - E_census_RR_destroyed;
  E_RR_total += E_small_photon_RR_created - E_small_photon_RR_destroyed;

  tally_names.push_back("IMC E_RR_total");
  tallies.push_back( E_RR_total );
    //
    // next sum up the tallies using the static 
    // theThreadedDiagnosticData to buffer the data
    //

    //
    // a single thread needs to resize the sum buffer to the 
    // correct size.
    //
  std::vector<double> &theThreadedDiagnosticData = mThreadedDiagnosticData();
  
#pragma omp single
    {
        theThreadedDiagnosticData.resize(tallies.size());
        std::transform(theThreadedDiagnosticData.begin(),theThreadedDiagnosticData.end(),
                       theThreadedDiagnosticData.begin(),Zero_functor<double>());
    }

    //
    // now do the vector sum reduction
    for(size_t i=0 ; i<tallies.size(); i++)
    {
#pragma omp atomic
        theThreadedDiagnosticData[i] += tallies[i];
    }
    
    // pause here until all threads are reduced
#pragma omp barrier

    // copy into local ull_tallies vector
    std::copy(theThreadedDiagnosticData.begin(),theThreadedDiagnosticData.end(), tallies.begin() );
    
    tally_names.push_back("E_census");
    tallies.push_back( E_census );
    tally_names.push_back("E_absorbed_total");
    tallies.push_back( E_absorbed_total );
  
    // This is probably broken in parallel; it is computed on a per processor
    // basis and not globally.
    tally_names.push_back("IMC Energy_check");


    double theEnergyBalance = energyBalance();
    
    tallies.push_back( theEnergyBalance );

#pragma omp master
    {
        printGlobalInfoPrintf( tally_names,
                               tallies,
                               0, // root
                               false );
    }
#pragma omp barrier
#endif // ifdef USE_OPENMP
}

//---------------------------------------------------------------------------//


template<typename mesh_types, typename mat_types>
void IMC_4Momentum<mesh_types, mat_types>::
RRDiagnostics( const mesh_zone_ID_type& zone,
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
    ASSERT( Mesh.zone_is_real(zone) );

    eCensusRRCreated = E_census_RR_created_zone[zone];
    eCensusRRDestroyed = E_census_RR_destroyed_zone[zone];

    eSmallPhotonRRCreated = E_small_photon_RR_created_zone[zone];
    eSmallPhotonRRDestroyed = E_small_photon_RR_destroyed_zone[zone];

    eRRTotal = 0.0;
    eRRTotal += E_census_RR_created_zone[zone] - E_census_RR_destroyed_zone[zone];
    eRRTotal += E_small_photon_RR_created_zone[zone] - E_small_photon_RR_destroyed_zone[zone];
//    eRRTotal += Source_DB.Net_source_RR_Energy(zone);

    nCensusRRCreated = N_census_RR_created_zone[zone];
    nCensusRRDestroyed = N_census_RR_destroyed_zone[zone];

    nSmallPhotonRRCreated = N_small_photon_RR_created_zone[zone];
    nSmallPhotonRRDestroyed = N_small_photon_RR_destroyed_zone[zone];
}

//---------------------------------------------------------------------------//

//    add up data across all copies of the problem in parallel. 

template<typename mesh_types, typename mat_types>
void IMC_4Momentum<mesh_types, mat_types>::
sum_over_replicated_meshes()
{
#ifdef USE_OPENMP
    if( mIsOpenMPCoarseThreaded )
    {
//    sum deposition into radiation energy density
        E_rad_INT64.threadSum();
        
//    sum deposition into matter energy
        E_absorbed_INT64.threadSum();
    }
#endif

#ifdef USE_MPI

    if( !mesh_replicated ) return;

//    sum deposition into radiation energy density
    E_rad_INT64.replicaSum();

//    sum absorption deposition
    E_absorbed_INT64.replicaSum();

#endif
}


//---------------------------------------------------------------------------//

}    //    namespace IMC_namespace
