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
// class Source_data_base - calculates source photons for IMC
//
//===========================================================================//

#ifdef USE_MPI
#include <mpi.h>
#endif

#include <cstdio>

#include "Source_data_base.hh"
#include "shave.hh"
#include "set_isotropic_cosines.hh"
#include "rng.hh"
#include "setDomainInformation.hh"
#include "sumOverDomains.hh"

#ifdef USE_OPENMP
#include <omp.h>
#include "OpenMP_ReductionFunctors.hh"
#endif

using namespace std;

namespace IMC_namespace
{

//---------------------------------------------------------------------------//

//    constructor for problems with no external_sources
//    initial_seed defaults to 1

template<typename mesh_types, typename mat_types>
Source_data_base<mesh_types, mat_types>::
Source_data_base( const Mesh_type& Mesh_in,
                  const Material_data_base_type& Material_DB_in,
                  const Opacity_data_base<mesh_types, mat_types>& Opacity_DB_in,
                  const vector<photon_source<mesh_types, mat_types>*>& photon_source_list_in,
                  unsigned long long initial_seed )
    : Mesh( Mesh_in ),
      Material_DB( Material_DB_in ),
      Opacity_DB( Opacity_DB_in ),
      photon_source_list( photon_source_list_in ),
      E_in_photons( 0.0 ),
      E_photons_total( 0.0 ),
      E_emitted( Mesh_in ),
      zonal_seed( Mesh ),
      external_source_list( 0 ),
      E_source_RR_created( 0.0 ),
      E_source_RR_destroyed( 0.0 ),
      _N_initial( 0 ),
      _N_created( 0 ),
      N_source_RR_destroyed( 0 ),
      E_source_RR_created_zone( Mesh ), E_source_RR_destroyed_zone( Mesh ),
      N_created_zone( Mesh ), N_source_RR_destroyed_zone( Mesh )
{
#ifdef USE_OPENMP
#pragma omp single
    {
        createThreadDiagnosticBuffers();
    }
#endif
    initialize_zonal_seed( initial_seed );

    E_input = 0.0;

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

//---------------------------------------------------------------------------//
#ifdef USE_OPENMP
template<typename mesh_types, typename mat_types>
std::vector<unsigned long long>& Source_data_base<mesh_types, mat_types>::

mULLThreadedDiagnosticData()
{
    static std::vector<unsigned long long> sULLThreadedDiagnosticData(1);
    return sULLThreadedDiagnosticData;
}


template<typename mesh_types, typename mat_types>
std::vector<double>& Source_data_base<mesh_types, mat_types>::
mDoubleThreadedDiagnosticData()
{
    static std::vector<double> sDoubleThreadedDiagnosticData(1);
    return sDoubleThreadedDiagnosticData;
}

#endif

//---------------------------------------------------------------------------//

//    constructor for problems with external_sources
//    initial_seed defaults to 1

template<typename mesh_types, typename mat_types>
Source_data_base<mesh_types, mat_types>::
Source_data_base( const Mesh_type& Mesh_in,
                  const Material_data_base_type& Material_DB_in,
                  const Opacity_data_base<mesh_types, mat_types>& Opacity_DB_in,
                  const vector<photon_source<mesh_types, mat_types>*>& photon_source_list_in,
                  const vector<external_source<mesh_types, mat_types>*>& external_source_list_in,
                  unsigned long long initial_seed )
    : Mesh( Mesh_in ),
      Material_DB( Material_DB_in ),
      Opacity_DB( Opacity_DB_in ),
      photon_source_list( photon_source_list_in ),
      E_in_photons( 0.0 ),
      E_photons_total( 0.0 ),
      E_emitted( Mesh_in ),
      zonal_seed( Mesh ),
      external_source_list( external_source_list_in ),
      E_source_RR_created_zone( Mesh ), E_source_RR_destroyed_zone( Mesh ),
      N_created_zone( Mesh ), N_source_RR_destroyed_zone( Mesh )
{
    initialize_zonal_seed( initial_seed );

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

//---------------------------------------------------------------------------//

//   Called by ctors to set values in zcf zonal_seed, used to generate random
//   number states. After this call, each zone should have a very different
//   value, makeing sure that states of random number generators for photons
//   born in the zones are uncorreleated.

template<typename mesh_types, typename mat_types>
void Source_data_base<mesh_types, mat_types>::
initialize_zonal_seed( unsigned long long initial_seed )
{
//    initial_seed is a user input we can vary to make runs different
//    At this point, all zones have same state
    zonal_seed = rng_tools::hash_state( initial_seed );

//    Make seed different for each zone by using position.  Position is
//    shaved to prevent problems from roundoff. We can't use global zone
//    number because it may be different for different
//    decompositions. Normalize position to 1 so that nearby zones don't get
//    same seed when doubles are cast to unsigned long long.

// First find the extent of the mesh so we can normalize coordinates.
    double xmax[3];
    xmax[0] = -1.0e303; xmax[1] = -1.0e303; xmax[2] = -1.0e303;

    double xmin[3];
    xmin[0] = 1.0e303; xmin[1] = 1.0e303; xmin[2] = 1.0e303;

    for( ZoneIterator zoneItr = Mesh.PhysicalZoneBegin();
         zoneItr != Mesh.PhysicalZoneEnd();
         ++zoneItr )
    {
        vector<double> x_zone = Mesh.zone_position( *zoneItr );

        for( unsigned int index = 0; index < x_zone.size(); ++index )
        {
            if( x_zone[index] > xmax[index])
                xmax[index] = x_zone[index];
            if( x_zone[index] < xmin[index])
                xmin[index] = x_zone[index];
        }
    }

//    Need same answer no matter how many domains there are
#ifdef USE_MPI
// need different buffer to store in
    std::vector<double>& lMinMaxShared = mDoubleThreadedDiagnosticData();
    
#pragma omp master
    {
        lMinMaxShared.resize(6);

        double lmax[3];
        double lmin[3];
        MPI_Allreduce( xmax, lmax, 3, MPI_DOUBLE, MPI_MAX, Mesh.getWorldCommunicator() );
        MPI_Allreduce( xmin, lmin, 3, MPI_DOUBLE, MPI_MIN, Mesh.getWorldCommunicator() );

        lMinMaxShared[0]=lmin[0];
        lMinMaxShared[1]=lmin[1];
        lMinMaxShared[2]=lmin[2];
        lMinMaxShared[3]=lmax[0];
        lMinMaxShared[4]=lmax[1];
        lMinMaxShared[5]=lmax[2];
        
    }
#pragma omp barrier

    xmax[0] = lMinMaxShared[3];
    xmax[1] = lMinMaxShared[4];
    xmax[2] = lMinMaxShared[5];

    xmin[0] = lMinMaxShared[0];
    xmin[1] = lMinMaxShared[1];
    xmin[2] = lMinMaxShared[2];
#endif

//    need to shave these because there may be slight differences in mesh
//    coordinates when they are domain decomposed
    for( unsigned int i = 0; i <= 2; ++i )
    {
        xmax[i] = shave( xmax[i] );
        xmin[i] = shave( xmin[i] );
    }

// Compute the interval
    double invdx[3];
    for(unsigned int i=0; i < 3; i++)
    {
        double dx = xmax[i] - xmin[i];
        if(dx != 0.0)
            invdx[i] = 1.0 / dx;
        else if (xmax[i] != 0.0)
            invdx[i] = 1.0 / xmax[i];
        else
            invdx[i] = 1.0;
    }

// Now come up with unique integers based on normalized coordinates
    for( ZoneIterator zoneItr = Mesh.PhysicalZoneBegin();
         zoneItr != Mesh.PhysicalZoneEnd();
         ++zoneItr )
    {
    //    use coords of zone to make states different in each zone
        vector<double> x_zone = Mesh.zone_position( *zoneItr );

        for( unsigned int index = 0; index < x_zone.size(); ++index )
        {
        //    this will be between 0 and 1
            double x = (x_zone[index]  - xmin[index]) * invdx[index];

        //    Need to make x a function of index also, so that we don't get
        //    same values for pairs of zones with e.g., same x values and y
        //    and z values switched. raising it to index+1 power will still
        //    leave it between 0 and 1. Need to use index +1 because index =
        //    0 will result in x = 1 for all x.
            x = std::pow( x, static_cast<double>(index+1) );

        //    Map x from (0,1) to (0, 2**63) to stuff into 64 bit int.
        //    Need to shave of lower order digits because they can be
        //    different for parallel and serial runs on the "same" mesh.
            const double multiplier = 1.8446744e19;
            double norm_shaved_x = shave( multiplier * x );

        //    add should be different for every zone for at least one value
        //    of index as it loops over dimensions of zone position.
            unsigned long long addend = static_cast<unsigned long long>( norm_shaved_x );

        //    hash addend to mix it up some more
            zonal_seed[*zoneItr] += rng_tools::hash_state( addend );
        }

    //    hash zonal_seed to mix it up some more
        zonal_seed[*zoneItr] = rng_tools::hash_state( zonal_seed[*zoneItr] );
    }
}

//---------------------------------------------------------------------------//

//    used in Domain Decomposed case to add a Domain_source

template<typename mesh_types, typename mat_types>
void Source_data_base<mesh_types, mat_types>::
add_external_source( external_source<mesh_types, mat_types>* source_ptr )
{
    external_source_list.push_back( source_ptr );
}

//---------------------------------------------------------------------------//

//    sets up photon sources before each timestep

template<typename mesh_types, typename mat_types>
void Source_data_base<mesh_types, mat_types>::
setUp( double tn, double dt )
{
//    loop over sources
    typename vector<photon_source<mesh_types, mat_types>*>::iterator source;
    for( source= photon_source_list.begin();
         source != photon_source_list.end();
         ++source )
    {
        (*source)->setUp( tn, dt );
    }

}

//---------------------------------------------------------------------------//

//    sets up photon sources before each timestep

template<typename mesh_types, typename mat_types>
void Source_data_base<mesh_types, mat_types>::
setUpExternalSourcePhotons( )
{
    typename vector<external_source_type*>::iterator e_source;
    for( e_source= external_source_list.begin();
         e_source != external_source_list.end();
         ++e_source )
    {
        (*e_source)->setUp();
    }
}

//---------------------------------------------------------------------------//

//    This routine returns the total energy radiated in photons in a
//    cycle. This is the physical value, which means before russian
//    roulette. This value is used in the RR_comb_census() routine and in
//    get_source_photons(double dt). Both routines need it's value to figure
//    out how many source photons to create.

template<typename mesh_types, typename mat_types>
double Source_data_base<mesh_types, mat_types>::
E_photons_cycle( double tn, double dt )
{
    double E_photons = 0.0;

//    loop over sources, add up total photon energy from each
    typename vector<photon_source<mesh_types, mat_types>*>::iterator source;
    for( source= photon_source_list.begin();
         source != photon_source_list.end();
         source++ )
    {
        E_photons += (*source)->total_energy_emitted(tn, dt);
    }

    ASSERT( E_photons >= 0.0 );

    return E_photons;
}

//---------------------------------------------------------------------------//

//    This function is called by Source_data_base::get_photons_from_element
//    Given a photon, add it to the list

template<typename mesh_types, typename mat_types>
void Source_data_base<mesh_types, mat_types>::
add_photon( sourceItrType sourceItr,
	    unsigned int element,
	    photon_type& new_photon,
	    double tn, double dt,
	    unsigned long long N_element_new,
	    double photon_Energy,
	    unsigned int photon_count,
	    unsigned int source_index,
	    list< photon_type >& source_photons )
{
//    make the new photon
    (*sourceItr)->generate_source_photon( element,
					  new_photon,
					  photon_Energy,
					  tn,
					  dt);

    unsigned int N_photons_to_make = 1;
    double E_photons = photon_Energy;

    add_photons_to_source_list( E_photons, N_photons_to_make,
                                source_index,
                                new_photon, source_photons );
}

//---------------------------------------------------------------------------//

//    This function is called by Source_data_base::get_photons_from_element
//    Given a photon, compare it's energy to E_desired and decide how
//    many copies to make with energy of each set to E_desired

template<typename mesh_types, typename mat_types>
void Source_data_base<mesh_types, mat_types>::
add_photon_statistically( sourceItrType sourceItr,
			  unsigned int element,
			  photon_type& new_photon,
			  double E_desired,
			  double tn, double dt,
			  int N_killed,
			  unsigned long long N_element_new,
			  double photon_Energy,
			  unsigned int photon_count,
			  unsigned int source_index,
			  list< photon_type >& source_photons )
{
//    Get number of photons to add to list, and energy they should have.
    unsigned int N_photons_to_make;
    double E_photons;
    if( N_element_new == 1 )
    {
    //    don't RR photon if it's the only one from this element
        N_photons_to_make = 1;
        E_photons = photon_Energy;
    }
    else
    {
        N_photons_to_make = static_cast<int>( photon_Energy/E_desired +
                                              new_photon.random_number() );

        E_photons = E_desired;
    }

    if( N_photons_to_make == 0 )    // photon dies
    {
        E_source_RR_destroyed += photon_Energy;
        N_source_RR_destroyed += 1;
        N_killed += 1;
    }
    else
    {
    //    make the new photon
        (*sourceItr)->generate_source_photon( element,
					      new_photon,
					      photon_Energy,
					      tn,
					      dt);

        add_photons_to_source_list( E_photons, N_photons_to_make,
                                    source_index,
                                    new_photon, source_photons );
    }
}

//---------------------------------------------------------------------------//

//    This function is called by Source_data_base::get_photons_from_source 
//    Given pointer to source and an element of that source, loop over
//    each element, generate photons from it and put them on the
//    photon list

template<typename mesh_types, typename mat_types>
void Source_data_base<mesh_types, mat_types>::
get_photons_from_element( sourceItrType sourceItr,
			  unsigned int element,
			  double E_desired,
			  double tn, double dt,
			  int N_killed,
			  unsigned int& N_made,
			  unsigned int source_index,
			  list< photon_type >& source_photons )
{
//    get energy and number of photons from that element
    double E_photons_element;
    E_photons_element = (*sourceItr)->element_energy_emitted( element, tn, dt );


//    If source has turned off, E_photons_element could = 0
//    If element has negative volume or area, E_emitted element could be < 0
//    In that case, don't make any photons.
    if( E_photons_element <= 0.0 )
        return;    //    go on to next element

    ASSERT( E_photons_element > 0.0 );

//    make sure there is at least a chance of generating one
//    photon from each element that radiates energy by adding 0.5; if none
//    are created, add .999999 to allow for occasional small photons
    const double Nee = E_photons_element/E_desired;
    unsigned long long N_element_new;
    N_element_new = static_cast<unsigned long long>(Nee + 0.5);

//    Small zones get probability of at least 1 photon
    if( N_element_new == 0 )
    {
        N_element_new = 1; 
    }

//    energy of photons emitted from this element
    double photon_Energy;
    if( N_element_new != 0 )
        photon_Energy = E_photons_element/N_element_new; 
    else
    {
        photon_Energy = 0.0;

    //    We are not creating a photon for this small source, and
    //    have to account for that
        E_source_RR_destroyed += E_photons_element;
    }

//    loop over each photon to be generated in the element of the
//    source. Actually make one, so that we can treat it analogously
//    to RR_comb_census, to get the right number of source photons
//    generated
    for( unsigned int photon_count = 1; photon_count <= N_element_new;
         photon_count++ )
    {
        mesh_zone_ID_type new_zone = (*sourceItr)->zone( element );
        unsigned long long initial_state = next_state( new_zone );

    //    only make photon on this replica if this is satisfied
        N_made += 1;

        if( mesh_replicated && (N_made%N_replicas != replicaID) )
            continue;

        if( mIsOpenMPCoarseThreaded && (N_made%numOmpThreads != ompThreadID) )
            continue;

    // Need to make photon here to get RNG.
        photon_type new_photon( initial_state );

        add_photon_statistically( sourceItr,
                                  element,
                                  new_photon,
                                  E_desired,
                                  tn,  dt,
                                  N_killed,
                                  N_element_new,
                                  photon_Energy,
                                  photon_count,
                                  source_index,
                                  source_photons );

    }    //    end of loop over photon_count in element
}

//---------------------------------------------------------------------------//

//    This function is called by Source_data_base::get_source_photons
//    Given pointer to source, loop over each element, generate
//    photons from it and put them on the photon list

template<typename mesh_types, typename mat_types>
void Source_data_base<mesh_types, mat_types>::
get_photons_from_source( sourceItrType sourceItr,
			 double E_desired,
			 double tn, double dt,
			 int N_killed,
			 unsigned int& N_made,
			 unsigned int source_index,
			 list< photon_type >& source_photons )
{
//    Set variables for energy accounting
    (*sourceItr)->get_source_energy( tn, dt, 
                                     E_in_photons, E_emitted );

//    now loop over each element in the source
    for( unsigned int element = (*sourceItr)->elementBegin();
         element != (*sourceItr)->elementEnd(); ++element )
    {
        get_photons_from_element( sourceItr,
				  element,
				  E_desired,
				  tn,  dt,
				  N_killed,
				  N_made,
				  source_index,
				  source_photons );
    }  
}

//---------------------------------------------------------------------------//

//    This routine uses the variable E_source, calculated in a call
//    to set_E_source(dt), which is called before the RR_comb_census() routine.
//    It is calculated there instead of here, because RR_comb_census()
//    needs the value to figure out how many photons need to
//    be in census.
//    Every source will have at least one photon emitted, so there
//    may be more than N_source created. Combing the whole list will
//    bring the total number of photons back down before they are advanced.

template<typename mesh_types, typename mat_types>
void Source_data_base<mesh_types, mat_types>::
get_source_photons( unsigned long long N_cycle_photons,
                    double E_total,
                    double tn, double dt,
                    list< photon_type >& source_photons )
{
    ASSERT( E_total >= 0.0 );
    if( N_cycle_photons == 0.0 || E_total == 0.0 )
        return;

//    Figure out how much energy we want new photons to have
    const double E_desired = E_total/N_cycle_photons;
    ASSERT( E_desired > 0.0 );

    int N_killed = 0;

//    zero amount of energy emitted in each zone;
//    the sources will set the correct value
    E_emitted = 0.0;

//    now get number from each source and pop them onto the list
    unsigned int N_made = 0;
    unsigned int source_index = 0;    //    used to index N_source vector
    typename vector<photon_source<mesh_types, mat_types>*>::iterator sourceItr;
    for( sourceItr = photon_source_list.begin();
         sourceItr != photon_source_list.end(); sourceItr++ )
    {
        get_photons_from_source( sourceItr, 
				 E_desired,
				 tn, dt, 
				 N_killed, N_made, 
				 source_index,
				 source_photons );

    //    go on to next source, so increment source_index
        source_index += 1;
    }    //    end of loop over sources

}

//---------------------------------------------------------------------------//

//    Change photon energy to desired value and add it, and the appropriate
//    number of copies, to the source_list.

template<typename mesh_types, typename mat_types>
void Source_data_base<mesh_types, mat_types>::
add_photons_to_source_list( double E_desired, int N_to_add,
                            unsigned int source_index,
                            photon_type& new_photon,
                            list<photon_type>& source_photons )
{
// change photon energy to desired value and add it
    double delta_E = E_desired - new_photon.Energy;

    E_source_RR_created += delta_E;

    new_photon.Energy = E_desired;

//    set initial energy
    new_photon.Energy0 = new_photon.Energy;

//    Add N_to_add of this photon
    for( int copy = 0 ; copy < N_to_add; copy++ )
    {
        if( copy != 0 )
        {
            E_source_RR_created += E_desired;

        //    each copy needs different rng state
            new_photon.set_state( rng_tools::hash_state(new_photon.state() + copy) );
        }

        ASSERT( new_photon.consistent() );
        source_photons.push_back( new_photon );

        E_photons_total += new_photon.Energy;
        _N_created += 1;
        E_input += new_photon.Energy;
    }    //    end of for loop over photons

}

//---------------------------------------------------------------------------//

//    get next seed to put on photon generated in zone
//    Seed is a function of global zone number for reproducibility
//    on different numbers of domains.

template<typename mesh_types, typename mat_types>
unsigned long long Source_data_base<mesh_types, mat_types>::
next_state( const mesh_zone_ID_type& zone )
{
    zonal_seed[zone] += 1;

    return rng_tools::hash_state( zonal_seed[zone] );
}

//---------------------------------------------------------------------------//

//    gives new photons from external sources (like other domains
//    in a domain decomposed problem) to IMC

template<typename mesh_types, typename mat_types>
void Source_data_base<mesh_types, mat_types>::
get_external_source_photons( list<photon_type>& IMC_source_photons,
                             double& E_entered,
                             bool flush_buffer,
                             unsigned long long& N_entered )
{
//    loop over external sources and put external source photons onto the IMC list
    typename vector<external_source<mesh_types, mat_types>*>::const_iterator source_ptr;
    for( source_ptr = external_source_list.begin();
         source_ptr != external_source_list.end(); ++source_ptr )
    {

        (*source_ptr)->add_source_photons( IMC_source_photons, 
                                           E_entered,
                                           flush_buffer,
                                           N_entered );
    }
}

//---------------------------------------------------------------------------//

template<typename mesh_types, typename mat_types>
void Source_data_base<mesh_types, mat_types>::
finalize_external_source_photons()
{
    typename vector<external_source<mesh_types, mat_types>*>::const_iterator source_ptr;
    for( source_ptr = external_source_list.begin();
         source_ptr != external_source_list.end(); ++source_ptr )
    {

        (*source_ptr)->finalize();
    }
}

//---------------------------------------------------------------------------//


//    return information on russian roullette in zone

template<typename mesh_types, typename mat_types>
void Source_data_base<mesh_types, mat_types>::
RRDiagnostics( const mesh_zone_ID_type& zone,
               unsigned long long& N_source_created_RR,
               unsigned long long& N_source_destroyed_RR,
               double& E_source_created_RR,
               double& E_source_destroyed_RR ) const
{
    ASSERT( Mesh.zone_is_real(zone) );

    N_source_created_RR = N_created_zone[zone];
    N_source_destroyed_RR = N_source_RR_destroyed_zone[zone];

    E_source_created_RR = E_source_RR_created_zone[zone];
    E_source_destroyed_RR = E_source_RR_destroyed_zone[zone];
}

//---------------------------------------------------------------------------//

//    Check that the sum of energy of all photons actually created -
//    net RR photon energy in those photons = total photon source energy

template<typename mesh_types, typename mat_types>
double Source_data_base<mesh_types, mat_types>::
energyBalance() const
{
    double delta_E_RR = E_source_RR_created - E_source_RR_destroyed;

    double E_difference = E_photons_total - delta_E_RR - E_in_photons;

    return E_difference/( E_in_photons + 1.0e-20 );
}

//---------------------------------------------------------------------------//

template<typename mesh_types, typename mat_types>
void Source_data_base<mesh_types, mat_types>::
diagnosticOutput() const
{
    cout << endl;
    cout << "Source_data_base diagnostic information:" << endl;
    cout << endl;

    cout << "cummulative photon number information:" << endl;
    printf("N_initial = %llu\n", _N_initial );
    printf("N_created = %llu\n", _N_created );
    printf("N_source_RR_destroyed = %llu\n", N_source_RR_destroyed );
    unsigned long long N_passed_to_IMC;
    N_passed_to_IMC = _N_initial + _N_created - N_source_RR_destroyed;
    printf("N passed to IMC =  %llu\n", N_passed_to_IMC );
    cout << endl;

    cout << "source diagnostic info:" << endl;
    typename vector<photon_source<mesh_types, mat_types>*>::const_iterator sourceItr;
    for( sourceItr = photon_source_list.begin();
         sourceItr != photon_source_list.end(); sourceItr++ )
    {
        (*sourceItr)->diagnostic_output();
    }
    cout << endl;

    double E_emitted_total = 0.0;

    for( unsigned int zone = Mesh.first_real_zone();
         zone <= Mesh.last_real_zone();
         zone = Mesh.next_real_zone(zone) )
    {
        E_emitted_total += E_emitted[zone];
    }

    cout << "cummulative photon energy information:" << endl;
    cout << "E_in_photons = ";
    cout << E_in_photons << endl;
    cout << "E_photons_total = ";
    cout << E_photons_total << endl;
    cout << "E_emitted_total = " << E_emitted_total << endl;
    cout << "E_source_RR_created = " << E_source_RR_created << endl;
    cout << "E_source_RR_destroyed = " << E_source_RR_destroyed << endl;
    double delta_E_RR = E_source_RR_created - E_source_RR_destroyed;
    cout << "delta_E_RR = " << delta_E_RR << endl;
    double E_difference = E_photons_total - delta_E_RR - E_in_photons;
    cout << "E_difference = " << E_difference << endl;
    cout << endl;

    cout << "Source energy balance = " << energyBalance() << endl;
    cout << endl;
}

template<typename mesh_types, typename mat_types>
void Source_data_base<mesh_types, mat_types>::
threadedDiagnosticOutput()
{
    const unsigned int root = 0;
    int my_id = 0;
#ifdef USE_MPI
#pragma omp master
    {
        MPI_Comm_rank( MPI_COMM_WORLD, &my_id);
        MPI_Barrier( MPI_COMM_WORLD );
    }
#endif

    int tid =0;
#ifdef USE_OPENMP
#pragma omp barrier

    tid = omp_get_thread_num();

    if( tid == 0 && my_id == 0)
    {
        printf("\n");
        printf("Source_data_base diagnostic information:\n");
        printf("\n");

        printf("cumulative photon number information:\n");
    }
    
    std::vector<unsigned long long> ull_diags(3);
    ull_diags[0] = _N_initial;
    ull_diags[1] = _N_created;
    ull_diags[2] = N_source_RR_destroyed;
    
    //
    // next sum up the ull_diags using the static 
    // mULLThreadedDiagnosticData to buffer the data
    //

    //
    // a single thread needs to resize the sum buffer to the 
    // correct size.
    //
    std::vector<unsigned long long>& theULLThreadedDiagnosticData = mULLThreadedDiagnosticData();
    
#pragma omp single
    {
        theULLThreadedDiagnosticData.resize(ull_diags.size());
        std::transform(theULLThreadedDiagnosticData.begin(),theULLThreadedDiagnosticData.end(),
                       theULLThreadedDiagnosticData.begin(),Zero_functor<unsigned long long>());
    }

    //
    // now do the vector sum reduction
    for( size_t i=0;i<ull_diags.size(); i++ )
    {
#pragma omp atomic
        theULLThreadedDiagnosticData[i]+= ull_diags[i];
    }
    
        
    // pause here until all threads are reduced
#pragma omp barrier 

    // copy into local ull_diags vector
    std::copy(theULLThreadedDiagnosticData.begin(),theULLThreadedDiagnosticData.end(), ull_diags.begin() );

    unsigned long long N_passed_to_IMC;
    N_passed_to_IMC = ull_diags[0] + ull_diags[1] - ull_diags[2];

    if( tid == 0 && my_id == 0)
    {
        printf("N_initial = %llu\n", ull_diags[0] );
        printf("N_created = %llu\n", ull_diags[1] );
        printf("N_source_RR_destroyed = %llu\n", ull_diags[2] );
        printf("N passed to IMC =  %llu\n", N_passed_to_IMC );
        printf("\n");
        printf("source diagnostic info:\n");
    }
    
    typename vector<photon_source<mesh_types, mat_types>*>::const_iterator sourceItr;
    for( sourceItr = photon_source_list.begin();
         sourceItr != photon_source_list.end(); sourceItr++ )
    {
        (*sourceItr)->threaded_diagnostic_output();
    }

    // pause here until all threads are reduced
#pragma omp barrier 

    double E_emitted_total = 0.0;

    for( unsigned int zone = Mesh.first_real_zone();
         zone <= Mesh.last_real_zone();
         zone = Mesh.next_real_zone(zone) )
    {
        E_emitted_total += E_emitted[zone];
    }

    std::vector<double> double_diags(4);
    double_diags[0] = E_emitted_total;
    double_diags[1] = E_photons_total;
    double_diags[2] = E_source_RR_created;
    double_diags[3] = E_source_RR_destroyed;
    
    //
    // next sum up the double_diags using the static 
    // mDoubleThreadedDiagnosticData to buffer the data
    //

    //
    // a single thread needs to resize the sum buffer to the 
    // correct size.
    //
    std::vector<double>& theDoubleThreadedDiagnosticData = mDoubleThreadedDiagnosticData();
    
#pragma omp single
    {
        theDoubleThreadedDiagnosticData.resize(double_diags.size());
        std::transform(theDoubleThreadedDiagnosticData.begin(),theDoubleThreadedDiagnosticData.end(),
                       theDoubleThreadedDiagnosticData.begin(),Zero_functor<double>());
    }

    //
    // now do the vector sum reduction
    for(size_t i=0; i<double_diags.size(); i++)
    {
#pragma omp atomic
        theDoubleThreadedDiagnosticData[i] += double_diags[i];
    }
    
        

    // pause here until all threads are reduced
#pragma omp barrier 


    double sum_E_in_photons = E_in_photons;
    double sum_E_emitted_total = theDoubleThreadedDiagnosticData[0];
    double sum_E_photons_total = theDoubleThreadedDiagnosticData[1];
    double sum_E_source_RR_created = theDoubleThreadedDiagnosticData[2];
    double sum_E_source_RR_destroyed = theDoubleThreadedDiagnosticData[3];
    
    double delta_E_RR = sum_E_source_RR_created - sum_E_source_RR_destroyed;
    double E_difference = sum_E_photons_total - delta_E_RR - sum_E_in_photons;
    double E_balance = E_difference/(E_in_photons + 1.0e-20);
    
    if( tid == 0 && my_id == 0) 
    {
        printf("cumulative photon energy information:\n");
        printf("E_in_photons = %f\n",sum_E_in_photons);
        printf("E_photons_total = %f\n",sum_E_photons_total);
        printf("E_emitted_total = %f\n",sum_E_emitted_total);
        printf("E_source_RR_created = %f\n",sum_E_source_RR_created);
        printf("E_source_RR_destroyed = %f\n",sum_E_source_RR_destroyed);
        printf("delta_E_RR = %f\n",delta_E_RR);
        printf("E_difference = %g\n",E_difference);
        printf("\n");
        printf("Source energy balance = %g\n",E_balance);
        printf("\n");
    }
#endif // ifdef USE_OPENMP
}

//---------------------------------------------------------------------------//
//                              end of Source_data_base.cc
//---------------------------------------------------------------------------//

}    //    namespace IMC_namespace


