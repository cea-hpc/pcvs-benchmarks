//----------------------------------*-C++-*----------------------------------//
// Copyright 2009 Lawrence Livermore National Security, LLC
// All rights reserved.
//---------------------------------------------------------------------------//

// This work performed under the auspices of the U.S. Department of Energy byomZone
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

#include <iostream>
#include <algorithm>
#include <cstdio>

#include "isotropic_source.hh"
#include "set_isotropic_cosines.hh"

#ifdef USE_MPI
#include <mpi.h>
#endif

#ifdef USE_OPENMP
#include <omp.h>
#include "OpenMP_ReductionFunctors.hh"
#endif

using namespace std;

namespace IMC_namespace
{

//---------------------------------------------------------------------------//

//    constructor for isotropic_source class

template<typename mesh_types, typename mat_types>
isotropic_source<mesh_types, mat_types>::
isotropic_source( double sourceStrength_in,
                  double xRoot_in, double yRoot_in,
                  double omega_in, double phi_in,
                  bool perfectlyLoadBalanced,
                  const std::vector<mesh_zone_ID_type>& mesh_zones,
                  const Mesh_type& Mesh_in,
                  const Material_data_base_type& Material_DB_in,
                  const Opacity_data_base<mesh_types, mat_types>& Opacity_DB_in )
    : photon_source<mesh_types, mat_types>(),
      sourceStrength( sourceStrength_in ),
      xRoot( xRoot_in ),
      yRoot( yRoot_in ),
      omega( omega_in ),
      phi( phi_in ),
      mPerfectlyLoadBalanced( perfectlyLoadBalanced ),
      Mesh( Mesh_in ),
      Material_DB( Material_DB_in ),
      Opacity_DB( Opacity_DB_in ),
      N_photons_created( 0 ),
      E_in_physical( 0.0 ),
      E_in_photons( 0.0 ),
      source_name( "isotropic_source" )
{
    //
    // Touch the static arrays used for threaded diagnostic reduction to 
    // create them before they might be used in OpenMP regions
    //
#ifdef USE_OPENMP
#pragma omp single
    {
        createThreadDiagnosticBuffers();
    }
#endif
//    Go over every element of mesh_zones
    typedef typename std::vector<mesh_zone_ID_type>::const_iterator zoneIterator;
    for( zoneIterator zoneItr = mesh_zones.begin();
         zoneItr != mesh_zones.end(); ++zoneItr )
    {
    //    skip over non-physical zones
        if( !Mesh.zone_is_real( *zoneItr ) )
            continue;

        source_zones.push_back( *zoneItr );
    }

    number_of_source_zones = source_zones.size();

    if( sourceStrength < 0.0 )
    {
        cout << "sourceStrength = " << sourceStrength;
        cout << " in isotropic source ctor! "<< endl;
        cout << "It must be >= 0.0 !" << endl;
    }
    ASSERT( sourceStrength >= 0.0 );
}

//---------------------------------------------------------------------------//

#ifdef USE_OPENMP
template<typename mesh_types, typename mat_types>
std::vector<unsigned long long>& isotropic_source<mesh_types, mat_types>::
mULLThreadedDiagnosticData()
{
    static std::vector<unsigned long long> sULLThreadedDiagnosticData(1);
    return sULLThreadedDiagnosticData;
}


template<typename mesh_types, typename mat_types>
std::vector<double>& isotropic_source<mesh_types, mat_types>::
mDoubleThreadedDiagnosticData()
{
    static std::vector<double> sDoubleThreadedDiagnosticData(1);
    return sDoubleThreadedDiagnosticData;
}

#endif

//    total energy added to problem by all the zones in zone_list over time dt
//    needed for IMC code to figure out what energy each photon should have

template<typename mesh_types, typename mat_types>
double isotropic_source<mesh_types, mat_types>::
total_energy_emitted( double tn, double dt ) const
{
    ASSERT( dt > 0.0 );

    double total_energy = 0.0;
    for( unsigned int element = elementBegin();
         element < elementEnd();
         ++element )
    {
        total_energy += element_energy_emitted( element, tn, dt );
    }

    ASSERT( total_energy >= 0.0 );
    return total_energy;
}

//---------------------------------------------------------------------------//

//    For load-imbalanced case:
//      source is sourceStrength * Sx(x) * Sy(y) * T(t) with
//      Sx(x) = (x-xRoot)**3 * (x+xRoot)**3 / xRoot**6
//      Sy(y) = (y-yRoot)**3 * (y+yRoot)**3 / yRoot**6
//      T(t) = 0.5 * [1.0 + sin( omega * t + phi )]
//      This holds for x < xRoot, y < yRoot
//    For load-balanced case:
//      source is sourceStrength * T(t)
//    Then we need to integrate source over zone volume and dt to get the
//    energy emitted over the time step.

template<typename mesh_types, typename mat_types>
double isotropic_source<mesh_types, mat_types>::
loadImbalancedSourceIntegral( unsigned int zone, 
                              double tn, double dt ) const
{
    double elementVolume = Mesh.mesh_zone_volume(zone);

//    For space parts, approximate integral over zones by evaluating
//    source at zone center values
    double x = Mesh.x_zone( zone );
    double y = Mesh.y_zone( zone );

    if( (x > xRoot) || (y > yRoot) )
      return 0.0;

    double SxIntegral = (x - xRoot)*(x + xRoot);
    SxIntegral = SxIntegral*SxIntegral*SxIntegral;
    SxIntegral /= pow( xRoot, 6.0 );

    double SyIntegral = (y - yRoot)*(y + yRoot);
    SyIntegral = SyIntegral*SyIntegral*SyIntegral;
    SyIntegral /= pow( yRoot, 6.0 );

    double SpaceIntegral = SxIntegral*SyIntegral*elementVolume;

//    For time part, need to integrate T(t) from tn to tn + dt
    double term1 = -cos( omega * (tn + dt) + phi );
    double term2 = -cos( omega * tn + phi );
    double sinIntegral;
    if( std::abs(omega * dt) < 1.0e-5 )
        sinIntegral = (term1 - term2)/omega;
    else
        sinIntegral = sin( omega * tn + phi ) * dt;

    double TIntegral = 0.5 * ( dt + sinIntegral );

    return sourceStrength*SpaceIntegral*TIntegral;
}

//---------------------------------------------------------------------------//

template<typename mesh_types, typename mat_types>
double isotropic_source<mesh_types, mat_types>::
loadBalancedSourceIntegral( unsigned int zone, 
                            double tn, double dt ) const
{
    double elementVolume = Mesh.mesh_zone_volume(zone);

//    No space dependence
    double SpaceIntegral = sourceStrength*elementVolume;

//    For time part, need to integrate T(t) from tn to tn + dt
    double term1 = -cos( omega * (tn + dt) + phi );
    double term2 = -cos( omega * tn + phi );
    double sinIntegral;
    if( std::abs(omega * dt) < 1.0e-5 )
        sinIntegral = (term1 - term2)/omega;
    else
        sinIntegral = sin( omega * tn + phi ) * dt;
    double TIntegral = 0.5 * ( dt + sinIntegral );

    return SpaceIntegral*TIntegral;
}

//---------------------------------------------------------------------------//

template<typename mesh_types, typename mat_types>
double isotropic_source<mesh_types, mat_types>::
sourceIntegral( unsigned int zone, 
                double tn, double dt ) const
{
    if( mPerfectlyLoadBalanced )
        return loadBalancedSourceIntegral( zone, tn, dt );
    else
        return loadImbalancedSourceIntegral( zone, tn, dt );

}

//---------------------------------------------------------------------------//

//    energy an individual zone radiates in time dt
//    needed to determine how many photons get radiated from this zone

template<typename mesh_types, typename mat_types>
double isotropic_source<mesh_types, mat_types>::
element_energy_emitted( unsigned int element,
                        double tn, double dt ) const
{
    ASSERT( dt > 0.0 );
    ASSERT( element >= elementBegin() );
    ASSERT( element < elementEnd() );

//    get the zone that the local int element refers to
    mesh_zone_ID_type zone = source_zones[element];

//   energy is integral of source over zone volume  and time step
    double energy_of_element = sourceIntegral( zone, tn, dt );
    ASSERT( energy_of_element >= 0.0 );

    return energy_of_element;
}

//---------------------------------------------------------------------------//

//    energy an individual zone radiates in time dt
//    needed to determine how many photons get radiated from this zone

template<typename mesh_types, typename mat_types>
void isotropic_source<mesh_types, mat_types>::
get_source_energy( double tn,
                   double dt,
                   double& E_photons,
                   zcf_double& E_emitted ) const
{
//    add energy emitted by this source to the total amount of energy added
//    to problem.
    double E_in = total_energy_emitted( tn, dt );

    E_photons += E_in;

    E_in_physical += E_in;
}

//---------------------------------------------------------------------------//

//    assign characteristics to a source photon on the given zone

template<typename mesh_types, typename mat_types>
void isotropic_source<mesh_types, mat_types>::
generate_source_photon( unsigned int element,
                        photon_type& source_photon,
                        double Energy,
                        double tn,
                        double dt ) const
{
//    get the global zone number that the local int element refers to
    EXPENSIVE_ASSERT( element < source_zones.size() );
    mesh_zone_ID_type source_zone = source_zones[element];
    EXPENSIVE_ASSERT( Mesh.mesh_zone_volume(source_zone) >= 0.0 );

//    produce distribution of source photons in zone

    Mesh.randomZoneLocation( source_photon.get_rng(),
                             source_zone, source_photon.X,
                             source_photon.zone );

    ASSERT( Mesh.zone_is_real(source_photon.zone) );
    ASSERT( Mesh.d_min( source_photon.X, source_photon.zone) > -Mesh.getTrackingTolerance() );

    source_photon.t = tn + source_photon.random_number()*dt;  // photon time

//    isotropic direction for direction cosines
    double r1 = source_photon.random_number();
    double r2 = source_photon.random_number();
    set_isotropic_cosines( r1, r2, source_photon.Omega);

    source_photon.Energy = Energy;

    EXPENSIVE_ASSERT( source_photon.consistent() );

    N_photons_created += 1;
    E_in_photons += Energy;
}

//---------------------------------------------------------------------------//

//    what zone a photon will be in when created in element

template<typename mesh_types, typename mat_types>
typename isotropic_source<mesh_types, mat_types>::mesh_zone_ID_type
isotropic_source<mesh_types, mat_types>::
zone( unsigned int element ) const
{
    EXPENSIVE_ASSERT( element < source_zones.size() );

    return source_zones[element];
}


//---------------------------------------------------------------------------//

//    output for debugging

template<typename mesh_types, typename mat_types>
void
isotropic_source<mesh_types, mat_types>::
diagnostic_output() const
{
    cout << "for source " << source_name << endl;
    cout << "N_photons_created = " << N_photons_created << endl;
    cout << "E_in_physical = " << E_in_physical << endl;
    cout << "E_in_photons = " << E_in_photons << endl;
    
    double E_error = 0.0;
    if( E_in_physical != 0.0 )
    {
        E_error = (E_in_photons - E_in_physical)/E_in_physical;
    }
    
    cout << "fractional Energy input error = " << E_error << endl;
    
    cout << endl;
}

//---------------------------------------------------------------------------//

template<typename mesh_types, typename mat_types>
void
isotropic_source<mesh_types, mat_types>::
threaded_diagnostic_output()
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

#ifdef USE_OPENMP
#pragma omp barrier

    int tid = omp_get_thread_num();
    unsigned long long sum_N_photons_created = 0;
    double sum_E_in_physical = 0.0;
    double sum_E_in_photons = 0.0;
    
    std::vector<unsigned long long> ull_diags(1);
    ull_diags[0] = N_photons_created;
    
    std::vector<unsigned long long>& theULLThreadedDiagnosticData = mULLThreadedDiagnosticData();
    
    // 
    // initialize the theULLThreadedDiagnosticData reduction buffer
#pragma omp single
    {
        theULLThreadedDiagnosticData.resize(ull_diags.size());
        std::transform(theULLThreadedDiagnosticData.begin(),theULLThreadedDiagnosticData.end(),
                       theULLThreadedDiagnosticData.begin(),Zero_functor<unsigned long long>());
    }
    //
    // now do the vector sum reduction
    for(size_t i=0;i<ull_diags.size(); i++)
    {
#pragma omp atomic
        theULLThreadedDiagnosticData[i] += ull_diags[i];
    }
    
    
    // pause here until all threads are reduced
#pragma omp barrier 

    sum_N_photons_created = theULLThreadedDiagnosticData[0];

    std::vector<double> double_diags(1);
    double_diags[0] = E_in_photons;
    
    std::vector<double>& theDoubleThreadedDiagnosticData = mDoubleThreadedDiagnosticData();
    
    // 
    // initialize the theDoubleThreadedDiagnosticData reduction buffer
#pragma omp single
    {
        theDoubleThreadedDiagnosticData.resize(double_diags.size());
        std::transform(theDoubleThreadedDiagnosticData.begin(),theDoubleThreadedDiagnosticData.end(),
                       theDoubleThreadedDiagnosticData.begin(),Zero_functor<double>());
    }

    //
    // now do the vector sum reduction
    for(size_t i = 0 ; i< double_diags.size() ; i++)
    {
#pragma omp atomic
        theDoubleThreadedDiagnosticData[i] += double_diags[i];
        
    }
    

    // pause here until all threads are reduced
#pragma omp barrier 
    sum_E_in_physical= E_in_physical;
    sum_E_in_photons= theDoubleThreadedDiagnosticData[0];

    if( tid == 0 && my_id == 0)
    {
        printf("for source %s\n",source_name.c_str());
        printf("N_photons_created = %llu\n",sum_N_photons_created);
        printf("E_in_physical = %f\n",sum_E_in_physical);
        printf("E_in_photons = %f\n",sum_E_in_photons);
    }

    double E_error = 0.0;
    if( sum_E_in_physical != 0.0 )
    {
        E_error = (sum_E_in_photons - sum_E_in_physical)/sum_E_in_physical;
    }
    
    if( tid == 0 && my_id == 0)
    {
        printf("fractional Energy input error = %f\n",E_error);
        printf("\n");
    }
#endif // ifdef USE_OPENMP
}

//---------------------------------------------------------------------------//
}    //    namespace IMC_namespace

