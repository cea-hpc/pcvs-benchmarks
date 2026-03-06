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

//    this class impliments opacity functions

#include <cmath>
#include <iostream>

#include "Analytic_opacity_data_base.hh"
#include "ASSERT.hh"

using  namespace std;

namespace IMC_namespace
{

//---------------------------------------------------------------------------//

//    constructor

template<typename mesh_types, typename mat_types>
Analytic_opacity_data_base<mesh_types, mat_types>::
Analytic_opacity_data_base( const vector<absorption_opacity_base*>& absorption_opacities_in,
                            const vector<scattering_opacity_base*>& scattering_opacities_in,
                            const Mesh_type& Mesh_in,
                            const Mat_DB_type& Mat_DB_in )
    : Opacity_data_base<mesh_types, mat_types>( Mesh_in, Mat_DB_in ),
      absorption_opacities( absorption_opacities_in ),
      scattering_opacities( scattering_opacities_in ),
      mIsSetUp( false )
{
//    there has to be at least one opacity
    if( absorption_opacities.size() == 0 )
    {
        cout << "Error in Analytic_opacity_data_base ctor!" << endl;
        cout << "absorption_opacities.size() = ";
        cout << absorption_opacities.size() << endl;
        cout << "There has to be at least one opacity in ";
        cout << "absorption_opacities ctor argument!" << endl;
    }
    ASSERT( absorption_opacities.size() != 0 );
    
    if( scattering_opacities.size() == 0 )
    {
        cout << "Error in Analytic_opacity_data_base ctor!" << endl;
        cout << "scattering_opacities.size() = ";
        cout << scattering_opacities.size() << endl;
        cout << "There has to be at least one opacity in ";
        cout << "scattering_opacities ctor argument!" << endl;
    }
    ASSERT( scattering_opacities.size() != 0 );
    
    if( scattering_opacities.size() != absorption_opacities.size() )
    {
        cout << "Error in Analytic_opacity_data_base ctor!" << endl;
        cout << "scattering_opacities.size() = ";
        cout << scattering_opacities.size() << endl;
        cout << "absorption_opacities.size() = ";
        cout << absorption_opacities.size() << endl;
        cout << "These have to be the same size!" << endl;
    }
}

//---------------------------------------------------------------------------//

template<typename mesh_types, typename mat_types>
void Analytic_opacity_data_base<mesh_types, mat_types>::
setUp( double dt )    
{
    mIsSetUp = true;
}

//---------------------------------------------------------------------------//

template<typename mesh_types, typename mat_types>
double Analytic_opacity_data_base<mesh_types, mat_types>::
absorption_opacity( const mesh_zone_ID_type& zone ) const
{
    
    unsigned int material_index = this->Mat_DB.material_index( zone );
    ASSERT( material_index < absorption_opacities.size() );

    absorption_opacity_base* opac;
    opac = absorption_opacities[material_index];
    
    return opac->absorption_opacity();
}

//---------------------------------------------------------------------------//

template<typename mesh_types, typename mat_types>
double Analytic_opacity_data_base<mesh_types, mat_types>::
scattering_opacity( const mesh_zone_ID_type& zone ) const
{    
    unsigned int material_index = this->Mat_DB.material_index( zone );
    ASSERT( material_index < scattering_opacities.size() );

    scattering_opacity_base* scatPtr;
    scatPtr = scattering_opacities[material_index];
        
    return scatPtr->scattering_opacity();
}

//---------------------------------------------------------------------------//

template<typename mesh_types, typename mat_types>
void Analytic_opacity_data_base<mesh_types, mat_types>::
physical_scattering( const mesh_zone_ID_type& zone,
                     rng& rand,
                     double& new_cos_theta ) const
{
    ASSERT( this->Mesh.zone_is_real(zone) );
    
    unsigned int material_index = this->Mat_DB.material_index( zone );
    ASSERT( material_index < scattering_opacities.size() );

    scattering_opacity_base* opac;
    opac = scattering_opacities[material_index];
    opac->scattering_results( rand,
                              new_cos_theta );
                            
}

//---------------------------------------------------------------------------//

//    Return the memory allocated by setUp()
//    For analytic opacities, there is no memory allocated.

template<typename mesh_types, typename mat_types>
void Analytic_opacity_data_base<mesh_types, mat_types>::
release_memory()
{
    mIsSetUp = false;
}

//---------------------------------------------------------------------------//

}    //    namespace IMC_namespace

