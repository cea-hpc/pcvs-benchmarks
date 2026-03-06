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

#ifndef __Analytic_opacity_data_base_hh__
#define __Analytic_opacity_data_base_hh__

#include <vector>

#include "Opacity_data_base.hh"
#include "absorption_opacity_base.hh"
#include "scattering_opacity_base.hh"
#include "rng.hh"

namespace IMC_namespace
{

template<typename mesh_types, typename mat_types>
class Analytic_opacity_data_base : public Opacity_data_base<mesh_types, mat_types>
{
  public:
    typedef typename mesh_types::Mesh_type Mesh_type;
    typedef typename mesh_types::mesh_zone_ID_type mesh_zone_ID_type;
    typedef typename mesh_types::zcf_double zcf_double;
    typedef typename mat_types::Material_data_base_type Mat_DB_type;

    Analytic_opacity_data_base( const std::vector<absorption_opacity_base*>& absorption_opacities,
                                const std::vector<scattering_opacity_base*>& scattering_opacities,
                                const Mesh_type& Mesh,
                                const Mat_DB_type& Mat_DB );

    double absorption_opacity( const mesh_zone_ID_type& zone ) const;
                               
    double scattering_opacity( const mesh_zone_ID_type& zone ) const;

    void physical_scattering( const mesh_zone_ID_type& zone,
                              rng& rand,
                              double& new_cos_theta ) const;
                     
//    calculate opacity and allocate and fill vectors of 
//    subsidiary information needed, such as Planck opacity
    void setUp( double dt );
    bool isSetUp() const { return mIsSetUp; }
    
//    return the memory allocated by setUp()
    void release_memory();
    
  private:    
//    set opacity data from material opacities
    void calculate_zone_averages();

//    one analytic absorption opacity for each material in the problem
    std::vector<absorption_opacity_base*> absorption_opacities;
    
//    one analytic scattering opacity for each material in the problem
    std::vector<scattering_opacity_base*> scattering_opacities;
    
    bool mIsSetUp;

//    don't want these used
    Analytic_opacity_data_base( );
    Analytic_opacity_data_base( const Analytic_opacity_data_base& );
    Analytic_opacity_data_base& operator=( const Analytic_opacity_data_base& );
};

//---------------------------------------------------------------------------// 

}    //    namespace IMC_namespace

#endif    //    __Analytic_opacity_data_base_hh__
