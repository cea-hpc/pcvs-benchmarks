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

#ifndef __Opacity_data_base_hh__
#define __Opacity_data_base_hh__

#include <vector>

#include "rng.hh"

namespace IMC_namespace
{

template<typename mesh_types, typename mat_types>
class Opacity_data_base
{
  public:
    typedef typename mesh_types::Mesh_type Mesh_type;
    typedef typename mesh_types::mesh_zone_ID_type mesh_zone_ID_type;
    typedef typename mesh_types::zcf_double zcf_double;
    typedef typename mat_types::Material_data_base_type Mat_DB_type;

    Opacity_data_base( const Mesh_type& Mesh,
                       const Mat_DB_type& Mat_DB );

    virtual ~Opacity_data_base() {};

    virtual double absorption_opacity( const mesh_zone_ID_type& zone ) const = 0;
                                       
    virtual double scattering_opacity( const mesh_zone_ID_type& zone ) const = 0;
                                           
    virtual void physical_scattering( const mesh_zone_ID_type& zone,
                                      rng& rand,
                                      double& new_cos_theta ) const = 0;

//    calculate opacity and allocate and fill vectors of 
//    subsidiary information needed, such as Planck opacity
    virtual void setUp( double dt ) = 0;
    virtual bool isSetUp() const = 0;
    
//    return the memory allocated by setUp()
    virtual void release_memory() = 0;

  protected:

    const Mesh_type& Mesh;
    const Mat_DB_type& Mat_DB;

  private:

//    don't want these used
    Opacity_data_base( const Opacity_data_base& );
    Opacity_data_base& operator=( const Opacity_data_base& );
};

//---------------------------------------------------------------------------//

}    //    namespace IMC_namespace

#endif    //    __Opacity_data_base_hh__

