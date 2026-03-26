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

#ifndef __IMC_Domain_organizer_hh__
#define __IMC_Domain_organizer_hh__

#include <vector>

#include "Domain_organizer.hh"
#include "Domain_photon_source.hh"
#include "Domain_BC.hh"
#include "photon_types.hh"
#include "ASSERT.hh"

namespace IMC_namespace
{

template<typename mesh_types, typename mat_types, typename Source_DB_type>  
class IMC_Domain_organizer 
    : public Domain_organizer<typename mesh_types::Mesh_type,
                              typename photon_types<mesh_types,mat_types>::photon_type>
{
  public:
    typedef typename mesh_types::Mesh_type Mesh_type;
    typedef typename mesh_types::mesh_face_ID_type mesh_face_ID_type;
    typedef typename photon_types<mesh_types,mat_types>::photon_type photon_type;
    typedef std::vector<Boundary_Condition<Mesh_type,photon_type>*> BC_vector;

//    constructor
    explicit IMC_Domain_organizer( const Mesh_type& Mesh, 
                                   unsigned int buffer_size = 500 );
    
//    function that sets up sources and boundary conditions, allocating the
//    necessary Domain_exchange, Domain_source, and Domain_BC objects.
    void setUpDomainDecomposition( Source_DB_type& Source_DB,
                                   BC_vector& Domain_BCs );

    ~IMC_Domain_organizer(); 

  private: 
//    The Domain_source object that gives photons to the IMC
    Domain_photon_source<mesh_types, mat_types>* Domain_source_ptr;
};

}    //    namespace IMC_namespace

#endif    // __IMC_Domain_organizer_hh__



