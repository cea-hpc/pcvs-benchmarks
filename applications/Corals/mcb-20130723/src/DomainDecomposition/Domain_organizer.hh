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

//    This class sets up boundary conditions for use with domain decomposed
//    Monte Carlo. It is pure virtual. Monte Carlo classes have their own
//    Domain_organizer classes which decend from this class. 

#ifndef __Domain_organizer_hh__
#define __Domain_organizer_hh__

#include <vector>

#include "BC_list.hh"
#include "Domain_BC.hh"

#include "ASSERT.hh"

namespace IMC_namespace
{

template<typename Mesh_type, typename particle_type>  
class Domain_organizer
{
  public:
    typedef typename Mesh_type::mesh_face_ID_type mesh_face_ID_type;
    typedef std::vector<Boundary_Condition<Mesh_type,particle_type>*> BC_vector;

//    constructor
    explicit Domain_organizer( const Mesh_type& Mesh, unsigned int buffer_size = 512 );
    
//    function that sets up boundary conditions, allocating the
//    necessary Domain_BC objects.
    void setUpDomainDecompositionBCs( BC_vector& MonteCarlo_BCs );

    virtual ~Domain_organizer() = 0; 
   
    // Method that reinitializes the object given the mesh, the 
    // original list of boundary conditions (the list without 
    // the Domain_BC objects added by a call to the 
    // setUpDomainDecompositionBCs function), and a buffer 
    // size.  Note that any fields of boundary conditions 
    // originally built from the list of boundary conditions 
    // will also need to be reinitialized using the list of 
    // boundary conditions returned from this method.
    void reinitialize( const Mesh_type& Mesh, BC_vector& MonteCarlo_BCs, 
                       unsigned int buffer_size = 512 );

    void diagnosticOutput() const;

  protected: //    protected data used in inheriting classes
//    Meshes used to get geometry communication info
    const Mesh_type& Mesh;

//    MPI task ID number this object is created on
    int thisMPITask;

//    Domain_exchange objects used to exchange photons between domains.
    std::vector< Domain_exchange<Mesh_type,particle_type>* > DE_list;
    typedef typename std::vector< Domain_exchange<Mesh_type,particle_type>* >::const_iterator DE_const_it;

//    list of the Domain_BC objects that will be passed off to the Monte
//    Carlo object that uses this class
    std::vector< Domain_BC<Mesh_type,particle_type>* > Domain_BC_list;

    // Method used to initialize the data members in the class.
    void initialize( const Mesh_type& Mesh, unsigned int buffer_size );
};

}    //    namespace IMC_namespace

#endif    // __Domain_organizer_hh__





