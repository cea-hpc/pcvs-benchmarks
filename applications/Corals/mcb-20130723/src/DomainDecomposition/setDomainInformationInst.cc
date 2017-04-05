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

#include "mesh_typedef.hh"

#include "setDomainInformation.cc"

namespace IMC_namespace
{
    typedef mesh_types::Mesh_type Mesh_type;

    template 
    void 
    setDomainInformation<Mesh_type>( const Mesh_type& Mesh,
                                     bool& domain_decomposed,
                                     unsigned int& N_domains,
                                     unsigned int& domainID,
                                     bool& mesh_replicated,
                                     unsigned int& N_replicas,
                                     unsigned int& replicaID,
                                     unsigned int& nProcs,
                                     unsigned int& procID,
                                     bool& omp_coarse_threaded,
                                     unsigned int& numOmpThreads,
                                     unsigned int& ompThreadID );
    
}    //    namespace IMC_namespace
