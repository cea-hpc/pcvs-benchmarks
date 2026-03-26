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

#ifdef USE_MPI
#include <mpi.h>
#endif

#ifdef USE_OPENMP
#include <omp.h>
#endif

#include <vector>
#include <iostream>

#include "setDomainInformation.hh"

using namespace std;

namespace IMC_namespace
{

template<typename Mesh_type>
void
setDomainInformation( const Mesh_type& Mesh,
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
                      unsigned int& ompThreadID)
{
    domain_decomposed = false;
    N_domains = 1;
    domainID = 0;
    mesh_replicated = false;
    N_replicas = 1;
    replicaID = 0;
    nProcs = 1;
    procID = 0;
    omp_coarse_threaded = false;
    numOmpThreads=1;
    ompThreadID=0;
    
#ifdef USE_MPI

    N_domains = Mesh.getNumberOfDomains();
    if( N_domains > 1 )
        domain_decomposed = true;
    else
        domain_decomposed = false;

    domainID = Mesh.getDomainID();
   
    vector<int> procIDs = Mesh.getProcessIDsFromDomain( domainID );
    N_replicas = procIDs.size();

    if( N_replicas > 1 )
        mesh_replicated = true;
    else
        mesh_replicated = false;

    replicaID = Mesh.getDomainGroupRank();

    nProcs = Mesh.getNumberOfProcs();
    
    procID = Mesh.getRank();

//cout << "domain_decomposed = " << domain_decomposed << endl;
//cout << "N_domains = " << N_domains << endl;
//cout << "domainID = " << domainID << endl;
//cout << "mesh_replicated = " << mesh_replicated << endl;
//cout << "N_replicas = " << N_replicas << endl;
//cout << "replicaID = " << replicaID << endl;
//cout << "nProcs = " << nProcs << endl;
//cout << "procID = " << procID << endl;
//cout << endl;

#endif

#ifdef USE_OPENMP
    omp_coarse_threaded = true;
    numOmpThreads = omp_get_max_threads();
    ompThreadID = omp_get_thread_num();
#endif
}

}    //    end IMC_namespace


