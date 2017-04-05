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
//    function to sum values over the different domains (i. e., only over the
//    master domains) then broadcast the value to all procs
//---------------------------------------------------------------------------//

#ifdef USE_MPI
#include <mpi.h>
#include "MPI_type.hh"
#endif

#ifdef USE_OPENMP
#include <omp.h>
#include "OpenMP_ReductionFunctors.hh"
#endif

#include <iostream>

#include "sumOverDomains.hh"
#include "ASSERT.hh"

using namespace std;

//---------------------------------------------------------------------------//

namespace IMC_namespace
{

template<typename MeshType, typename DataType>
void
sumOverDomains( const MeshType& Mesh,
                DataType inputValue, DataType& summedValue )
{
#ifdef USE_MPI

#ifdef USE_OPENMP
#pragma omp master
#endif
    {
        //    need to get MPI type as a function of DataType
        MPI_type<DataType> mpiType;

        //    need to add input across master procs
        //    and then broadcast value to all procs

        //    first, sum over all master procs - summedValue will be correct only on
        //    the master procs
        if( Mesh.isDomainMaster() )
        {
            const MPI_Comm &domainMastersComm = Mesh.getDomainMastersCommunicator();
            MPI_Allreduce( &inputValue, &summedValue, 
                           1,
                           mpiType.get_MPI_type(),
                           mpiType.get_MPI_SUM(), 
                           domainMastersComm );
    
        }

        const MPI_Comm &domainGroupComm = Mesh.getDomainGroupCommunicator();
    
        //    get rank of master proc of this domain
        //    0 is always the local ID of the master in the domain group
        int domainGroupMasterRank = 0;
    
        //   This code just checks convention that group master rank in domain
        //   group comm is 0
        if( Mesh.isDomainMaster() )
        {            
            MPI_Comm_rank( domainGroupComm, &domainGroupMasterRank );
            ASSERT( domainGroupMasterRank == 0 );
        }
        else
        {
            int domainGroupRank;
            MPI_Comm_rank( domainGroupComm, &domainGroupRank );
            ASSERT( domainGroupRank != 0 );
        }
    
        //    Need to set the summedValue on every processor - broadcast from master
        //    to all members of domain group. summedValue should now be the same on
        //    every proc

        MPI_Bcast( &summedValue, 
                   1, 
                   mpiType.get_MPI_type(), 
                   domainGroupMasterRank, 
                   domainGroupComm );

    }// end of omp master section
#else    //    not doing MPI - summed value is the same as input value
    summedValue = inputValue;

#endif    //  using MPI
}


#ifdef USE_OPENMP
template<typename MeshType, typename DataType>
void
sumOverDomainsThreaded( const MeshType& Mesh,
                        DataType inputValue, DataType& summedValue,
                        std::vector<DataType> &sharedData )
{

    
    //
    // Now do the sum across processors if we are also using MPI
    //
#ifdef USE_MPI
#pragma omp master
    {
        //    need to get MPI type as a function of DataType
        MPI_type<DataType> mpiType;

        //    need to add input across master procs
        //    and then broadcast value to all procs

        //    first, sum over all master procs - summedValue will be correct only on
        //    the master procs
        if( Mesh.isDomainMaster() )
        {
            const MPI_Comm &domainMastersComm = Mesh.getDomainMastersCommunicator();
            MPI_Allreduce( &inputValue, &summedValue, 
                           1,
                           mpiType.get_MPI_type(),
                           mpiType.get_MPI_SUM(), 
                           domainMastersComm );
    
        }

        const MPI_Comm &domainGroupComm = Mesh.getDomainGroupCommunicator();
    
        //    get rank of master proc of this domain
        //    0 is always the local ID of the master in the domain group
        int domainGroupMasterRank = 0;
    
        //   This code just checks convention that group master rank in domain
        //   group comm is 0
        if( Mesh.isDomainMaster() )
        {            
            MPI_Comm_rank( domainGroupComm, &domainGroupMasterRank );
            ASSERT( domainGroupMasterRank == 0 );
        }
        else
        {
            int domainGroupRank;
            MPI_Comm_rank( domainGroupComm, &domainGroupRank );
            ASSERT( domainGroupRank != 0 );
        }
    
        //    Need to set the summedValue on every processor - broadcast from master
        //    to all members of domain group. summedValue should now be the same on
        //    every proc

        MPI_Bcast( &summedValue, 
                   1, 
                   mpiType.get_MPI_type(), 
                   domainGroupMasterRank, 
                   domainGroupComm );

    }//end of omp master section

#else    //    not doing MPI - summed value is the same as input value
    summedValue = inputValue;

#endif    //  using MPI

    // pause here until all threads are reduced
#pragma omp barrier 
    //
    // summedValue is correct on the omp master thread.  Propagate this to the other
    // threads 
    //
#pragma omp master
    {
        sharedData[0] = summedValue;
    }
#pragma omp barrier
    
    summedValue = sharedData[0];
} 

#endif // using OPENMP

}    //    end IMC_namespace
