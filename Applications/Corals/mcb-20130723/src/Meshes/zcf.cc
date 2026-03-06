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

//    Function definitions of the zcf class for general meshes.
#include "zcf.hh"

#ifdef USE_OPENMP
#include <omp.h>
#endif

#ifdef USE_MPI
#include "mpi.h"
#include "MPI_type.hh"
#endif

namespace IMC_namespace
{

//---------------------------------------------------------------------------//

//    for assignment of all members of field to a scalar

template<class Mesh_type, class data_type>
zcf<Mesh_type, data_type>& 
zcf<Mesh_type, data_type>::operator=( data_type x )
{
    for( typename std::vector<data_type>::iterator i = data.begin();
         i != data.end(); ++i )
    {
        (*i) = x;
    }

    return *this;
}

//---------------------------------------------------------------------------//

template<class Mesh_type, class data_type>
void zcf<Mesh_type, data_type>::
replicaSum()
{
#ifdef USE_MPI

#pragma omp master
    {
        size_t length = data.size();
        
        std::vector<data_type> storage( length );
        
        MPI_type<data_type> mpi_type;
        
        MPI_Allreduce( &data[0], 
                       &storage[0], 
                       static_cast<int>(length),
                       mpi_type.get_MPI_type(), 
                       mpi_type.get_MPI_SUM(), 
                       Mesh.getDomainGroupCommunicator() );
        
//    give all procs the same summed values
        data = storage;
    }
    
#endif
}

//---------------------------------------------------------------------------//

template<class Mesh_type, class data_type>
void zcf<Mesh_type, data_type>::
replicaSum( zcf& scratch )
{
#ifdef USE_MPI

#pragma omp master
    {
        size_t length = data.size();

        std::vector<data_type>& storage = scratch.data;

        MPI_type<data_type> mpi_type;

        MPI_Allreduce( &data[0], 
                       &storage[0], 
                       static_cast<int>(length),
                       mpi_type.get_MPI_type(), 
                       mpi_type.get_MPI_SUM(), 
                       Mesh.getDomainGroupCommunicator() );

//    give all procs the same summed values
        data = storage;
    }
    
#endif
}

//---------------------------------------------------------------------------//

template<class Mesh_type, class data_type>
void zcf<Mesh_type, data_type>::
replicaMax()
{
#ifdef USE_MPI

#pragma omp master
    {
        size_t length = data.size();

        std::vector<data_type> storage( length );

        MPI_type<data_type> mpi_type;

        MPI_Allreduce( &data[0], 
                       &storage[0], 
                       static_cast<int>(length),
                       mpi_type.get_MPI_type(), 
                       mpi_type.get_MPI_MAX(), 
                       Mesh.getDomainGroupCommunicator() );

//    give all procs the same max value
        data = storage;
    }
    
#endif
}

//---------------------------------------------------------------------------//

template<class Mesh_type, class data_type>
void zcf<Mesh_type, data_type>::
replicaMax( zcf& scratch )
{
#ifdef USE_MPI

#pragma omp master
    {
        size_t length = data.size();

        std::vector<data_type>& storage = scratch.data;

        MPI_type<data_type> mpi_type;

        MPI_Allreduce( &data[0], 
                       &storage[0], 
                       static_cast<int>(length),
                       mpi_type.get_MPI_type(), 
                       mpi_type.get_MPI_MAX(), 
                       Mesh.getDomainGroupCommunicator() );

//    give all procs the same max value
        data = storage;
    }
    
#endif
}

//---------------------------------------------------------------------------//
#ifdef USE_OPENMP
template<class Mesh_type, class data_type>
std::vector<data_type>& zcf<Mesh_type, data_type>::
mThreadReductionBuffer()
{
    static std::vector<data_type> sThreadReductionBuffer(1);
    return sThreadReductionBuffer;
}
#endif


//---------------------------------------------------------------------------//
}    //    namespace IMC_namespace

