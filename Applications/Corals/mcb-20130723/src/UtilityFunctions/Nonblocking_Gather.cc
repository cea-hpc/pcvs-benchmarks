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

#include <mpi.h>
#include <vector>

#include "Nonblocking_Gather.hh"
#include "relatives.hh"
#ifdef USE_OPENMP
#include <omp.h>
#endif

#include <cstdio>

#ifdef EXTRA_TALLIES
extern unsigned long erec;
#endif

using std::vector;

namespace IMC_namespace
{


// global static count of gathers
int num_gather_sends = 0;

//------------------------------------------------------------------------------------------

//! Gets rank and size of the MPI Communicator, gets our relatives in the gather tree from
//! IMC_namespace::relatives(), and initiates nonblocking receives.  In order to be scalable,
//! The gather is done in tree-like manner given by IMC_namespace::relatives().
//!
//! \note This attempts to clean up any outstanding messages, but does not always
//! get all of them.  You should use other means to make sure all messages really
//! are collected, otherwise you might have some problems using this multiple times.
//! (Like each time step.)
//!
Nonblocking_Gather::Nonblocking_Gather( int message_tag_in,
                                        unsigned int array_length_in,
                                        MPI_Comm comm_in, 
                                        RelativesType relatives_type_in )
        : message_tag(message_tag_in),
          comm(comm_in),
          total_processors(0),
          rank(0),
          array_length(array_length_in),
          incoming_data(0)
{
    // 
    // touch the static vector of unsigned long longs at start to ensure
    // proper intialization in threaded world.
    //
#pragma omp master
    {
        int numThreads = 1;
#ifdef USE_OPENMP
        numThreads = omp_get_num_threads();
#endif

        std::vector<unsigned long long> &theULLBuffer = mULLThreadedData();
        theULLBuffer.resize(array_length,0);
        
        std::vector<size_t> &theSizeTBuffer = mSizeTThreadedData();
        theSizeTBuffer.resize(numThreads,0);

    }
    
    //
    // put barrier here to ensure proper initialization of the shared threaded data
    // buffer before it may be used 
    //
#pragma omp barrier 

    // Get my information
    MPI_Comm_size(comm, &total_processors);
    int intRank;
    MPI_Comm_rank(comm, &intRank);
    rank = static_cast<unsigned int>( intRank );

          
#pragma omp master
    {
        // Get who I talk to

        ASSERT(relatives_type_in == RT_BINARY 
               || relatives_type_in == RT_FAT 
               || relatives_type_in == RT_MST);
        if (relatives_type_in == RT_BINARY)
        {
            relatives_binary(rank, total_processors, parent, children);
        }
        else if (relatives_type_in == RT_FAT)
        {
            relatives_fat(rank, total_processors, parent, children);
        }
        else if (relatives_type_in == RT_MST)

        {
            relatives_mst(rank, total_processors, parent, children);
        }
        
        if( children.size() > 0 )
        {

            // Resize storage vectors
            requests.resize( children.size() );
            done_indexes.resize( children.size() );
            stats.resize( children.size() );

            // Allocate 2D arrays to store data.
            incoming_data = new unsigned long long *[children.size()];
            incoming_data[0] = new unsigned long long[ array_length * children.size() ];

            for(unsigned int kid=0; kid < children.size(); ++kid)
            {

                // Assign array pointer for receive buffer.
                if( kid > 0)
                {
                    incoming_data[kid] = incoming_data[0] + kid * array_length;
                }

                // Initiate communications
                MPI_Irecv( incoming_data[kid],
                           array_length,
                           MPI_UNSIGNED_LONG_LONG,
                           children[kid],
                           message_tag,
                           comm,
                           &requests[kid]);
            }
        }
    }//end of omp master block

}

//------------------------------------------------------------------------------------------

//! Check make sure we receive all incoming messages, then delete all requests.
//! Because the destructor calls Blocking_MPI_Send::~Blocking_MPI_Send(),
//! this function will not return until all the outgoing messages have completed.
//! (In other words, it calls a blocking MPI function.)
Nonblocking_Gather::~Nonblocking_Gather()
{
#pragma omp master
    {
        // Get any outstanding messages so that we don't receive them next time this is called.
        if( children.size() != 0 )
        {
            int num_count_done=0;

            // Keep getting messages until they have all be received.
            do
            {

                MPI_Testsome( static_cast<int>(children.size()),
                              &requests[0],
                              &num_count_done,
                              &done_indexes[0],
                              &stats[0]);

                for(int d=0; d<num_count_done; ++d)
                {
                    int kid = done_indexes[d];

#ifdef EXTRA_TALLIES
                    ++erec;
#endif

                    // Post new receives for future messages.
                    MPI_Irecv( incoming_data[kid],
                               array_length,
                               MPI_UNSIGNED_LONG_LONG,
                               children[kid],
                               message_tag,
                               comm,
                               &requests[kid]);
                }

            }
            while( num_count_done > 0 );
        }

        // Free all outstanding requests
        for(unsigned int kid = 0; kid < children.size(); ++kid)
        {
            if( requests[kid] != MPI_REQUEST_NULL )
            {
                MPI_Cancel( &requests[kid] );
            }
        }

        // Delete buffers after canceling MPI requests.
        if( children.size() > 0 )
        {
            ASSERT( incoming_data != 0 );
            delete [] incoming_data[0];
        }
        delete [] incoming_data;
    }//end of omp master block

}


//------------------------------------------------------------------------------------------

//! A static vector for accumulating thread local data 
std::vector<unsigned long long>& Nonblocking_Gather::mULLThreadedData()
{
    static std::vector<unsigned long long> sULLThreadedData(1);
    return sULLThreadedData;
}

//------------------------------------------------------------------------------------------

//! A static vector for tagging thread local data
std::vector<size_t>& Nonblocking_Gather::mSizeTThreadedData()
{
    static std::vector<size_t> sSizeTThreadedData(1);
    return sSizeTThreadedData;
}

//------------------------------------------------------------------------------------------

void Nonblocking_Gather::reset_shared_buffer(  )
{
    size_t theNumThreads = 1;

#ifdef USE_OPENMP
    theNumThreads = omp_get_num_threads();
#endif

    std::vector<unsigned long long>& theThreadData = mULLThreadedData();
    std::vector<size_t>& theThreadDataTags = mSizeTThreadedData();
#pragma omp master
    {
        for(size_t i=0;i<array_length; i++)
        {
            theThreadData[i] = 0;
        }

        for(size_t i=0;i<theNumThreads; i++)
        {
            theThreadDataTags[i] = 0;
        }
        
    }
}

//------------------------------------------------------------------------------------------
//! check whether all threads have completed the accumulate_local process
bool Nonblocking_Gather::areAllThreadsLocallyAccumulated()
{
    size_t theNumThreads = 1;

#ifdef USE_OPENMP
    theNumThreads = omp_get_num_threads();
#endif

    std::vector<size_t>& theThreadDataTags = mSizeTThreadedData();
    bool theReturnValue=false;
    
#pragma omp critical(sumUpLocallyAccumulatedTags)
    {
        size_t theSum = 0;
        
        for(size_t i=0;i<theNumThreads; i++)
        {
            theSum += theThreadDataTags[i];
        }
        theReturnValue = (theSum == theNumThreads);
        
    }//end of critical construct
    
    return theReturnValue;
}

       
//------------------------------------------------------------------------------------------

//! Accumulates threaded data into the shared static vector.
void Nonblocking_Gather::accumulate_local( unsigned long long* local_counts )
{
    int tid = 0;
#ifdef USE_OPENMP
    tid = omp_get_thread_num();
#endif

    std::vector<unsigned long long>& theThreadData = mULLThreadedData();

// #pragma omp critical (threadDataAccumulate)
//     {
        //
        // now do the vector sum reduction
        for(size_t i=0;i<array_length; i++)
        {
#pragma omp atomic
            theThreadData[i] += local_counts[i];
        }
//     }//end of critical section

    //
    // set the tid index of the tag vector to 1 since we've added the local_count
    // contribution to the shared data buffer.
    //
    // When this vector has no zeros in it then
    // we know all threads have contributed to the accumulated 
    // thread sum
    //
    std::vector<size_t>& theThreadDataTags = mSizeTThreadedData();
    theThreadDataTags[tid] = 1;
    
#pragma omp barrier
    // 
    //  update local_counts to threaded sum value
    // 
    for(size_t i=0;i<array_length; i++)
    {
        local_counts[i]=theThreadData[i];
    }
}

//------------------------------------------------------------------------------------------
// Check for incoming messages, and return partial sum from children.
void Nonblocking_Gather::accumulate( unsigned long long* local_counts )
{
    int tid = 0;
#ifdef USE_OPENMP
    tid = omp_get_thread_num();
#endif
    //
    // only valid if all threaded data is already accumulated 
    // and we are on the master thread only
    //
    ASSERT(areAllThreadsLocallyAccumulated());
    ASSERT( tid == 0 );
    
    // This check is not strictly necessary by the MPI Standard, but two MPI implementations
    // have broken because MPI_Testsome() doesn't work with a zero size request array.
    // (These were the MPI on Red Storm and OpenMPI.)  Plus it may not be meaningful
    // to take the address of the first element of a std::vector<> that is zero length.
    if( children.size() != 0 )
    {
        int num_count_done=0;

        MPI_Testsome( static_cast<int>(children.size()),
                      &requests[0],
                      &num_count_done,
                      &done_indexes[0],
                      &stats[0]);

        for(int d=0; d<num_count_done; ++d)
        {

            int kid = done_indexes[d];

            // Accumulate data from buffers and reset them
            for(unsigned int i=0; i< array_length; ++i)
            {
                local_counts[i] += incoming_data[kid][i];
                incoming_data[kid][i] = 0;
            }

            // Post new receives for future messages.
            MPI_Irecv( incoming_data[kid],
                       array_length,
                       MPI_UNSIGNED_LONG_LONG,
                       children[kid],
                       message_tag,
                       comm,
                       &requests[kid]);
        }
  
    }


}

//------------------------------------------------------------------------------------------

//! Get children counts.  If then local_count plus children is nonzero, send it to parent.
//! If root, just add result add local_count.  If we're not root, local_count is zeroed
//! after sending so that we don't double count.
void Nonblocking_Gather::accumulate_and_send( unsigned long long* local_counts )
{
    int tid = 0;
    int numThreads = 1;
#ifdef USE_OPENMP
    tid = omp_get_thread_num();
    numThreads = omp_get_num_threads();
#endif
    
    //
    // this function should only be called by the master thread in an 
    // OpenMP sense
    //
    ASSERT( tid == 0 );

    // Get data from children first.
    accumulate( local_counts );
        
    // See if there is anything worth sending down
    bool any_counts = false;
    for( unsigned int i = 0; i < array_length; ++i)
    {
        if( local_counts[i] != 0 )
        {
            any_counts = true;
        }
    }
    
    // Send to parent.  (Root doesn't have a parent.)
    if( any_counts && rank != root_rank() )
    {
        ASSERT( parent != -1 );
        num_gather_sends++;

        buffer.send( local_counts,
                     array_length,
                     MPI_UNSIGNED_LONG_LONG,
                     parent,
                     message_tag,
                     comm);
        
        // reset counts.
        for( unsigned int i = 0; i<array_length; ++i)
        {
            local_counts[i] = 0;
        }
    }//end of send data conditional
    
}//end of accumulate_and_send

}//end namespace IMC_namespace

