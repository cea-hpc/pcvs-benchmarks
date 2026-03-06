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

#ifndef NONBLOCKING_GATHER_HH
#define NONBLOCKING_GATHER_HH

#include <mpi.h>
#include <vector>
#include "Buffered_MPI_Send.hh"

#ifdef USE_OPENMP
#include <omp.h>
#endif

namespace IMC_namespace
{

   //! \brief A persistent, nonblocking gather
   //!
   //!  Manages the data and MPI requests for a persistent, nonblocking reduce,
   //!  where integers are collected from children periodically, added to the 
   //!  local count, and then occasionally this sum of children and local
   //!  data is sent to the parent processor.
   //!  
   //!  \note IMPORTANT: Successive instantiations of this class must either be created with
   //!  unique tags OR there needs to be a global barrier between the call the the
   //!  destructor of one instantiation and the constructor of the next.  Otherwise
   //!  the messages from different instances will get confused and bad things will happen.
   //!  I (Brunner) chose not to put an MPI_Barrier in the constructor because that is
   //!  a bit heavy handed, especially since most codes that use this have lots of barriers
   //!  already between time steps.  And using it more than once per time step, it is preferred
   //!  to use different tags.
   class Nonblocking_Gather
   {

     public:
      
       //! Enum specifying the type of relatives to 
       //! use for communication.
       enum RelativesType {RT_BINARY, RT_FAT, RT_MST};

       //! \brief  Initiate Gather.
       Nonblocking_Gather( int message_tag_in,
                           unsigned int array_length_in,
                           MPI_Comm comm_in = MPI_COMM_WORLD, 
                           RelativesType relatives_type_in = RT_BINARY );

       //! Finish recieving any incoming messages, delete outstanding MPI_Requests,
       //! and wait until all outgoing messages have finished.
       ~Nonblocking_Gather();

       //! Root is always zero processor because of IMC_namespace::relatives().
       unsigned int root_rank() const { return 0; }

       //! Accumulates threaded data from local_counts into the shared static vector.
       void accumulate_local( unsigned long long* local_counts );

       //! check whether all threads have completed the accumulate_local process
       bool areAllThreadsLocallyAccumulated();
       
       //! Check for incoming messages, accumulates data into \p local_counts.
       void accumulate( unsigned long long* local_counts );

       //! Get counts from children and send to parent, reset \p local_counts to zero,
       //! but only reset if we are not the root processor.
       void accumulate_and_send( unsigned long long* local_counts );

       //! Zero out the shared static vector.
       void reset_shared_buffer(  );
       
     private:

       //! Message tag, must be unique
       int message_tag;
       //! The MPI Communicator, typically MPI_COMM_WORLD.
       MPI_Comm comm;

       //! Total number of processors.
       int total_processors;
       //! My ID number
       unsigned int rank;

       //! Array of request for nonblocking receives.
       std::vector<MPI_Request> requests;
       //! Storage for MPI_Testsome results
       std::vector<int> done_indexes;
       //! Storage for MPI_Testsome results
       std::vector<MPI_Status> stats;

       //! Length of data array gathered.
       const unsigned int array_length;
       //! Array of arrays of incoming data.
       unsigned long long** incoming_data;

       //! The processor to whom I send results.
       int parent;
       //! Processors that I gather from.
       std::vector<int> children;

       //! A buffered send object for outgoing messages
       Buffered_MPI_Send buffer;

       //! A static vector for accumulating thread local data
       std::vector<unsigned long long>& mULLThreadedData();

       //! A static vector for tagging thread local data
       std::vector<std::size_t>& mSizeTThreadedData();

       //! Don't copy
       Nonblocking_Gather( const Nonblocking_Gather& );
       const Nonblocking_Gather& operator=( const Nonblocking_Gather& );

   };

}

#endif // NONBLOCKING_GATHER_HH
