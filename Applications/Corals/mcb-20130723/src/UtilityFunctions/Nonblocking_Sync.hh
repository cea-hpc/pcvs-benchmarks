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

#ifndef NONBLOCKING_SYNC_HH
#define NONBLOCKING_SYNC_HH

#include <mpi.h>
#include <vector>

#include "Buffered_MPI_Send.hh"

namespace IMC_namespace
{

   //! \brief An advanced global sync.
   //!
   //! This is a nonblocking global sync.  This allows a processor to keep working
   //! until it receives a done message from somebody else.  In order to be scalable,
   //! the finished message is passed along in a tree-like manner specified by 
   //! the function IMC_namespace::relatives().
   class Nonblocking_Sync
   {

     public:

       //! Enum specifying the type of relatives to 
       //! use for communication.
       enum RelativesType {RT_BINARY, RT_FAT, RT_MST};

       //! Initiate nonblocking sync
       explicit Nonblocking_Sync( int message_tag_in,
                                  MPI_Comm comm_in = MPI_COMM_WORLD, 
                                  RelativesType relatives_type_in = RT_BINARY );

       //! Cancel outstanding messages.
       ~Nonblocking_Sync();

       //! Root is always zero processor because of tree generator (IMC_namespace::relatives().
       unsigned int root_rank() const { return 0; }

       //! Re-initializes the communications so that a second sync can be performed.
       void reset();

       //! Check for incoming done message. If done, send to children. 
       bool finished();

       //! Initiate send done message to children.  Only valid from root_rank().
       void send_finished();

     private:

       //! Actually sends the messages.  Called by both finished() and send_finished().
       void send_finished_internal();

       //! Start nonblocking communications. 
       void initiate_communications();

       //! Message tag.  Should be unique.
       int message_tag;
       //! MPI Communicator, such as MPI_COMM_WORLD.
       MPI_Comm comm;

       //! Total number of processors in the MPI_Comm
       unsigned int size;
       //! My id number in the MPI_Comm
       unsigned int rank;

       //! The request for the nonblocking receive.
       MPI_Request request;
       //! Storage for the finished message receive
       int done;

       //! The id of the processor from whom I'll receive the finished message.
       int parent;
       //! The id of the processors that I need to tell that we're finished.
       std::vector<int> children;

       //! A buffered send object for outgoing messages
       Buffered_MPI_Send *buffer;

       //! A shared (static) bool to tell all threads the comm is done
       bool& mDoneYet();
       
       //! Don't copy
       Nonblocking_Sync( const Nonblocking_Sync& );
       //! Don't copy
       const Nonblocking_Sync& operator=( const Nonblocking_Sync& );

   };

}

#endif // NONBLOCKING_SYNC_HH
