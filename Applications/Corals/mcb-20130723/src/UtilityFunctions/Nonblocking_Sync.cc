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

#include "Nonblocking_Sync.hh"
#include "relatives.hh"
#include <climits>
#include <cstdio>

#ifdef USE_OPENMP
#include <omp.h>
#endif

namespace IMC_namespace
{

// global variable to count sync calls cheaply.
int num_sync_sends=0;

//------------------------------------------------------------------

//!  This gets the rank and size of the communicator, and posts a nonblocking
//!  receive for the all done message from the parent.
Nonblocking_Sync::Nonblocking_Sync( int message_tag_in, 
                                    MPI_Comm comm_in, 
                                    RelativesType relatives_type_in )
        : message_tag(message_tag_in),
          comm(comm_in),
          size(INT_MAX),
          rank(INT_MAX),
          done(0),
          parent(-1)
{
    int intSize, intRank;
    MPI_Comm_size(comm, &intSize);
    MPI_Comm_rank(comm, &intRank);
    size = static_cast<unsigned int>( intSize );
    rank = static_cast<unsigned int>( intRank );

#pragma omp master
    {
        // make sure the static bool gets initialized before we try and use it across all threads
        //
        bool& amIDoneYet = mDoneYet();
        amIDoneYet = false;

        // need to control the destruction of the Buffered_MPI_Send object so I'll 
        // build one here only on the master thread and destroy it on the master thread
        // the Nonblocking_Sync::~Nonblocking_Sync()
        //
        buffer = new Buffered_MPI_Send();
        
        ASSERT(relatives_type_in == RT_BINARY 
               || relatives_type_in == RT_FAT 
               || relatives_type_in == RT_MST);

        //
        // determine parent MPI task and children MPI task(s).
        if (relatives_type_in == RT_BINARY)
        {
            relatives_binary(rank, size, parent, children);
        }
        else if (relatives_type_in == RT_FAT)
        {
            relatives_fat(rank, size, parent, children);
        }
        else if (relatives_type_in == RT_MST)
        {
            relatives_mst(rank, size, parent, children);
        }

        //
        // post nonblocking receive from parent task
              
        initiate_communications();
    }//end of omp master block
}

//------------------------------------------------------------------

//! Delete outstanding messages
Nonblocking_Sync::~Nonblocking_Sync()
{
#pragma omp master
    {
        delete buffer;
        if( rank != root_rank() && request != MPI_REQUEST_NULL )
        {
            MPI_Cancel( &request );
        }
    }//end of omp master block
}

//------------------------------------------------------------------

//! get Static data holding flag about local comm done yet
bool& Nonblocking_Sync::mDoneYet()
{
    static bool sDoneYet;
    return sDoneYet;
}


//! Re-initializes the communications so that a second sync can be performed.
void Nonblocking_Sync::reset()
{
    ASSERT( done == 1 );
    done = 0;
#pragma omp master
    {
        buffer->check_and_free();
        initiate_communications();
    }//end of omp master block
}

//------------------------------------------------------------------

//! Check for incoming done message. If done, send to children. 
bool Nonblocking_Sync::finished()
{
    int message_received = 0;

    bool &amIDoneYet = mDoneYet();
       
    //
    // For non-master threads, amIDoneYet tells me if the communication has finished.
    // There is a race condition here between this read of amIDoneYet and the write in
    // the omp master section below where amIDoneYet is set to true.  This is probably
    // OK since finished() is called by the the non-root mpi task threads to check
    // if the root task has sent the 'done' message.  The worst case (I believe) is 
    // a few extra calls to finished from the threads until the amIDoneYet below gets 
    // set to true.  
    // 
    message_received = amIDoneYet;
    
#pragma omp master
    {
        if( rank == root_rank() )
        {
            message_received = done;
        }
        else
        {
            MPI_Status status;
               
            MPI_Test( &request, &message_received, &status);
               
            if( message_received )
            {
                ASSERT(done == 1);

                amIDoneYet = true;
                send_finished_internal();
            }
        }
           
    }//end of omp master block
    return message_received;
       
}

//------------------------------------------------------------------

//! Initiate send done message to children.  Only valid from root_rank().
void Nonblocking_Sync::send_finished()
{
    ASSERT(done == 0);

    done = 1;
    bool &amIDoneYet = mDoneYet();

    ASSERT(rank == root_rank());
#pragma omp master
    {
        amIDoneYet = true;
        send_finished_internal();
    }
      
}

//------------------------------------------------------------------

//! Send finished message to children, but only if we've already been told
//! we're done ourselves. 
void Nonblocking_Sync::send_finished_internal()
{
  ASSERT(done == 1);
  
  // Send done message to kids.
#pragma omp master
  {
    num_sync_sends++;
    for(unsigned int kid = 0; kid< children.size(); ++kid)
    {
      buffer->send( &done, 1, MPI_INT, children[kid], message_tag, comm);
    }
  }
}

//------------------------------------------------------------------

//! Start nonblocking communications. 
void Nonblocking_Sync::initiate_communications()
{
    ASSERT( done == 0 );

    if(parent != -1 )
    {
        ASSERT( rank != root_rank() );
        
        MPI_Irecv( &done,
                   1,
                   MPI_INT,
                   parent,
                   message_tag,
                   comm,
                   &request);
    
    }
    else
    {
        ASSERT( rank == root_rank() );
    }
}//end of Nonblocking_Sync::initiate_communications()

}//end of namespace IMC_namespace
