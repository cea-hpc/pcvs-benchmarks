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

// For memcpy
#include <string.h>
#include <cstdio>
#include "ASSERT.hh"
#include "Buffered_MPI_Send.hh"

//#include <iostream>

namespace IMC_namespace
{

// global static to count buffered sends
int num_buffered_mpi_send_sends=0;

//-----------------------------------------------------------------------

Buffered_MPI_Send::Buffered_MPI_Send()
{
}

//-----------------------------------------------------------------------

Buffered_MPI_Send::~Buffered_MPI_Send()
{
  wait_until_all_done();
}

//-----------------------------------------------------------------------

void Buffered_MPI_Send::fill_request_vector()
{
  requests.resize( outstanding_buffers.size() );
  iterators.resize( outstanding_buffers.size() );

  std::vector<MPI_Request>::iterator rvi = requests.begin();
  std::vector<iterator>::iterator ii = iterators.begin();

  for( iterator i = outstanding_buffers.begin();
       i != outstanding_buffers.end();
       ++i
    )
  {
    ASSERT( rvi != requests.end() );
    ASSERT( ii != iterators.end() );
    *rvi = i->first;
    *ii = i;
    ++rvi;
    ++ii;
  }
}

//-----------------------------------------------------------------------

//! See if any outgoing messages have finished, then free the memory if they have.
void Buffered_MPI_Send::check_and_free()
{

  fill_request_vector();

  // If there are any messages out, check to see if they are done
  if( requests.size() > 0 )
  {
    int num_count_done = 0;

    done_indexes.resize( requests.size() );
    stats.resize( requests.size() );

    MPI_Testsome( static_cast<int>(requests.size()),
		  &requests[0],
		  &num_count_done,
		  &done_indexes[0],
		  &stats[0]
      );

    // Free the memory
    for( int d=0; d<num_count_done; ++d )
    {
      int i = done_indexes[d];

      delete [] iterators[i]->second;
      outstanding_buffers.erase( iterators[i] );
    }
  }
}

//-----------------------------------------------------------------------

//!  Wait (block) until all outgoing messages are finished,
//!  then free the memory.
void Buffered_MPI_Send::wait_until_all_done()
{
       
  fill_request_vector();

  // Wait until they are all done.
  if( requests.size() > 0 )
  {
    stats.resize( requests.size() );

    MPI_Waitall( static_cast<int>(requests.size()),
		 &requests[0],
		 &stats[0]
      );
  }

  // Free the memory
  for( iterator i = outstanding_buffers.begin();
       i != outstanding_buffers.end();
       ++i
    )
  {
    delete [] i->second;
  }

  // empty the array.
  outstanding_buffers.clear();
}

//-----------------------------------------------------------------------


// Copies the data to a new buffer and starts a nonblocking send.
// Also calls check_and_free() to remove any completed buffers.
void Buffered_MPI_Send::send( void *buf,
			      int count,
			      MPI_Datatype datatype,
			      int dest, 
			      int tag,
			      MPI_Comm comm
  )
{
  num_buffered_mpi_send_sends++;

  // Make a new request
  MPI_Request req;

  // Get the data size
  int datatype_size;
  MPI_Type_size( datatype, &datatype_size );
  int buffer_size = datatype_size*count;

  // Allocate the new memory
  char* buf_copy = new char[buffer_size];

  // Copy the buffer to our own storage.
  memcpy( buf_copy, buf, buffer_size );

  // Send the message, finally.
  MPI_Isend(buf_copy, count, datatype, dest, tag, comm, &req);

  outstanding_buffers.insert( value_type( req, buf_copy ) );

  // Eagerly free memory if possible.
  check_and_free();
}

//-----------------------------------------------------------------------
}

