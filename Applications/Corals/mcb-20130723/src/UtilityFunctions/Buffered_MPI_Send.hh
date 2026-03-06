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

#ifndef BUFFERED_MPI_SEND_HH
#define BUFFERED_MPI_SEND_HH

#include <mpi.h>
#include <map>
#include <vector>

namespace IMC_namespace
{

   //! \brief A dynamically allocated nonblocking send utility.
   //!
   //! This class will copy a buffer, start a nonblocking send
   //! of the data, and puts the request in stack.  The stack
   //! of outstanding messages can be checked using check_and_free()
   //! to see what messages have finished, and then free the buffer,
   //! or wait_until_all_done() will block until all outstanding 
   //! messages have finished, then free the memory.
   class Buffered_MPI_Send
   {
      public:

         Buffered_MPI_Send();

         //! Calls wait_until_all_done();
         ~Buffered_MPI_Send();

         //! See if any outgoing buffers have finished, and if they have, free the memory.
         void check_and_free();
         //! Wait until all the outgoing buffers have finished, and free all memory.
         void wait_until_all_done();

         //! Copy the incoming buffer to new memory and initiate a nonblocking send.
         void send( void *buf,
                    int count,
                    MPI_Datatype datatype,
                    int dest, 
                    int tag,
                    MPI_Comm comm
                  );

      private:

         //! Outstanding in the sense they haven't finished.
         //! (Not in the sense they are really nice buffers.)
         std::map<MPI_Request, char*> outstanding_buffers;
         typedef std::map<MPI_Request, char*>::value_type value_type;
         typedef std::map<MPI_Request, char*>::iterator iterator;

         //! Make an array out of the outstanding_buffers map to pass to MPI.
         void fill_request_vector();
         //! Temporary storage for MPI_Testsome and MPI_Waitall
         std::vector<MPI_Request> requests;
         //! Temporary storage for MPI_Testsome and MPI_Waitall
         std::vector<int> done_indexes;
         //! Temporary storage for MPI_Testsome and MPI_Waitall
         std::vector<MPI_Status> stats;
         //! Iterators of outstanding_buffers mapped to vector.
         std::vector<iterator> iterators;

         //! Don't copy
         Buffered_MPI_Send( const Buffered_MPI_Send& );
         //! Don't copy
         const Buffered_MPI_Send& operator=( const Buffered_MPI_Send& );
   };

}



#endif // BUFFERED_MPI_SEND_HH
