/*-----------------------------------------------------------------------------
MESSAGE PASSING INTERFACE TEST CASE SUITE

Copyright - 1996 Intel Corporation

Intel Corporation hereby grants a non-exclusive license under Intel's
copyright to copy, modify and distribute this software for any purpose
and without fee, provided that the above copyright notice and the following
paragraphs appear on all copies.

Intel Corporation makes no representation that the test cases comprising
this suite are correct or are an accurate representation of any standard.

IN NO EVENT SHALL INTEL HAVE ANY LIABILITY FOR ANY DIRECT, INDIRECT OR
SPECULATIVE DAMAGES, (INCLUDING WITHOUT LIMITING THE FOREGOING, CONSEQUENTIAL,
INCIDENTAL AND SPECIAL DAMAGES) INCLUDING, BUT NOT LIMITED TO INFRINGEMENT,
LOSS OF USE, BUSINESS INTERRUPTIONS, AND LOSS OF PROFITS, IRRESPECTIVE OF
WHETHER INTEL HAS ADVANCE NOTICE OF THE POSSIBILITY OF ANY SUCH DAMAGES.

INTEL CORPORATION SPECIFICALLY DISCLAIMS ANY WARRANTIES INCLUDING, BUT NOT
LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY, FITNESS FOR A
PARTICULAR PURPOSE AND NON-INFRINGEMENT.  THE SOFTWARE PROVIDED HEREUNDER
IS ON AN "AS IS" BASIS AND INTEL CORPORATION HAS NO OBLIGATION TO PROVIDE
MAINTENANCE, SUPPORT, UPDATES, ENHANCEMENTS OR MODIFICATIONS.
-----------------------------------------------------------------------------*/
/******************************************************************************
                          Test Template
******************************************************************************/

#include "mpitest_cfg.h"
#include "mpitest.h"


main(int argc, char *argv[])
{
   int
/* For configured data types: */
      test_type,		/* the index of the current buffer type      */
      type_count,		/* loop counter for data type loop           */

/* For configured data lengths: */
      length,			/* The length of the current buffer          */
      byte_length,		/* The length of the current buffer in bytes */
      length_count,		/* loop counter for message length loop      */
      max_length,		/* maximum buffer length specified in config.
				 * file   */
      max_byte_length,		/* maximum buffer length in bytes            */

/* For configured communicators: */
      test_nump,		/* The number of processors in current
				 * communicator  */
      comm_index,		/* the array index of the current comm       */
      comm_type,		/* the index of the current communicator type*/
      comm_count;		/* loop counter for communicator loop        */
   MPI_Comm comm;		/* MPI communicator                          */

   int
      error,			/* errors from one MPI call                  */
      pass,    			/* counts total number of tests PASSED       */
      fail,			/* counts total number of failures           */
      size,			/* return size from MPI_Error_string         */
      ierr,			/* return value from MPI calls               */
      i, j;

   struct dataTemplate
      value;			/* dataTemplate for initializing buffers     */

   void *send_buffer;		/* message buffer                            */

   char
      info_buf[256],		/* buffer for passing mesages to MPITEST     */
      testname[128];		/* the name of this test                     */
    char error_string[MPI_MAX_ERROR_STRING];


   ierr = MPI_Init(&argc, &argv);
   if (ierr != MPI_SUCCESS) {
      sprintf(info_buf, "MPI_Init() returned %d", ierr);
      MPITEST_message(MPITEST_FATAL, info_buf);
		  /* Fatal errors will not return */
   }

   sprintf(testname, "Template");

   /* Initialize the library */
   MPITEST_init(argc, argv);

   /* MPITEST_me is my rank in MPI_COMM_WORLD
      MPITEST_NUMP is size of MPI_COMM_WORLD
   */
   if (MPITEST_me == 0) {
      sprintf(info_buf, "Starting %s test", testname);
      MPITEST_message(MPITEST_INFO0, info_buf); /* _INFO0 is always printed */
   }

   /* set the global error counter */
   fail = 0;
   pass = 0;

   /* find the maximum sized buffer we will use */
   max_byte_length = MPITEST_get_max_message_length();

   /* Loop through configured communicators */
   for (comm_count = 0; comm_count < MPITEST_num_comm_sizes(); comm_count++) {
      comm_index = MPITEST_get_comm_index(comm_count);
      comm_type = MPITEST_get_comm_type(comm_count);

      test_nump = MPITEST_get_communicator(comm_type, comm_index, &comm);
      /* MPITEST_current_rank is rank in comm, or MPI_UNDEFINED */

      /* Loop through data types */
      for (type_count = 0; type_count < MPITEST_num_datatypes(); type_count++) {
	 test_type = MPITEST_get_datatype(type_count);


	 /* convert the number of bytes in the maximum length message */
	 /* into the number of elements of the current type */
	 max_length = MPITEST_byte_to_element(test_type, max_byte_length);

	 /* then allocate the buffer - this does a malloc */
	 MPITEST_get_buffer(test_type, max_length, &send_buffer);

	 /* Loop through data lengths */
	 for (length_count = 0; length_count < MPITEST_num_message_lengths(); length_count++) {
	    byte_length = MPITEST_get_message_length(length_count);
	    length = MPITEST_byte_to_element(test_type, byte_length);

	    /* Set up the dataTemplate for initializing send buffer */
	    MPITEST_dataTemplate_init(&value, MPITEST_current_rank);

	    /* Initialize send buffer */
	    MPITEST_init_buffer(test_type, length + 1,
				      value, send_buffer);


/* Make the MPI calls here For example:
		  ierr = MPI_Bcast(send_buffer, length,
				   MPITEST_mpi_datatypes[test_type],
				   root, comm);

		  if (ierr != MPI_SUCCESS) {
		     sprintf(info_buf, "MPI_Bcast() returned %d", ierr);
		     MPITEST_message(MPITEST_NONFATAL, info_buf);
		     MPI_Error_string(ierr, &info_buf[0], &size);
		     MPITEST_message(MPITEST_FATAL, info_buf);
		     fail++;
		  }
*/
	    /*
	     * Set up the dataTemplate for checking the received
	     * buffer.
	     */
	    MPITEST_dataTemplate_init(&value, MPITEST_current_rank);

	    error = MPITEST_buffer_errors(test_type, length, value, send_buffer);

	    /* check for receive buffer overflow */
	    MPITEST_dataTemplate_init(&value, MPITEST_current_rank);
	    error += MPITEST_buffer_errors_ov(test_type,
					length, value, send_buffer);

/* Error contains the number of data errors found, plus one byte later (in
   case of overflow */
	    if (error) {
	       if (ierr == MPI_SUCCESS)
		  fail++;
	       sprintf(info_buf, "%d errors in buffer (%d,%d,%d) len %d commsize %d commtype %d data_type %d",
			     error, length_count, comm_count, type_count,
			     length, test_nump, comm_type, test_type);
	       MPITEST_message(MPITEST_NONFATAL, info_buf);
	    }
	    else {
	       pass++;
	       sprintf(info_buf, "%d errors found in buffer", error);
	       MPITEST_message(MPITEST_INFO2, info_buf);
		   /* _INFO1 & _INFO2 correspond to -verbose options */
	    }

	 } /* length loop */
	 free(send_buffer);
      } /* data type loop */

      /* comm may be a pre-defined communicator (MPI_COMM_WORLD) or
	 MPI_COMM_NULL.  Let the library figure it out.  */
      MPITEST_free_communicator(comm_type, &comm);
   } /* communicator loop */
   /* report overall results  */
   MPITEST_report(pass, fail, 0, testname);
     /* Third parm is Manuall Verify */

   ierr = MPI_Finalize();
   return 0;
}				/* main() */
