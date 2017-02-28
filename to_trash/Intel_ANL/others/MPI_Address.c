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
                          Test for MPI_Address()

All ranks in the current communicator used will allocate a buffer space
which size of the buffer is determined by the input length arrays (mpitest_cfg.h).
The output address will then be verified so that it will be more than or
equal to MPI_BOTTOM.

This test may be run in any communicator with a minimum of 1 group members,
with any data type, and with any non-negative message length.

The MPITEST environment provides looping over communicator size,
message length.  The properties of the loops are encoded in configuration
arrays in the file mpitest_cfg.h .

MPI Calls dependencies for this test:
  MPI_Init(), MPI_Finalize(), MPI_Address(), MPI_Error_string(),
  MPI_Barrier(),
  [MPI_Get_count(), MPI_Allreduce(), MPI_Comm_rank(), MPI_Comm_size()]

Test history:
   1  07/08/96     simont       Original version

******************************************************************************/

#include "mpitest_cfg.h"
#include "mpitest.h"

int main(int argc, char *argv[])
{
   int
     length_count,      /*  loop counter for length loop                      */
     length,            /*  The length of the current buffer                  */
     fail,              /*  counts total number of failures                   */
     size,              /*  return size from MPI_Error_string                 */
     loop_cnt,          /*  counts total number of loops through test         */
     ierr;              /*  return value from MPI calls                       */

   int i;               /*  Loop counter                                      */

   char *buffer;        /* message buffer                                     */

   MPI_Aint out_addr;   /* output address                                     */

   char
     info_buf[256],     /*  buffer for passing mesages to MPITEST             */
     testname[128];     /*  the name of this test                             */
    char error_string[MPI_MAX_ERROR_STRING];

   ierr = MPI_Init(&argc, &argv);
   if (ierr!=MPI_SUCCESS) {
      sprintf(info_buf, "MPI_Init() returned %d", ierr);
      MPITEST_message(MPITEST_FATAL, info_buf);
   }

   sprintf(testname, "MPI_Address");

   MPITEST_init(argc, argv);

   if (MPITEST_me==0) {
      sprintf(info_buf, "Starting %s test", testname);
      MPITEST_message(MPITEST_INFO0, info_buf);
   }

   /* set the global error counter */
   fail = 0;
   loop_cnt = 0;

   for (length_count=0;length_count<MPITEST_num_message_lengths();length_count++) { 
      length = MPITEST_get_message_length(length_count);

      if (length == 0) {
	 sprintf(info_buf, "Skipping length = 0");
	 MPITEST_message(MPITEST_INFO1, info_buf);
	 continue;
      }

      /* print an informational message */
      if (MPITEST_current_rank==0) {
	 sprintf(info_buf, "(%d) length %d",
		 length_count, length);
	 MPITEST_message(MPITEST_INFO1, info_buf);
      }

      /* allocate address space */
      buffer = (char *) malloc(sizeof(char) * length);
      if (!buffer) {
	 fail++;
	 sprintf(info_buf, "Cannot allocate enough buffer space of size %ld bytes",
		 length * sizeof(char));
	 MPITEST_message(MPITEST_NONFATAL, info_buf);
      }
      else {
	 loop_cnt++;

	 for (i = 0; i < length; i++) {
	    ierr = MPI_Address(&(buffer[i]), &out_addr);
	    if (ierr != MPI_SUCCESS) {
	       sprintf(info_buf, "MPI_Address() returned %d", ierr);
	       MPITEST_message(MPITEST_NONFATAL, info_buf);
	       MPI_Error_string(ierr, error_string, &size);
	       MPITEST_message(MPITEST_FATAL, error_string);
	    }
	    else {
	       if (out_addr < (MPI_Aint) MPI_BOTTOM) {
		  fail++;
		  sprintf(info_buf, "Returned address (%d) from MPI_Address is less than MPI_BOTTOM (%d)", out_addr, (int)MPI_BOTTOM);
		  MPITEST_message(MPITEST_NONFATAL, info_buf);
	       }
	    }
	 }
      }

      free(buffer);

#ifdef MPITEST_SYNC
      ierr = MPI_Barrier(MPI_COMM_WORLD);
      if (ierr != MPI_SUCCESS) {
	 sprintf(info_buf, "MPI_Barrier() returned %d", ierr);
	 MPITEST_message(MPITEST_NONFATAL, info_buf);
	 MPI_Error_string(ierr, error_string, &size);
	 MPITEST_message(MPITEST_FATAL, error_string);
      }
#endif

   }

   /* report overall results  */
   MPITEST_report(loop_cnt-fail, fail, 0, testname);

   ierr = MPI_Finalize();
   if (ierr != MPI_SUCCESS) {
      fail++;
      sprintf(info_buf, "MPI_Finalize() returned %d, FAILED", ierr);
      MPITEST_message(MPITEST_NONFATAL, info_buf);
      MPI_Error_string(ierr, error_string, &size);
      MPITEST_message(MPITEST_FATAL, error_string);
   }

   return 0;
} /* main() */

