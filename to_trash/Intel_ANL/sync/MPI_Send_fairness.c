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
		  Test for MPI_Send() 

This test is for informational purposes only.  All ranks but 0 send NUM_MSG
messages to rank 0; which receives them using MPI_ANY_SOURCE.  A simple
histogram is printed after every NUM_MSG receives, indicating from which
rank messages were received.  MPI does not specify how this program should
behave (as long as all the messages are received) but this does represent
some interesting data about the underlying "fairness" of the underlying
protocols.
******************************************************************************/

#include "mpitest_cfg.h"
#include "mpitest.h"

#define MSG_SIZE 10
#define NUM_MSG 200

int main(int argc, char *argv[])
{

    int
	buffer[10],		/* Message buffer 			 */
       *recv_counts,		/* Number of messages from each sender   */
	ierr,
	i,j;

    char
        info_buf[256],		/* buffer for passing mesages to MPITEST */
        testname[64];		/* the name of this test                 */
    char error_string[MPI_MAX_ERROR_STRING];

    MPI_Status
	recv_stat;

    ierr = MPI_Init(&argc, &argv);
    if (ierr != MPI_SUCCESS)
    {
	sprintf(info_buf, "Non-zero return code (%d) from MPI_Init()", ierr);
	MPITEST_message(MPITEST_FATAL, info_buf);
    }

    sprintf(testname, "MPI_Send_fairness");

    MPITEST_init(argc, argv);
    if (MPITEST_me == 0)
    {
	sprintf(info_buf, "Starting %s test", testname);
	MPITEST_message(MPITEST_INFO0, info_buf);
    }

    if (MPITEST_nump < 2)
    {
	sprintf(info_buf, "At least 2 ranks required to run this test");
	MPITEST_message(MPITEST_FATAL, info_buf);
    }

    if (MPITEST_me == 0)
    {
	recv_counts = (int *)malloc(MPITEST_nump * sizeof(int));
	for (j=1; j<MPITEST_nump; j++) recv_counts[j] = 0;

	MPI_Barrier(MPI_COMM_WORLD);

	for (i=1; i<MPITEST_nump; i++)
	{
	    for (j=1; j<=NUM_MSG; j++) 
	    {
		MPI_Recv(buffer, MSG_SIZE, MPI_INT, MPI_ANY_SOURCE,
			MPI_ANY_TAG, MPI_COMM_WORLD, &recv_stat);
		recv_counts[recv_stat.MPI_SOURCE]++;
	    }
	    sprintf(info_buf, "For messages %d-%d, rank 0 received:", 
		(i-1) * NUM_MSG + 1, i * NUM_MSG);
	    MPITEST_message(MPITEST_INFO0, info_buf);
	    for (j=1; j<MPITEST_nump; j++) 
	    {
		sprintf(info_buf, " %3d from rank %3d (%3d%%)", recv_counts[j],
			j, (100 * recv_counts[j])/NUM_MSG);
		MPITEST_message(MPITEST_INFO0, info_buf);
		recv_counts[j]=0;
	    }
	}
    }
    else
    {
	MPI_Barrier(MPI_COMM_WORLD);

	for (i=1; i<=NUM_MSG; i++)
	    MPI_Send(buffer, MSG_SIZE, MPI_INT, 0, i, MPI_COMM_WORLD);
    }

    /* report overall results  */

    MPITEST_report(0, 0, 1, testname);

    MPI_Finalize();

    return 0;

}/* main() */
