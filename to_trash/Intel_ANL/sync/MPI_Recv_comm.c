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
*                          Functional test for MPI_Recv
*
*  This test verifies MPI_Recv sorts based on the communicator
*  specified.  3 communicators are created, and an assortment of messages
*  are sent from rank 0 to rank 1, using the first 2 communicators.
*
*  Rank 1 starts a receive on comm 2 (which should not be satisfied until
*  the end), then the receives specified above.  Messages should be
*  received in the order sent for each communicator, not in the order
*  the receives were posted (which is different).
******************************************************************************/

#define  PROG_NAME   "MPI_Recv_comm"
#define  PROG_DESC   " "

#define  NUMMESG     20	/* Number of messages to send                 */
#define  NUMELM      10	/* # of elements to send and receive                 */

#include "mpitest_cfg.h"
#include "mpitest.h"

int main(int argc, char *argv[])

{
    int
        i, j,    	/* general index variable                            */
        cnt_len,        /* received length returned by MPI_Get_Count         */
        dest,    	/* Destination of send message                       */
        fail,   	/* counts total number # of failures                 */
        error,  	/* errors from one MPI call                          */
        ierr,   	/* return value from MPI calls                       */
        errorclass,	/* error class of ierr                               */
        size,   	/* length of error message                           */
        total_cnt;	/* total test count                                  */

    char
        testname[128],	/* the name of this test                             */
        info_buf[256];	/* for sprintf                                       */
    char error_string[MPI_MAX_ERROR_STRING];

    struct dataTemplate
        value;  	/* dataTemplate for initializing buffers             */

    MPI_Request
	recv_request;	/* MPI request structure		             */

    MPI_Status
	send_stat,  	/* Send Source/Tag information                       */
	recv_stat;	/* Recv Source/Tag information                       */

    MPI_Comm
	comm[3];	/* A number of communicators to sort over	     */
    int
        recv_buffer[3 * NUMMESG+1][NUMELM],	/* input to Recv             */
        send_buffer[3 * NUMMESG][NUMELM];	/* input to send             */

    char
        bsend_buff[NUMMESG * (8 * NUMELM + MPI_BSEND_OVERHEAD)];
    char *bb;

    /*-----------------------  MPI Initialization  --------------------------*/

    /* Initialize the MPI environment and test environment. */

    ierr = MPI_Init(&argc, &argv);
    if (ierr != MPI_SUCCESS)
    {
	sprintf(info_buf, "Non-zero return code (%d) from MPI_Init()", ierr);
	MPITEST_message(MPITEST_FATAL, info_buf);
    }

    sprintf(testname, "%s:  %s", PROG_NAME, PROG_DESC);

    /*--------------------  MPITEST Initialization  -------------------------*/

    MPITEST_init(argc, argv);

    if (MPITEST_me == 0)
    {
	sprintf(info_buf, "Starting:  %s ", testname);
	MPITEST_message(MPITEST_INFO0, info_buf);
    }

    total_cnt = 0;
    fail = 0;

    /* For this simple wait test we need only two nodes  */

    if (MPITEST_nump < 2)
    {
	if (MPITEST_me == 0)
	{
	    sprintf(info_buf, "At least 2 ranks required to run this test");
	    MPITEST_message(MPITEST_FATAL, info_buf);
	}
    }

    /* Create some communicators.  Use the first 2 to send some messages.
       Leave the last idle until the end
    */
    comm[0] = MPI_COMM_WORLD;
    for (i=1; i<3; i++)
    {
	ierr = MPI_Comm_dup(MPI_COMM_WORLD, &comm[i]);
	if (ierr != MPI_SUCCESS)
	{
	    sprintf(info_buf, "Non-zero return code (%d) from MPI_Comm_dup", ierr);
	    MPITEST_message(MPITEST_NONFATAL, info_buf);
	    MPI_Error_string(ierr, error_string, &size);
	    MPITEST_message(MPITEST_FATAL, error_string);
	    fail++;
	}
    }

    /*-------------------------------  Sends --------------------------------*/
    if (MPITEST_me == 0)
    {
	ierr = MPI_Buffer_attach(bsend_buff,
			       NUMMESG * (8 * NUMELM + MPI_BSEND_OVERHEAD + 100));
	if (ierr != MPI_SUCCESS)
	{
	    sprintf(info_buf, "Non-zero return code (%d) from MPI_Buffer_attach", ierr);
	    MPITEST_message(MPITEST_NONFATAL, info_buf);
	    MPI_Error_string(ierr, error_string, &size);
	    MPITEST_message(MPITEST_FATAL, error_string);
	    fail++;
	}

	/* Initialize Send buffers */

	for (i = 0; i < 3 * NUMMESG; i++)
	{
	    MPITEST_dataTemplate_init(&value, i);
	    MPITEST_init_buffer(MPITEST_int, NUMELM, value, &send_buffer[i]);

	}

	j = -1;
	for (i = 0; i < NUMMESG; i++)
	{
	    j++;
	    if (j == 2) j = 0;
	    ierr = MPI_Send(send_buffer[3 * i],
				 NUMELM,
				 MPI_INT,
				 1,
				 1,
				 comm[j]);

	    total_cnt++;
	    if (ierr != MPI_SUCCESS)
	    {
		sprintf(info_buf, "Non-zero return code (%d) from MPI_Send", ierr);
		MPITEST_message(MPITEST_NONFATAL, info_buf);
		MPI_Error_string(ierr, error_string, &size);
		MPITEST_message(MPITEST_NONFATAL, error_string);
		fail++;
	    }	/* Send  Error Test  */

	    j++;
	    if (j == 2) j = 0;
	    ierr = MPI_Bsend(send_buffer[3 * i + 1],
				  NUMELM,
				  MPI_INT,
				  1,
				  1,
				  comm[j]);

	    total_cnt++;
	    if (ierr != MPI_SUCCESS)
	    {
		sprintf(info_buf, "Non-zero return code (%d) from MPI_Bsend", ierr);
		MPITEST_message(MPITEST_NONFATAL, info_buf);
		MPI_Error_string(ierr, error_string, &size);
		MPITEST_message(MPITEST_NONFATAL, error_string);
		fail++;
	    }	/* Bsend  Error Test  */

	    j++;
	    if (j == 2) j = 0;
	    ierr = MPI_Ssend(send_buffer[3 * i + 2],
				  NUMELM,
				  MPI_INT,
				  1,
				  1,
				  comm[j]);

	    total_cnt++;
	    if (ierr != MPI_SUCCESS)
	    {
		sprintf(info_buf, "Non-zero return code (%d) from MPI_Ssend", ierr);
		MPITEST_message(MPITEST_NONFATAL, info_buf);
		MPI_Error_string(ierr, error_string, &size);
		MPITEST_message(MPITEST_NONFATAL, error_string);
		fail++;
	    }	/* Ssend  Error Test  */


	}	/* Send Loop */

	/* Wait before sending the final message on the last comm.
	*/
	MPI_Barrier(comm[2]);
	ierr = MPI_Send(send_buffer[0],
			 NUMELM,
			 MPI_INT,
			 1,
			 1,
			 comm[2]);

	total_cnt++;
	if (ierr != MPI_SUCCESS)
	{
	    sprintf(info_buf, "Non-zero return code (%d) from MPI_Send", ierr);
	    MPITEST_message(MPITEST_NONFATAL, info_buf);
	    MPI_Error_string(ierr, error_string, &size);
	    MPITEST_message(MPITEST_NONFATAL, error_string);
	    fail++;
	}	/* send  Error Test  */


	ierr = MPI_Buffer_detach(&bb, &size);
	if (ierr != MPI_SUCCESS)
	{
	    sprintf(info_buf, "Non-zero return code (%d) from MPI_Buffer_detach", ierr);
	    MPITEST_message(MPITEST_NONFATAL, info_buf);
	    MPI_Error_string(ierr, error_string, &size);
	    MPITEST_message(MPITEST_FATAL, error_string);
	    fail++;
	}

    }	/* End of sends from node 0 */

    /*--------------------------------- Receives  ---------------------------*/
    if (MPITEST_me == 1)
    {
	/* Initialize Receive buffers - start with the message to be received
           last on comm2, then do the rest
        */

	ierr = MPI_Irecv(recv_buffer[3*NUMMESG],
				 NUMELM,
				 MPI_INT,
				 0,
				 1,
				 comm[2],
				 &recv_request);

	total_cnt++;
	if (ierr != MPI_SUCCESS)
	{
	    sprintf(info_buf, "Non-zero return code (%d) from MPI_Irecv", ierr);
	    MPITEST_message(MPITEST_NONFATAL, info_buf);
	    MPI_Error_string(ierr, error_string, &size);
	    MPITEST_message(MPITEST_NONFATAL, error_string);
	    fail++;
	}	/* Irecv Error Test  */


	j = -1;
	for (i = 0; i < 3 * NUMMESG; i++)
	{
	    j++;
	    if (j == 2) j = 0;
	    MPITEST_dataTemplate_init(&value, -1);
	    MPITEST_init_buffer(MPITEST_int, NUMELM, value, &recv_buffer[i]);

	    ierr = MPI_Recv(recv_buffer[i],
				 NUMELM,
				 MPI_INT,
				 0,
				 1,
				 comm[j],
				 &recv_stat);

	    total_cnt++;
	    if (ierr != MPI_SUCCESS)
	    {
		sprintf(info_buf, "Non-zero return code (%d) from MPI_Recv for message %d", ierr, i);
		MPITEST_message(MPITEST_NONFATAL, info_buf);
		MPI_Error_string(ierr, error_string, &size);
		MPITEST_message(MPITEST_NONFATAL, error_string);
		fail++;
	    }	/* Recv Error Test  */


	    /*---------------------  Receive  Test  ----------------------*/

	    /* Set up the dataTemplate for checking the recv'd buffer. */

	    MPITEST_dataTemplate_init(&value, i);

	    error = MPITEST_buffer_errors(MPITEST_int, NUMELM, value, recv_buffer[i]);
	    if (error)
	    {
		sprintf(info_buf, "Unexpected value in buffer %d, actual =  %d    expected = %d", i, recv_buffer[i][0], i);
		MPITEST_message(MPITEST_NONFATAL, info_buf);
		fail++;
	    }

	   /* Call the MPI_Get_Count function, and compare value with NUMELM */

	    cnt_len = -1;

	    ierr = MPI_Get_count(&recv_stat, MPI_INT, &cnt_len);
	    if (ierr != MPI_SUCCESS)
	    {
		sprintf(info_buf, "Non-zero return code (%d) from MPI_Get_count", ierr);
		MPITEST_message(MPITEST_NONFATAL, info_buf);
		MPI_Error_string(ierr, error_string, &size);
		MPITEST_message(MPITEST_NONFATAL, error_string);
		fail++;
	    }
	    /*
	     * Print non-fatal error if Received length not equal to send
	     * length
	     */

	    error = NUMELM - cnt_len;

	    if (error)
	    {
		sprintf(info_buf, "send/receive lengths differ - Sender(length)=%d,  Receiver(index/length)=%d/%d", NUMELM, i, cnt_len);
		MPITEST_message(MPITEST_NONFATAL, info_buf);
		fail++;
	    }
	    /*
	     * Print non-fatal error if tag is not correct.
	     */
	    total_cnt++;
	    if (recv_stat.MPI_TAG != 1)
	    {
		sprintf(info_buf, "Unexpected tag value=%d, expected=%d",
			recv_stat.MPI_TAG, i);
		MPITEST_message(MPITEST_NONFATAL, info_buf);

		fail++;
	    }

	}	/* End of for loop: test received records */


	/* Wait here, then receive for the first recv posted on comm2 */
	MPI_Barrier(comm[2]);
	MPI_Wait(&recv_request, &recv_stat);

    }	/* End of node 1 receives  */

    if (MPITEST_me >= 2)
    {	/* rank >= 2 need to match Barrier above */
	MPI_Barrier(comm[2]);
    }

    for (j=1; j < 3; j++)
    {
	ierr = MPI_Comm_free(&comm[j]);
	if (ierr != MPI_SUCCESS)
	{
	    sprintf(info_buf, "Non-zero return code (%d) from MPI_Comm_free", ierr);
	    MPITEST_message(MPITEST_NONFATAL, info_buf);
	    MPI_Error_string(ierr, error_string, &size);
	    MPITEST_message(MPITEST_FATAL, error_string);
	    fail++;
	}
    }

    /* report overall results  */

    MPITEST_report(total_cnt - fail, fail, 0, testname);

    MPI_Finalize();
    return fail;
}/* main() */
