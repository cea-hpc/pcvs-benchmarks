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
*                          Functional test for MPI_Waitsome
*
*  Waitsome references:
*    MPI Standard:  Section 3.7.5  Multiple Completions
*                   Section 3.7.4  Semantics of Nonblocking Communications
*                   Section 3.7.3  For MPI_Request_free
*  MPI_Request_free states that an ongoing communication associated with
*      the request will be allowed to complete, after which the request
*      will be deallocated.  After deallocation, the request becomes
*      equal to MPI_REQUEST_NULL
*
*  Section 3.7.5  of the MPI Standard notes that if one or more of the
*      communications completed by a call to MPI_Waitsome fail, the function
*      will return ther error code:  MPI_ERR_IN_STATUS, and will set the
*      error field of each status to a specific error code, which will
*      be MPI_SUCCESS if that specific communication was a success
*
*  This test sends messages from node 0 to node 1, and uses MPI_Waitsome
*  to check for their proper reception.  After the send the program calls
*  MPI_Request_free for two of the messages to ensure they are sent before
*  the Request Objects are freed.  This test Does a Waitsome on messages
*  that have already been Waitsomeed on.  The code verifies that a successful
*  waited on a message sets the request oobject for that message to
*  MPI_REQUEST_NULL.  The MPI_Test_cancelled function is used to test the
*  status object for the send operation, as per the MPI Standard, section 3.7.3
*
*
* Test history:
*    1  05/07/96     jh   Created
*
******************************************************************************/

#define  PROG_NAME   "MPI_Waitsome"

#define  NUMMESG    20	/* Number of messages to Isend/Irecv                 */
#define  NUMELM     10	/* # of elements to send and receive                 */

#include "mpitest_cfg.h"
#include "mpitest.h"

int main(int argc, char *argv[])

{
    int
        flag,	        /* flag return from  MPI_Test_cancelled              */
        i,j,k,l,        /* general index variables                           */
        cnt_len,	/* received length returned by MPI_Get_Count         */
        dest,	        /* Destination of Isend message                      */
        fail,     	/* counts total number # of failures                 */
        error,   	/* errors from one MPI call                          */
        ierr,   	/* return value from MPI calls                       */
        errorclass,	/* error class of ierr                               */
        size,   	/* length of error message                           */
        send_cnt,	/* number of completed operations from MPI_Waitsome  */
        recv_cnt,	/* number of completed operations from MPI_Waitsome  */
        recv_msg, total_cnt;	/* total test count                          */

    char
        testname[128],	/* the name of this test                             */
        info_buf[256];	/* for sprintf                                       */
    char error_string[MPI_MAX_ERROR_STRING];

    struct dataTemplate
        value;  	/* dataTemplate for initializing buffers             */

    MPI_Request
	send_request[4*NUMMESG],
	recv_request[4*NUMMESG];	/* MPI request structure                     */

    MPI_Status
        temp_stat[4*NUMMESG],
	send_stat[4*NUMMESG],	/* Send Source/Tag information               */
	recv_stat[4*NUMMESG];	/* Recv Source/Tag information               */

    int
        recv_buffer[4*NUMMESG][NUMELM],	/* messages to receive               */
        send_buffer[4*NUMMESG][NUMELM];	/* messages to send                  */

    int
        send_index[4*NUMMESG], recv_index[4*NUMMESG];

    char
        bsend_buff[NUMMESG * (8*NUMELM + MPI_BSEND_OVERHEAD + 100)];
    char *bb;
    /*-----------------------  MPI Initialization  --------------------------*/

    /*
     * *  Initialize the MPI environment and test environment.
     */

    ierr = MPI_Init(&argc, &argv);
    if (ierr != MPI_SUCCESS)
    {
	sprintf(info_buf, "Non-zero return code (%d) from MPI_Init()", ierr);
	MPITEST_message(MPITEST_FATAL, info_buf);
    }

    sprintf(testname, "%s ", PROG_NAME);


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

    if (MPITEST_me == 0)
    {
        ierr = MPI_Buffer_attach(bsend_buff,
                                 NUMMESG * (8*NUMELM + MPI_BSEND_OVERHEAD + 100));
        if (ierr != MPI_SUCCESS)
        {
            sprintf(info_buf, "Non-zero return code (%d) from MPI_Buffer_attach"
, ierr);
            MPITEST_message(MPITEST_NONFATAL, info_buf);
            MPI_Error_string(ierr, error_string, &size);
            MPITEST_message(MPITEST_FATAL, error_string);
            fail++;
        }

	/* Initialize Send buffers */

	for (i = 0; i < 4*NUMMESG; i++)
	{
	    MPITEST_dataTemplate_init(&value, i);
	    MPITEST_init_buffer(MPITEST_int, NUMELM, value, &send_buffer[i]);

	}
	MPI_Barrier(MPI_COMM_WORLD);

	for (i = 0; i < NUMMESG; i++)
	{
            ierr = MPI_Isend(send_buffer[4*i],
                             NUMELM,
                             MPI_INT,
                             1,
                             4*i,
                             MPI_COMM_WORLD,
                             &send_request[4*i]);

            total_cnt++;
            if (ierr != MPI_SUCCESS)
            {
                sprintf(info_buf, "Non-zero return code (%d) from MPI_Isend", ierr);
                MPITEST_message(MPITEST_NONFATAL, info_buf);
                MPI_Error_string(ierr, error_string, &size);
                MPITEST_message(MPITEST_NONFATAL, error_string);
                fail++;
            }   /* Isend  Error Test  */

            ierr = MPI_Ibsend(send_buffer[4*i+1],
                             NUMELM,
                             MPI_INT,
                             1,
                             4*i+1,
                             MPI_COMM_WORLD,
                             &send_request[4*i+1]);

            total_cnt++;
            if (ierr != MPI_SUCCESS)
            {
                sprintf(info_buf, "Non-zero return code (%d) from MPI_Ibsend", ierr);
                MPITEST_message(MPITEST_NONFATAL, info_buf);
                MPI_Error_string(ierr, error_string, &size);
                MPITEST_message(MPITEST_NONFATAL, error_string);
                fail++;
            }   /* Ibsend  Error Test  */

            ierr = MPI_Irsend(send_buffer[4*i+2],
                             NUMELM,
                             MPI_INT,
                             1,
                             4*i+2,
                             MPI_COMM_WORLD,
                             &send_request[4*i+2]);

            total_cnt++;
            if (ierr != MPI_SUCCESS)
            {
                sprintf(info_buf, "Non-zero return code (%d) from MPI_Irsend", ierr);
                MPITEST_message(MPITEST_NONFATAL, info_buf);
                MPI_Error_string(ierr, error_string, &size);
                MPITEST_message(MPITEST_NONFATAL, error_string);
                fail++;
            }   /* Irsend  Error Test  */

            ierr = MPI_Issend(send_buffer[4*i+3],
                             NUMELM,
                             MPI_INT,
                             1,
                             4*i+3,
                             MPI_COMM_WORLD,
                             &send_request[4*i+3]);

            total_cnt++;
            if (ierr != MPI_SUCCESS)
            {
                sprintf(info_buf, "Non-zero return code (%d) from MPI_Issend", ierr);
                MPITEST_message(MPITEST_NONFATAL, info_buf);
                MPI_Error_string(ierr, error_string, &size);
                MPITEST_message(MPITEST_NONFATAL, error_string);
                fail++;
            }   /* Issend  Error Test  */

	}	/* Send Loop */

	/*--------------------- Call MPI_Request_free -----------------------*/

	ierr = MPI_Request_free(&send_request[4]);
	total_cnt++;
	if (ierr != MPI_SUCCESS)
	{
	    sprintf(info_buf, "Non-zero return code (%d) from MPI_Request_free", ierr);
	    MPITEST_message(MPITEST_NONFATAL, info_buf);
	    MPI_Error_string(ierr, error_string, &size);
	    MPITEST_message(MPITEST_NONFATAL, error_string);
	    fail++;
	}

	total_cnt++;
	if (send_request[4] != MPI_REQUEST_NULL)
	{
	    sprintf(info_buf, "request not equal to  MPI_REQUEST_NULL after calling MPI_Request_free");
	    MPITEST_message(MPITEST_NONFATAL, info_buf);
	    MPI_Error_string(ierr, error_string, &size);
	    MPITEST_message(MPITEST_NONFATAL, error_string);
	    fail++;
	}

	/*--------------------- Call MPI_Request_free -----------------------*/

	ierr = MPI_Request_free(&send_request[8]);
	total_cnt++;
	if (ierr != MPI_SUCCESS)
	{
	    sprintf(info_buf, "Non-zero return code (%d) from MPI_Request_free", ierr);
	    MPITEST_message(MPITEST_NONFATAL, info_buf);
	    MPI_Error_string(ierr, error_string, &size);
	    MPITEST_message(MPITEST_NONFATAL, error_string);
	    fail++;
	}

	total_cnt++;
	if (send_request[8] != MPI_REQUEST_NULL)
	{
	    sprintf(info_buf, "request not equal to  MPI_REQUEST_NULL after calling MPI_Request_free");
	    MPITEST_message(MPITEST_NONFATAL, info_buf);
	    MPI_Error_string(ierr, error_string, &size);
	    MPITEST_message(MPITEST_NONFATAL, error_string);
	    fail++;
	}

	/*----------------  Waitsome for all messages to send ------------*/

	i = 0;
	send_cnt = 0;

	/*
	 * You will only get valid Wait status for the number of messages sent
	 * - the two messages whose requests were freed
	 */

	/* 11-11-02 : The two freed requests cannot be waited on, since
	   they have been replaced with MPI_REQUEST_NULL.
	*/
	while (send_cnt != MPI_UNDEFINED)
	{
	    ierr = MPI_Waitsome(4*NUMMESG, send_request, &send_cnt, send_index, temp_stat);
	    total_cnt++;
	    i++;
	    sprintf(info_buf, "Waitsome on send: Loop  %d   Index_cnt %d   Error %d", i, send_cnt, ierr);
	    MPITEST_message(MPITEST_INFO1, info_buf);

	    for (k=0; k < send_cnt; k++)
	    {
		l = send_index[k];
		send_stat[l] = temp_stat[k];
	    }

	    if (ierr != MPI_SUCCESS)
	    {
		sprintf(info_buf, "Non-zero return code (%d) from MPI_Waitsome on send", ierr);
		MPITEST_message(MPITEST_NONFATAL, info_buf);
		MPI_Error_string(ierr, error_string, &size);
		MPITEST_message(MPITEST_NONFATAL, error_string);
		fail++;

	    }	/* End of Waitsome error test for send */

	}	/* End Waitsome while loop  */

	/* At this point all messages that were sent have been Waitsome ed on */

	for (i = 0; i < 4*NUMMESG; i++)
	{
	    /* Skip the two requests that were freed.  They'll never
	       be set */
	    if (i == 4 || i == 8) continue;
	    total_cnt++;
	    ierr = MPI_Test_cancelled(&send_stat[i], &flag);
	    if (ierr != MPI_SUCCESS)
	    {
		sprintf(info_buf, "Non-zero return code (%d) from MPI_Test_cancelled", ierr);
		MPITEST_message(MPITEST_NONFATAL, info_buf);
		MPI_Error_string(ierr, error_string, &size);
		MPITEST_message(MPITEST_NONFATAL, error_string);
		fail++;
	    }	/* MPI_Test_cancelled Error Test */

	    total_cnt++;
	    if (flag != FALSE)
	    {

		sprintf(info_buf, "MPI_Test_cancelled flag set,  record = %d,  flag = %d", i, flag);
		MPITEST_message(MPITEST_NONFATAL, info_buf);
		MPI_Error_string(ierr, error_string, &size);
		MPITEST_message(MPITEST_NONFATAL, error_string);
		fail++;
	    }	/* MPI_Test_cancelled Error Test */
	}	/* Test_cancelled loop  */

        ierr = MPI_Buffer_detach(&bb, &size);
        if (ierr != MPI_SUCCESS)
        {
            sprintf(info_buf, "Non-zero return code (%d) from MPI_Buffer_detach"
, ierr);
            MPITEST_message(MPITEST_NONFATAL, info_buf);
            MPI_Error_string(ierr, error_string, &size);
            MPITEST_message(MPITEST_FATAL, error_string);
            fail++;
        }

    }	/* Isend from node 0 */

    /*---------------------------------  Irecv  -----------------------------*/
    if (MPITEST_me == 1)
    {
	/* Initialize Receive  buffers */
	for (i = 0; i < 4*NUMMESG; i++)
	{
	    MPITEST_dataTemplate_init(&value, -1);
	    MPITEST_init_buffer(MPITEST_int, NUMELM, value, &recv_buffer[i]);
	}

	for (i = 0; i < 4*NUMMESG; i++)
	{
	    ierr = MPI_Irecv(recv_buffer[i],
			     NUMELM,
			     MPI_INT,
			     0,
			     i,
			     MPI_COMM_WORLD,
			     &recv_request[i]);

	    total_cnt++;
	    if (ierr != MPI_SUCCESS)
	    {
		sprintf(info_buf, "Non-zero return code (%d) from MPI_Irecv", ierr);
		MPITEST_message(MPITEST_NONFATAL, info_buf);
		MPI_Error_string(ierr, error_string, &size);
		MPITEST_message(MPITEST_NONFATAL, error_string);
		fail++;
	    }	/* Irecv Error Test  */

	}	/* Receive Loop  */
	MPI_Barrier(MPI_COMM_WORLD);

	/*---------------------  First Irecv  Waitsome  --------------------*/
	recv_cnt = 0;
	j = 0;
	while ( recv_cnt != MPI_UNDEFINED)
	{
	    ierr = MPI_Waitsome(4*NUMMESG, recv_request, &recv_cnt, recv_index, temp_stat);
	    j++;
	    total_cnt++;
	    sprintf(info_buf, "Waitsome on receive: Loop %d  recv_cnt %d  Error %d ", i, recv_cnt, ierr);
	    MPITEST_message(MPITEST_INFO1, info_buf);
	    for (k=0; k < recv_cnt; k++)
	    {
		l = recv_index[k];
		recv_stat[l] = temp_stat[k];
	    }

	    if (ierr != MPI_SUCCESS)
	    {
		fail++;
		sprintf(info_buf, "Non-zero return code (%d) from MPI_Waitsome on receive", ierr);
		MPITEST_message(MPITEST_NONFATAL, info_buf);
		MPI_Error_string(ierr, error_string, &size);
		MPITEST_message(MPITEST_NONFATAL, error_string);

		/* If error is #17, MPI_ERR_IN_STATUS print out status  */

		MPI_Error_class(ierr, &errorclass);
		if (errorclass == MPI_ERR_IN_STATUS)
		{
		    sprintf(info_buf, "MPI_ERR_IN_STATUS on receive, printing non-zero statuses");
		    MPITEST_message(MPITEST_NONFATAL, info_buf);
		    for (i = 0; i < 4*NUMMESG; i++)
		    {
			total_cnt++;
			ierr = recv_stat[i].MPI_ERROR;
			if (ierr != MPI_SUCCESS)
			{
			    sprintf(info_buf, "Error in received record %d, Source = %d,  Tag =  %d,  Error # =  %d", i, recv_stat[i].MPI_SOURCE, recv_stat[i].MPI_TAG, recv_stat[i].MPI_ERROR);
			    MPITEST_message(MPITEST_NONFATAL, info_buf);
			    MPI_Error_string(recv_stat[i].MPI_ERROR, error_string, &size);
			    MPITEST_message(MPITEST_NONFATAL, error_string);
			    fail++;
			}
		    }	/* End of MPI_ERR_IN_STATUS  for loop printing */

		}	/* End of MPI_ERR_IN_STATUS check */

	    }	/* End of Waitsome error test for receive */
	}	/* End of Waitsome loop over messages received  */

	/*
	 * At this point all messages that were received have been Waitsome ed
	 * on
	 */

	for (i = 0; i < 4*NUMMESG; i++)
	{

	    /*
	     * Set up the dataTemplate for checking the recv'd buffer.  Note
	     * that the sending record number  will be sent.
	     */
	    MPITEST_dataTemplate_init(&value, i);

	    total_cnt++;

	    error = MPITEST_buffer_errors(MPITEST_int, NUMELM, value, recv_buffer[i]);
	    if (error)
	    {
		sprintf(info_buf, "Unexpected value in buffer %d, actual =  %d    expected = %d", i, recv_buffer[i][0], i);
		MPITEST_message(MPITEST_NONFATAL, info_buf);
		fail++;
	    }

	    /*
	     * Call the MPI_Get_Count function, and compare value with NUMELM
	     */
	    cnt_len = -1;

	    total_cnt++;
	    ierr = MPI_Get_count(&recv_stat[i], MPI_INT, &cnt_len);
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
	    total_cnt++;

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
	    if (recv_stat[i].MPI_TAG != i)
	    {
		sprintf(info_buf, "Unexpected tag value=%d, expected=%d",
			recv_stat[i].MPI_TAG, i);
		MPITEST_message(MPITEST_NONFATAL, info_buf);
		fail++;
	    }

	}	/* End of for loop: test received records  */

		/*-------------------  Second  Waitsome  ------------------- */
	ierr = MPI_Waitsome(4*NUMMESG, recv_request, &recv_cnt, recv_index, recv_stat);

	sprintf(info_buf, "Waitsome on receive: Loop %d  recv_cnt %d  Error %d ", i, recv_cnt, ierr);
	MPITEST_message(MPITEST_INFO1, info_buf);

	total_cnt++;
	if (ierr != MPI_SUCCESS)
	{
	    sprintf(info_buf, "Non-zero return code (%d) from second MPI_Waitsome on receive", ierr);
	    MPITEST_message(MPITEST_NONFATAL, info_buf);
	    sprintf(info_buf, "ierr = %d, errorclass = %d", ierr, errorclass);
	    MPITEST_message(MPITEST_INFO2, info_buf);
	    MPI_Error_string(ierr, error_string, &size);
	    MPITEST_message(MPITEST_INFO1, error_string);
	    fail++;
	}

	for (i = 0; i < 4*NUMMESG; i++)
	{
	    if (recv_request[i] != MPI_REQUEST_NULL)
	    {
		sprintf(info_buf, "recv_request[%d] not equal to  MPI_REQUEST_NULL(%d/%d) after second Waitsome", i, recv_request[i], MPI_REQUEST_NULL);
		MPITEST_message(MPITEST_NONFATAL, info_buf);
		MPI_Error_string(ierr, error_string, &size);
		MPITEST_message(MPITEST_NONFATAL, error_string);
		fail++;
	    }
	}

    }	/* End of node 1 receives  */

    if (MPITEST_me >= 2)
    {	/* rank >= 2 need to match Barrier above */
	MPI_Barrier(MPI_COMM_WORLD);
    }

    /*
     * All nodes stop here so we can finish all sends and receives before we
     * finalize the run
     */
    MPI_Barrier(MPI_COMM_WORLD);

    /* report overall results  */

    MPITEST_report(total_cnt - fail, fail, 0, testname);

    MPI_Finalize();
    return fail;
}/* main() */
