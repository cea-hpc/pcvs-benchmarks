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
/***************************************************************************
                   Test for MPI_Request_free()

 This test verifies that MPI_Request_free can be used on each of the basic
 persistent calls.  Requests are expected to complete even though they are
 freed.

 This test uses the first 2 ranks in MPI_COMM_WORLD, with rank 0 making
 the non-blocking and free calls and rank1 as the destination.
******************************************************************************/

#include "mpitest_cfg.h"
#include "mpitest.h"

int main(int argc, char *argv[])
{

    int
        fail   ,	/* counts total number of failures               */
        loop_fail = 0,
        i,       	/* utility loop index variables                  */
        ierr,   	/* return value from MPI calls                   */
        loop_cnt,	/* counts total number of loops through test     */
        max_byte_length,/* maximum buffer length in bytes                */
        size;   	/* return size from MPI_Error_string             */


    int Send_buffer[10],	/* message buffer                        */
        Bsend_buffer[10],
        Rsend_buffer[10], 
        Ssend_buffer[10], 
        Isend_buffer[10], 
        recv_buffer[5];

    char
        info_buf[256],	/* buffer for passing mesages to MPITEST         */
        testname[64];	/* the name of this test                         */
    char error_string[MPI_MAX_ERROR_STRING];

    MPI_Request
	send_request,	/* MPI request structure                         */
	recv_request[5];

    MPI_Status
	send_stat,	/* MPI  status structure                         */
	recv_stat[5];
    char *buff; 	/* Ibsend message buffer                         */

    int bsize,  	/* Ibsend message buffer size                    */
        buffsize,	/* Size of malloc buffer for Ibsend              */
        berr;   	/* error flag for buffer ATTACH and DETACH       */


    /*-----------------------------  MPI_Init  ------------------------------*/
    ierr = MPI_Init(&argc, &argv);
    if (ierr != MPI_SUCCESS)
    {
	sprintf(info_buf, "Non-zero return code (%d) from MPI_Init()", ierr);
	MPITEST_message(MPITEST_FATAL, info_buf);
    }


    sprintf(testname, "MPI_Request_free_p");

    /*-----------------------------  MPITEST_init  --------------------------*/
    MPITEST_init(argc, argv);
    if (MPITEST_me == 0)
    {
	sprintf(info_buf, "Starting %s test", testname);
	MPITEST_message(MPITEST_INFO0, info_buf);
    }

    /* set the global error counter */
    fail = 0;
    loop_cnt = 0;

    max_byte_length = MPITEST_get_max_message_length();


    /* We need at least two nodes to run this test  */

    if (MPITEST_nump < 2)
    {
	if (MPITEST_me == 0)
	{
	    sprintf(info_buf, "At least 2 ranks required to run this test");
	    MPITEST_message(MPITEST_FATAL, info_buf);
	}
    }

    if (MPITEST_me < 2)
    {
	if (MPITEST_me == 0)
	{

	    MPI_Barrier(MPI_COMM_WORLD);


	    /*------------- Allocate buffer for Ibsend --------------*/

	    buffsize = 2000+MPI_BSEND_OVERHEAD;
	    buff = malloc(buffsize * sizeof(int));
	    if (!buff)
	    {
		sprintf(info_buf, "Malloc request failed");
		MPITEST_message(MPITEST_FATAL, info_buf);
	    }
	    berr = MPI_Buffer_attach(buff, buffsize);
	    if (berr != MPI_SUCCESS)
	    {
		sprintf(info_buf, "Non-zero return code (%d) from MPI_Buffer_attach", berr);
		MPITEST_message(MPITEST_FATAL, info_buf);
	    }
	    /*----------------------  Send_init  -------------------------*/
	    loop_cnt++;

	    ierr = MPI_Send_init(Send_buffer,
				 1,
				 MPI_INT,
				 1,
				 0,
				 MPI_COMM_WORLD,
				 &send_request);

	    if (ierr != MPI_SUCCESS)
	    {
		sprintf(info_buf, "Non-zero return code (%d) from MPI_Send_init", ierr);
		MPITEST_message(MPITEST_NONFATAL, info_buf);
		MPI_Error_string(ierr, error_string, &size);
		MPITEST_message(MPITEST_NONFATAL, error_string);
		fail++;
	    }	/* Isend Error Test  */

	    ierr = MPI_Start(&send_request);
	    if (ierr != MPI_SUCCESS)
	    {
		sprintf(info_buf, "Non-zero return code (%d) from MPI_Start for Send ", ierr);
		MPITEST_message(MPITEST_NONFATAL,
				info_buf);
		MPI_Error_string(ierr, &info_buf[0],
				 &size);
		MPITEST_message(MPITEST_NONFATAL,
				info_buf);
		fail++;
	    }	/* Start  Error Test  */



	    /*----------------- Call MPI_Request_free ------------------*/

	    ierr = MPI_Request_free(&send_request);
	    if (ierr != MPI_SUCCESS)
	    {
		sprintf(info_buf, "Non-zero return code (%d) from MPI_Request_free for Send", ierr);
		MPITEST_message(MPITEST_NONFATAL, info_buf);
		MPI_Error_string(ierr, error_string, &size);
		MPITEST_message(MPITEST_NONFATAL, error_string);
		fail++;
	    }

	    if (send_request != MPI_REQUEST_NULL)
	    {
		sprintf(info_buf, "request not equal to  MPI_REQUEST_NULL after calling MPI_Request_free (Send)");
		MPITEST_message(MPITEST_NONFATAL, info_buf);
		MPI_Error_string(ierr, error_string, &size);
		MPITEST_message(MPITEST_NONFATAL, error_string);
		fail++;
	    }

	    /*----------------------  Bsend_init  ---------------------*/
	    loop_cnt++;

	    ierr = MPI_Bsend_init(Bsend_buffer,
				  1,
				  MPI_INT,
				  1,
				  1,
				  MPI_COMM_WORLD,
				  &send_request);

	    if (ierr != MPI_SUCCESS)
	    {
		sprintf(info_buf, "Non-zero return code (%d) from MPI_Bsend_init", ierr);
		MPITEST_message(MPITEST_NONFATAL, info_buf);
		MPI_Error_string(ierr, error_string, &size);
		MPITEST_message(MPITEST_NONFATAL, error_string);
		fail++;
	    }	/* Bsend_init Error Test  */

	    ierr = MPI_Start(&send_request);

	    if (ierr != MPI_SUCCESS)
	    {
		sprintf(info_buf, "Non-zero return code (%d) from MPI_Start for Bsend", ierr);
		MPITEST_message(MPITEST_NONFATAL, info_buf);
		MPI_Error_string(ierr, error_string, &size);
		MPITEST_message(MPITEST_NONFATAL, error_string);
		fail++;
	    }	/* Start  Error Test  */


	    /*-------- Call MPI_Request_free -------*/

	    ierr = MPI_Request_free(&send_request);
	    if (ierr != MPI_SUCCESS)
	    {
		sprintf(info_buf, "Non-zero return code (%d) from MPI_Request_free for Bsend", ierr);
		MPITEST_message(MPITEST_NONFATAL, info_buf);
		MPI_Error_string(ierr, error_string, &size);
		MPITEST_message(MPITEST_NONFATAL, error_string);
		fail++;
	    }

	    /*---------------------  Ssend_init  ----------------------*/
	    loop_cnt++;


	    ierr = MPI_Ssend_init(Ssend_buffer,
				  1,
				  MPI_INT,
				  1,
				  2,
				  MPI_COMM_WORLD,
				  &send_request);

	    if (ierr != MPI_SUCCESS)
	    {
		sprintf(info_buf, "Non-zero return code (%d) from MPI_Ssend_init", ierr);
		MPITEST_message(MPITEST_NONFATAL,
				info_buf);
		MPI_Error_string(ierr, &info_buf[0],
				 &size);
		MPITEST_message(MPITEST_NONFATAL,
				info_buf);
		loop_fail++;
	    }	/* Issend Error Test  */


	    ierr = MPI_Start(&send_request);
	    if (ierr != MPI_SUCCESS)
	    {
		sprintf(info_buf, "Non-zero return code (%d) from MPI_Start for send", ierr);
		MPITEST_message(MPITEST_NONFATAL,
				info_buf);
		MPI_Error_string(ierr, &info_buf[0],
				 &size);
		MPITEST_message(MPITEST_NONFATAL,
				info_buf);
		loop_fail++;
	    }	/* Start  Error Test  */


	    /*--------- Call MPI_Request_free -------*/

	    ierr = MPI_Request_free(&send_request);
	    if (ierr != MPI_SUCCESS)
	    {
		sprintf(info_buf, "Non-zero return code (%d) from MPI_Request_free for Ssend", ierr);
		MPITEST_message(MPITEST_NONFATAL, info_buf);
		MPI_Error_string(ierr, error_string, &size);
		MPITEST_message(MPITEST_NONFATAL, error_string);
		fail++;
	    }

	    if (send_request != MPI_REQUEST_NULL)
	    {
		sprintf(info_buf, "request not equal to  MPI_REQUEST_NULL after calling MPI_Request_free (Ssend)");
		MPITEST_message(MPITEST_NONFATAL, info_buf);
		MPI_Error_string(ierr, error_string, &size);
		MPITEST_message(MPITEST_NONFATAL, error_string);
		fail++;
	    }

	    /*------------------------  Rsend_init  -------------------------*/
	    loop_cnt++;


	    ierr = MPI_Rsend_init(Rsend_buffer,
				  1,
				  MPI_INT,
				  1,
				  3,
				  MPI_COMM_WORLD,
				  &send_request);

	    if (ierr != MPI_SUCCESS)
	    {
		sprintf(info_buf, "Non-zero return code (%d) from MPI_Rend_init", ierr);
		MPITEST_message(MPITEST_NONFATAL, info_buf);
		MPI_Error_string(ierr, error_string, &size);
		MPITEST_message(MPITEST_NONFATAL, error_string);
		fail++;
	    }	/* Rsend_init Error Test  */

	    /*---------  Start the Rsend  ----------*/


	    ierr = MPI_Start(&send_request);

	    if (ierr != MPI_SUCCESS)
	    {
		sprintf(info_buf, "Non-zero return code (%d) from MPI_Start for Rsend", ierr);
		MPITEST_message(MPITEST_NONFATAL, info_buf);
		MPI_Error_string(ierr, error_string, &size);
		MPITEST_message(MPITEST_NONFATAL, error_string);
		fail++;
	    }	/* Start  Error Test  */


	    /*------- Call MPI_Request_free ------*/

	    ierr = MPI_Request_free(&send_request);
	    if (ierr != MPI_SUCCESS)
	    {
		sprintf(info_buf, "Non-zero return code (%d) from MPI_Request_free after Rsend", ierr);
		MPITEST_message(MPITEST_NONFATAL, info_buf);
		MPI_Error_string(ierr, error_string, &size);
		MPITEST_message(MPITEST_NONFATAL, error_string);
		fail++;
	    }

	    if (send_request != MPI_REQUEST_NULL)
	    {
		sprintf(info_buf, "request not equal to  MPI_REQUEST_NULL after calling MPI_Request_free");
		MPITEST_message(MPITEST_NONFATAL, info_buf);
		MPI_Error_string(ierr, error_string, &size);
		MPITEST_message(MPITEST_NONFATAL, error_string);
		fail++;
	    }
	    /*------------------------  Isend  --------------------------*/

	    loop_cnt++;


	    ierr = MPI_Isend(Isend_buffer,
			     1,
			     MPI_INT,
			     1,
			     4,
			     MPI_COMM_WORLD,
			     &send_request);

	    if (ierr != MPI_SUCCESS)
	    {
		sprintf(info_buf, "Non-zero return code (%d) from MPI_Isend", ierr);
		MPITEST_message(MPITEST_NONFATAL,
				info_buf);
		MPI_Error_string(ierr, &info_buf[0],
				 &size);
		MPITEST_message(MPITEST_NONFATAL,
				info_buf);
		loop_fail++;
	    }	/* Isend Error Test  */


	    ierr = MPI_Wait(&send_request, &send_stat);

	    if (ierr != MPI_SUCCESS)
	    {
		sprintf(info_buf, "Non-zero return code (%d) from MPI_Wait on Isend", ierr);
		MPITEST_message(MPITEST_NONFATAL, info_buf);
		MPI_Error_string(ierr, error_string, &size);
		MPITEST_message(MPITEST_NONFATAL, error_string);
		loop_fail++;
	    }	/* Wait on Isend Error Test  */
	} /* End of node 0 send code */

	else  /*  Start node 1 receive code */
	{


/*#############################  RECEIVERS  ###############################*/

	    for (i = 0; i < 4; i++)
	    {
		loop_cnt++;


		ierr = MPI_Irecv(&recv_buffer[i],
				 1,
				 MPI_INT,
				 0,
				 i,
				 MPI_COMM_WORLD,
				 &recv_request[i]);

		if (ierr != MPI_SUCCESS)
		{
		    sprintf(info_buf, "Non-zero return code (%d) from MPI_Irecv", ierr);
		    MPITEST_message(MPITEST_NONFATAL, info_buf);
		    MPI_Error_string(ierr, error_string, &size);
		    MPITEST_message(MPITEST_NONFATAL, error_string);
		    loop_fail++;
		}	/* End of Irecv Error Test  */

	    }	/* End of Irecv loop  */


	    /*-----  Set up Receive Initialization  -----*/

	    ierr = MPI_Recv_init(&recv_buffer[4],
				 1,
				 MPI_INT,
				 0,
				 4,
				 MPI_COMM_WORLD,
				 &recv_request[4]);

	    if (ierr != MPI_SUCCESS)
	    {
		sprintf(info_buf, "Non-zero return code (%d) from MPI_Recv_init", ierr);
		MPITEST_message(MPITEST_NONFATAL, info_buf);
		MPI_Error_string(ierr, error_string, &size);
		MPITEST_message(MPITEST_NONFATAL, error_string);
		loop_fail++;
	    }	/* Error Test  */


	    /* Start the Receive */

	    ierr = MPI_Start(&recv_request[4]);

	    if (ierr != MPI_SUCCESS)
	    {
		sprintf(info_buf, "Non-zero return code (%d) from MPI_Start for Recv", ierr);
		MPITEST_message(MPITEST_NONFATAL, info_buf);
		MPI_Error_string(ierr, error_string, &size);
		MPITEST_message(MPITEST_NONFATAL, error_string);
		loop_fail++;
	    }


	    MPI_Barrier(MPI_COMM_WORLD);



	    /*------------------------  Waitall  ----------------------------*/

	    ierr = MPI_Waitall(4, recv_request, recv_stat);

	    if (ierr != MPI_SUCCESS)
	    {
		fail++;
		sprintf(info_buf, "Non-zero return code (%d) from MPI_Waitall on receive", ierr);
		MPITEST_message(MPITEST_NONFATAL, info_buf);

		MPI_Error_string(ierr, error_string, &size);
		MPITEST_message(MPITEST_NONFATAL, error_string);

	    }
	    /*---------- Call MPI_Request_free ---------*/
	    ierr = MPI_Request_free(&recv_request[4]);
	    if (ierr != MPI_SUCCESS)
	    {
	        sprintf(info_buf, "Non-zero return code (%d) from MPI_Request_free for receive %d ", ierr, i);
		MPITEST_message(MPITEST_NONFATAL, info_buf);
		MPI_Error_string(ierr, error_string, &size);
		MPITEST_message(MPITEST_NONFATAL, error_string);
		fail++;
	    }


	}  /*  End of node 1 receive code */
    } /* End of test for node < 2 */
    else
    {
	/* Ranks >= 2 need to match BARRIER above */
	MPI_Barrier(MPI_COMM_WORLD);

    }


    /* Detach the buffer, and free the dynamic memory */
    if (MPITEST_me == 0) {
	berr = MPI_Buffer_detach(&buff, &bsize);
	free(buff);
    }


    /* report overall results  */

    MPITEST_report(loop_cnt - fail, fail, 0, testname);

    MPI_Finalize();

    return fail;

}/* main() */
