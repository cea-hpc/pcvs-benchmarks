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
*           Functional test for MPI_Test with Non-blocking I/O
*
*  Test references:
*    MPI Standard:  Section 3.7.5  Multiple Completions
*                   Section 3.7.4  Semantics of Nonblocking Communications
*                   Section 3.7.3  For MPI_Request_free
*  MPI_Request_free states that an ongoing communication associated with
*      the request will be allowed to complete, after which the request
*      will be deallocated.  After deallocation, the request becomes
*      equal to MPI_REQUEST_NULL
*
*  Section 3.7.5  of the MPI Standard notes that if one or more of the
*      communications completed by a call to MPI_Testall fail, the function
*      will return ther error code:  MPI_ERR_IN_STATUS, and will set the
*      error field of each status to a spceific error code, which will
*      be MPI_SUCCESS if that specific communication was a success
*
*  This test sends messages from node 0 to node 1, and uses MPI_Test
*  to check for their proper reception.  After the send the program calls
*  MPI_Request_free for two of the messages to ensure they are sent before
*  the Request Objects are freed.  This test Does a Test on messages
*  that have already been Tested on.
******************************************************************************/

#define  PROG_NAME   "MPI_Test"
#define  PROG_DESC   "MPI_Test on two nodes"

#define  NUMMESG     20	/* Number of messages to send                 */
#define  NUMELM      10	/* # of elements to send and receive                 */

#include "mpitest_cfg.h"
#include "mpitest.h"

int main(int argc, char *argv[])

{
    int
        flag,   	/* flag return from  MPI_Test calls                  */
        i,       	/* general index variable                            */
        cnt_len,        /* received length returned by MPI_Get_Count         */
        dest,    	/* Destination of Isend message                      */
        fail,   	/* counts total number # of failures                 */
        error,  	/* errors from one MPI call                          */
        ierr,   	/* return value from MPI calls                       */
        errorclass,	/* error class of ierr                               */
        size,   	/* length of error message                           */
        send_loop_cnt,	/* count tries on send Test loop                     */
        recv_loop_cnt,	/* count tries on receive Test loop                  */
        total_cnt;	/* total test count                                  */

    char
        testname[128],	/* the name of this test                             */
        info_buf[256];	/* for sprintf                                       */
    char error_string[MPI_MAX_ERROR_STRING];

    struct dataTemplate
        value;  	/* dataTemplate for initializing buffers             */

    MPI_Request
	send_request[4 * NUMMESG],
	recv_request[4 * NUMMESG];	/* MPI request structure             */

    MPI_Status
	send_stat,  	/* Send Source/Tag information                       */
	recv_stat;	/* Recv Source/Tag information                       */

    int
        recv_buffer[4 * NUMMESG][NUMELM],	/* input to Isend            */
        send_buffer[4 * NUMMESG][NUMELM];	/* input to Isend            */

    char
        bsend_buff[NUMMESG * (8 * NUMELM + MPI_BSEND_OVERHEAD + 100)];
    char *bb;

	MPITEST_message(MPITEST_NONFATAL, "ERROR\n");
