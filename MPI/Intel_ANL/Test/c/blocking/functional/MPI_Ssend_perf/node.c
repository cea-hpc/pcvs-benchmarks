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

 Latency tester

 For each trial, sends a message of the specified length around a ring.
 This technique ensures that the receive is posted before the message arrives
 so that the results can be reliably interpreted.

 The time reported is from entry into the send call on the sending node to
 exit of the receive call on the receiving node. There is no compensation
 for time spent in the timing call.
******************************************************************************/

#include <stdio.h>
#include <math.h>
#include "mpitest_cfg.h"
#include "mpitest.h"
#ifdef HAVE_UNISTD_H
#include <unistd.h>
#endif

int	odd_flag;
int	bw_flag;
int	test_msg_count;
int	test_repeat;
int	test_increment;
int	test_steps;
char	info_buf[256];

unsigned long maxlength;

double testtime;

int ntest;
double *dlength;
double *dtime;

#define ALIGN	8192
char *malloc_buffer, *buffer, *bufp;

double runtest( int, int, MPI_Comm );
void syncup( MPI_Comm );
void makebuf(void);
void report(int, double);

/*
 * Procedure:
 *	runtest(length)
 *
 * Arguments:
 *	length		Test length
 *
 * Purpose:
 *	Run one trial.
 */

double runtest(int length, int count, MPI_Comm comm)
{
	int	ptype;
	int	next;
	int	rank;
	int	comm_size;
	unsigned msg_count;
	double starttime = 0;
	register unsigned i;
	MPI_Status status;

	/*
	 * Obtain node and ptype
	 */

	MPI_Comm_rank(comm, &rank);
	MPI_Comm_size(comm, &comm_size);

	/*
	 * Calculate neighbors. This works for hypercube or 2x2 mesh.
	 */

	next = rank + 1;
	if (next == comm_size)
		next = 0;
	/*
	 * Make buffer pointer
	 */

	if (odd_flag)
		bufp = &buffer[odd_flag];
	else
		bufp = buffer;

	/*
	 * Each node will do 1/Comm_size of the messages
	 */

	msg_count = count/comm_size;

	MPI_Errhandler_set(comm, MPI_ERRORS_ARE_FATAL);

	/*
	 * The critical timing loop
	 * Don't change this code
	 */

	if (rank == 0) {
		MPI_Barrier(comm);
		sleep(2);
		starttime = MPI_Wtime();
		MPI_Ssend(bufp, length, MPI_BYTE, next, 0, comm);
		msg_count--;
	} else {
		MPI_Barrier(comm);
		MPI_Recv(bufp, length, MPI_BYTE, MPI_ANY_SOURCE, 0, comm, &status);
		MPI_Ssend(bufp, length, MPI_BYTE, next, 0, comm);
		msg_count--;
	}
	for (i = 0; i < msg_count; i++) {
		MPI_Recv(bufp, length, MPI_BYTE, MPI_ANY_SOURCE, 0, comm, &status);
		MPI_Ssend(bufp, length, MPI_BYTE, next, 0, comm);
	}
	if (rank == 0) {
		MPI_Recv(bufp, length, MPI_BYTE, MPI_ANY_SOURCE, 0, comm, &status);
		testtime = MPI_Wtime() - starttime;
	}

	/*
	 * Return measured time
	 */

	MPI_Errhandler_set(comm, MPI_ERRORS_RETURN);
	return testtime;
}

/*
 * Procedure:
 *	syncup()
 *
 * Purpose:
 *	Synchronize nodes before starting the test
 */

void syncup(MPI_Comm comm)
{
	double dummy;
	int    rank;
	char	info_buf[256];
	int	comm_size;

	MPI_Comm_rank(comm, &rank);
	MPI_Comm_size(comm, &comm_size);

	/*
	 * Run the test once around the ring.
	 */

	dummy = runtest(0, comm_size, comm);

	/*
	 * Report I'm ready
	 */

	sprintf(info_buf, "Rank %d ready", MPITEST_current_rank);
	MPITEST_message(MPITEST_INFO1, info_buf);
}

/*
 * Procedure:
 *	makebuf()
 *
 * Purpose:
 *	Create message buffer.
 *	Use page alignment to make it easier to interpret results, since
 *	every run will cross page boundaries at predictable places.
 */

void makebuf(void)
{
	char	info_buf[256];
	long	ii, *lp;	

	malloc_buffer = malloc(maxlength + ALIGN + 1);
	buffer = malloc_buffer;
	
	if (buffer == 0) {
		sprintf(info_buf, "not enough memory for buffer %ld",
			maxlength + ALIGN + 1);
		MPITEST_message(MPITEST_FATAL, info_buf);
		MPI_Abort(MPI_COMM_WORLD, 1);
		exit(1);
	}
	buffer = (char *)((((unsigned long)buffer) + ALIGN-1) & ~(ALIGN-1));

	/* touch the buffer */

	lp = (long *) buffer;
	for (ii=0; ii < maxlength; ii+=sizeof(long)) {
		*lp++ = ii;
	}

	sprintf(info_buf, "Buffer address " MPITEST_AINT_FMT_HEX_SPEC "  length %ld",
		(MPI_Aint)buffer, maxlength);
	MPITEST_message(MPITEST_INFO1, info_buf);
}

/*
 * Procedure:
 *	report(length, testtime)
 *
 * Arguments:
 *	length		Test length
 *	testtime	Measured time
 *
 * Purpose:
 *	Report results from one trial.
 */

void report(int length, double testtime)
{
	double	dlen = length;
	double	bw;
	char	lat_buf[100];
	char	info_buf[256];

	sprintf(lat_buf, "%s latency for %8d byte messages: %6.1f us",
		odd_flag? "odd" : "",
		length, testtime);
	if (length > 0) {
		bw = dlen / testtime;
		sprintf(info_buf, "%s, BW: %3.5f MB/s", lat_buf, bw);
		MPITEST_message(MPITEST_INFO0, info_buf);
	} else {
		MPITEST_message(MPITEST_INFO0, lat_buf);
	}
}


/*
 * Procedure:
 *	main(argc, argv)
 *
 * Arguments:
 *	standard
 *
 * Purpose:
 *	Main Program
 */

int main(int argc, char *argv[])
{
  int
    length,            /*  The length of the current buffer                  */
    byte_length,       /*  length of the current buffer in bytes             */
    test_nump,         /*  The number of processors in current communicator  */
    comm_index,        /*  the array index of the current comm               */
    comm_type,         /*  the index of the current communicator type        */
    type_count,        /*  loop counter for data type loop                   */
    length_count,      /*  loop counter for message length loop              */
    comm_count,        /*  loop counter for communicator loop                */
    ierr,              /*  return value from MPI calls                       */
    max_byte_length,   /*  max buffer length in bytes                        */
    i, j;              /* index variables                                    */
    double	dlen;
    int	ri;
    int	si;
    int	tstart;
    int	lstart;


  char
    info_buf[256],     /*  buffer for passing mesages to MPITEST             */
    testname[128];     /*  the name of the current test                      */

  MPI_Comm comm;       /*  MPI communicator                                  */

  int inter_flag;

  /*
   * Handle options
   */
  bw_flag = 1;
  test_steps = 1;
  test_repeat = 1;
  test_increment = 0;
  odd_flag = 0;

  ierr = MPI_Init(&argc, &argv);
  if (ierr!=MPI_SUCCESS) {
      sprintf(info_buf, "Non-zero return code (%d) from MPI_Init()", ierr);
      MPITEST_message(MPITEST_FATAL, info_buf);
  }

  sprintf(testname, "MPI_Ssend_perf");

  MPITEST_init(argc, argv);
  if (MPITEST_me==0) {
      sprintf(info_buf, "Starting %s test", testname);
      MPITEST_message(MPITEST_INFO0, info_buf);
  }

  if (MPITEST_nump < 2)
      MPITEST_message(MPITEST_FATAL, "This test requires at least 2 ranks");

  for (comm_count=0; comm_count<MPITEST_num_comm_sizes();comm_count++) {
      MPI_Barrier(MPI_COMM_WORLD);
      comm_index = MPITEST_get_comm_index(comm_count);
      comm_type = MPITEST_get_comm_type(comm_count);

      test_nump = MPITEST_get_communicator(comm_type, comm_index, &comm);

      if (MPITEST_current_rank == MPI_UNDEFINED) {
	MPITEST_free_communicator(comm_type, &comm);
	continue;
      }

      MPI_Comm_test_inter(comm, &inter_flag);
      if (inter_flag) /* Ignore inter-communicator for tests */ {
	MPITEST_free_communicator(comm_type, &comm);
	continue;
      }

      if (test_nump == 1) {
	if (MPITEST_me == 0) {
	  sprintf(info_buf, "Skipping comm size of 1");
	  MPITEST_message(MPITEST_INFO0, info_buf);
	}
	MPITEST_free_communicator(comm_type, &comm);
	continue;
      }

      /*
       * Round up count to a multiple of comm_size
       */

      test_msg_count = 1000;
      if (test_msg_count % test_nump != 0) {
	test_msg_count = test_msg_count
		+ test_nump - (test_msg_count % test_nump);
      }

      if (MPITEST_current_rank == 0) {
	  sprintf(info_buf, "Start commsize %d (%d messages)", test_nump, test_msg_count);
	  MPITEST_message(MPITEST_INFO0, info_buf);
      }


      for (length_count=0;length_count<MPITEST_num_message_lengths();length_count++) {
	    byte_length = MPITEST_get_message_length(length_count);


	    /*
	     * Allocate data arrays
	     */

	    if (ntest == 0)
		 ntest = 1;
	    dlength = (double *)malloc(ntest*sizeof(double));
	    dtime = (double *)malloc(ntest*sizeof(double));
	    if (dlength == NULL || dtime == NULL) {
		MPITEST_message(MPITEST_FATAL, "Too many tests (malloc failed)");
	    }

	    /*
	     * Initialize
	     */

	    maxlength = byte_length;
	    maxlength += test_steps * test_increment;

	    makebuf();
	    syncup(comm);

	    /*
	     * Run all tests
	     */

	    ntest = 0;


	    /*
	     * Set up for reps and steps from one length argument
	     */

	    tstart = ntest;
	    lstart = byte_length;

	    /*
	     * Run a set of reps
	     */

	    for (ri = test_repeat; ri; ri--) {

		/*
		 * For each rep run a set of steps
		 * This way we don't do all the reps for one length
		 * right next to each other.
		 */

		for (si = test_steps, byte_length = lstart;
			     si;
			     si--, byte_length += test_increment) {

		    /*
		     * Run one trial
		     */

		    testtime = runtest(byte_length, test_msg_count, comm);

		    /*
		     * Handle results from this test
		     */

		    if (MPITEST_current_rank == 0) {
			sprintf(info_buf, "End test");
			MPITEST_message(MPITEST_INFO1, info_buf);
			testtime = testtime * 1000000.0 /
			   (double)(test_msg_count);
			dlen = (double)byte_length;

			report(byte_length, testtime);

			dtime[ntest] = testtime;
			dlength[ntest] = dlen;
			ntest++;
		    }
		}
	    }

	/*
	 * Get the next length argument
	 */

	free(malloc_buffer);
	free(dlength);
	free(dtime);

	}  /****** for (length_count=0;...) *********/

      MPITEST_free_communicator(comm_type, &comm);
    } /****** for (comm_count=0;...) ***********/
  /* report overall results  */
  MPITEST_report(1, 0, 1, testname);

  MPI_Finalize();
  return 0;
} /* main() */



