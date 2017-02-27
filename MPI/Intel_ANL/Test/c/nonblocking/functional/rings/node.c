/****************************************************************************
 *
 *  NAME: rings/node.c
 *
 *
 *  USAGE: node.nx [-b] [-d] [-e] [-h] [-l max_loops] [-m max_len ]
 *                 [-n num_rings] [-r] [-s seed] [-t minutes] [-v]
 *
 *          -b   Force System Buffer Usage (off)
 *          -d   Print Defaults (off)
 *          -e   Enable error checking of messages (off)
 *          -h   Print Usage Message (off)
 *          -l   Number of shifts for each ring (num_nodes)
 *          -m   Message length (500000/(num_nodes + 5))
 *          -n   Number of rings (5*num_nodes)
 *          -r   Enable random message lengths up to max_len long (off)
 *          -s   Specify the seed for the random number generator (1000*dclock)
 *          -t   Specify the number of minutes the test should run,
 *               overrides -l value (0)
 *
 *          Default values are in parenthesis.
 *
 *
 *  DESCRIPTION:
 *     This test shifts messages around rings of nodes. Each ring has
 *     numnodes() number of nodes. For each ring, each node is assigned a node
 *     to send messages to and a node to receive messages from. A ring
 *     is shifted when each node in the ring sends a message to its
 *     assigned "send to" node and receives a message from its assigned
 *     "receive from" node.
 *
 *     The "send to" and "receive from" information is stored in a two
 *     dimensional array of structures. Each arraly element holds the send to
 *     and receive from node number for each node of each ring. Accesse is via
 *     the ring number and node number, ie: rings[2][my_node].to_node is
 *     the node that my_node should send to for ring 2. The to_node and
 *     from_node are assigned randomly.
 *
 *     See the usage description for configuration options. System message
 *     passing buffering is forced with the -b option by sending all
 *     messages before receiving. Otherwise receives are posted before
 *     sending messages. To avoid blocking sends, the number of buffered
 *     messages is limited to the number of nodes available. Error checking,
 *     when enabled with -e, only verifies the origin of received messages.
 *     The number of shifts for each ring can be modified with the -l option.
 *     All rings are shifted at the same time. If random message lengths are
 *     enabled with -r, message length is randomly set between 0 and max_len
 *     bytes. Otherwise all messages are max_len bytes.
 *
 *
 *  HISTORY:
 *     1    11/22/93   egb   Created from mesh and size tests
 *
 *
 ***************************************************************************/

#include<stdio.h>
#include<stdlib.h>
#include<setjmp.h>
#include<signal.h>
#include<time.h>
#if defined(HAVE_UNISTD_H)
#  include <unistd.h>
#endif

#include "mpitest_cfg.h"
#include "mpitest.h"

#define SEED_TYPE    100
#define RING_TYPE    200
#define MSG_TYPE   10000

MPI_Status mpi_status;
MPI_Request mpi_request;
int mpi_count, mpi_numnodes, mpi_rank, mpi_flag;

int num_rings, max_loops;
int cnt, error_chk, failed, minutes;
int useRandom, use_buffers;

int buf_size, max_len;
int ierr, size;

unsigned int seed;

double lastprint;

char *rcv_buf, *snd_buf, timebuf[20];

/*
 * rings[][] array: dynamically allocated, first index refers to the ring
 * number, second index is a node number, example: rings[2][4].from_node =
 * for ring 2, node 4 will receive a message from this value
 */
struct ring_type {
   long from_node;
   long to_node;
} **rings;

/* Add this header to messages for checking */
struct header {
   long from_node;
   long from_msg_type;
   long data_length;		/* does not include header bytes */
};

/* Globals for getopt() */
int opterr;
char *optarg;

/* Environment variables for set/longjmp */
int sig_caught;
jmp_buf to_vec;

/* Function Prototypes */
void check_buf(char *, int, int);
void fill_buf(char *, char *, long);
void get_cmd_opt(int, char **);
void init_rings(void);
void snd_rcv_buf(void);
void snd_rcv_nbuf(void);
void timeout(int);
char *tod(void);

char info_buf[512];	/* buffer for passing messages to
					 * MPITEST */
char info_buf1[512];	/* buffer for passing messages to
					 * MPITEST */

/****************************************************************************
 *
 *  main()
 *
 ***************************************************************************/
int main(int argc, char *argv[])
{
   int i, j;
   char testname[128];		/* the name of the Test */
   char error_string[MPI_MAX_ERROR_STRING];

   ierr = MPI_Init(&argc, &argv);
   if (ierr != MPI_SUCCESS) {
      sprintf(info_buf, "MPI_Init() returns (%d)\n", ierr);
      MPITEST_message(MPITEST_FATAL, info_buf);
   }

   sprintf(testname, "rings");

   MPITEST_init(argc, argv);

   if (MPITEST_me == 0) {
      sprintf(info_buf, "Starting %s test", testname);
      MPITEST_message(MPITEST_INFO0, info_buf);
   }

   /* Initialize variables to default values */

   num_rings = 5 * MPITEST_nump;

   max_len = 50000 / (MPITEST_nump + 5);

   max_loops = 10 * num_rings;	/* Each ring shifted 10 times */

   error_chk = 0;
   failed = 0;
   minutes = 0;
   useRandom = 0;
   seed = (unsigned int) (1000 * MPI_Wtime());
   use_buffers = 0;

   /* Parse command line options */
   get_cmd_opt(argc, argv);

   /* Print parameters */
   if (MPITEST_me == 0) {
      sprintf(info_buf,
	      "(%d): %s Start RINGS tests\n"
	      "\terror_chk   = %d\n"
	      "\tmax_len     = %d\n"
	      "\tmax_loops   = %d\n"
	      "\tminutes     = %d\n"
	      "\tnum_nodes   = %d\n"
	      "\tnum_rings   = %d\n"
	      "\trandom      = %d\n"
	      "\tseed        = %u\n"
	      "\tuse_buffers = %d\n",
	      MPITEST_me, tod(), error_chk, max_len, max_loops,
	      minutes, MPITEST_nump, num_rings, useRandom, seed, use_buffers);
      MPITEST_message(MPITEST_INFO0, info_buf);
   }

   /* Allocate user buffer and  rings array */
   buf_size = max_len + sizeof(struct header);

   rcv_buf = (char *) malloc(buf_size);
   if (rcv_buf == NULL) {
      sprintf(info_buf, "allocation of rcv_buf[] *** FAILED ***");
      MPITEST_message(MPITEST_FATAL, info_buf);
   }

   snd_buf = (char *) malloc(buf_size);
   if (snd_buf == NULL) {
      MPITEST_message(MPITEST_FATAL, "allocation of snd_buf[] *** FAILED ***");
   }

   rings = (struct ring_type **) calloc(num_rings, sizeof(struct ring_type *));
   if (rings == NULL) {
      MPITEST_message(MPITEST_FATAL, "allocation of rings[][] *** FAILED ***");
   }

   for (i = 0; i < num_rings; i++) {
      rings[i] = (struct ring_type *) calloc(MPITEST_nump,
					     sizeof(struct ring_type));
      if (rings[i] == NULL) {
	 MPITEST_message(MPITEST_FATAL, "allocaiont of rings[] *** FAILED ***");
      }
   }

   /* Initialize rings */
   if (MPITEST_me == 0) {
      init_rings();
   }

   /* Broadcast rings to all nodes */
   for (i = 0; i < num_rings; i++) {
      ierr = MPI_Bcast(rings[i], 2 * MPITEST_nump, MPI_LONG, 0, MPI_COMM_WORLD);
      if (ierr != MPI_SUCCESS) {
	 sprintf(info_buf, "MPI_Bcast() returned %d", ierr);
	 MPITEST_message(MPITEST_NONFATAL, info_buf);
	 MPI_Error_string(ierr, error_string, &size);
	 MPITEST_message(MPITEST_FATAL, error_string);
      }

   }

   /* Broadcast seed value and seed random number generator */
   ierr = MPI_Bcast(&seed, 1, MPI_UNSIGNED, 0, MPI_COMM_WORLD);
   if (ierr != MPI_SUCCESS) {
      sprintf(info_buf, "MPI_Bcast() returned %d", ierr);
      MPITEST_message(MPITEST_NONFATAL, info_buf);
      MPI_Error_string(ierr, error_string, &size);
      MPITEST_message(MPITEST_FATAL, error_string);
   }
   srand(seed * (MPITEST_me + 1));


   /* Sync up and prepare for test loop */
   ierr = MPI_Barrier(MPI_COMM_WORLD);
   if (ierr != MPI_SUCCESS) {
      sprintf(info_buf, "MPI_Barrier() returned %d", ierr);
      MPITEST_message(MPITEST_NONFATAL, info_buf);
      MPI_Error_string(ierr, error_string, &size);
      MPITEST_message(MPITEST_FATAL, error_string);
   }

   lastprint = MPI_Wtime();

   /* Begin test */
   if (setjmp(to_vec) == 0) {
      signal(SIGALRM, timeout);
      signal(SIGINT, timeout);
      alarm(minutes * 60);

      cnt = 0;
      while (cnt != max_loops) {
	 cnt++;

	 if (use_buffers) {
	    snd_rcv_buf();
	 }
	 else {
	    snd_rcv_nbuf();
	 }

	 if (MPITEST_me == 0 && MPI_Wtime() - lastprint > 60.0) {
	    sprintf(info_buf, "(%d): %s All rings shifted %d times\n",
		    MPITEST_me, tod(), cnt);
	    MPITEST_message(MPITEST_INFO0, info_buf);

	    lastprint = MPI_Wtime();
	 }
      }				/* while */
   }
   else {
      /* Time out, setjmp vectors here after SIGALRM or SIGINT */
      if (MPITEST_me == 0) {
	 if (sig_caught == SIGINT) {
	    sprintf(info_buf, "(%d): RINGS: Caught ^C\n", MPITEST_me);
	    MPITEST_message(MPITEST_NONFATAL, info_buf);
	 }
	 else {
	    sprintf(info_buf, "(%d): RINGS: %d min Time Limit Reached!, %d shifts\n",
		    MPITEST_me, minutes, cnt);
	    MPITEST_message(MPITEST_NONFATAL, info_buf);
	 }
      }
   }

   /* End test */

/*   if (failed) {
      sprintf(info_buf, "(%d): RINGS: *** FAILED ***\n", MPITEST_me);
      MPITEST_message(MPITEST_INFO0, info_buf);
   }
   else {
      sprintf(info_buf, "(%d): RINGS: PASSED\n", MPITEST_me);
      MPITEST_message(MPITEST_INFO0, info_buf);
   }
*/
   MPITEST_report(max_loops - failed, failed, 0, "RINGS");

   if (MPITEST_me == 0) {
      sprintf(info_buf, "(%d): %s End RINGS Test\n", MPITEST_me, tod());
      MPITEST_message(MPITEST_INFO0, info_buf);
   }

   MPI_Finalize();
   
   return 0;
}				/* main */


/*****************************************************************************
 *
 *   check_buf(): Check the contents of a message againts the header
 *                information. Return non-zero if there is a mismatch.
 *
 ****************************************************************************/
void 
check_buf(char *buf, int ring, int msg)
{
   long error_cnt, header_len, header_node, header_type;
   long info_len, info_node, info_type;

   char *buf_ptr, fill_char, *end, *start;
   char error_string[MPI_MAX_ERROR_STRING];

   header_len = ((struct header *) buf)->data_length;
   header_node = ((struct header *) buf)->from_node;
   header_type = ((struct header *) buf)->from_msg_type;

   ierr = MPI_Get_count(&mpi_status, MPI_BYTE, &mpi_count);
   if (ierr != MPI_SUCCESS) {
      sprintf(info_buf, "MPI_Get_count() returned %d", ierr);
      MPITEST_message(MPITEST_NONFATAL, info_buf);
      MPI_Error_string(ierr, error_string, &size);
      MPITEST_message(MPITEST_FATAL, error_string);
   }

   info_len = mpi_count;
   info_node = mpi_status.MPI_SOURCE;
   info_type = mpi_status.MPI_TAG;

   if (info_node != rings[ring][MPITEST_me].from_node) {
      failed++;
      sprintf(info_buf, "(%d): Ring %d Msg %d: *** FAILED *** Not from expected node, %ld != %ld\n",
	      MPITEST_me, ring, msg, rings[ring][MPITEST_me].from_node, info_node);
      MPITEST_message(MPITEST_INFO0, info_buf);
   }

   if (info_len != header_len + sizeof(struct header)) {
      failed++;
      sprintf(info_buf, "(%d): Ring %d Msg %d: *** FAILED *** info_len != header_len, %ld != %ld\n",
	      MPITEST_me, ring, msg, info_len, header_len + sizeof(struct header));
      MPITEST_message(MPITEST_NONFATAL, info_buf);
   }

   if (info_node != header_node) {
      failed++;
      sprintf(info_buf, "(%d): Ring %d Msg %d: *** FAILED *** info_node != header_node, %ld != %ld\n",
	      MPITEST_me, ring, msg,
	      info_len, header_len + sizeof(struct header));
      MPITEST_message(MPITEST_NONFATAL, info_buf);
   }

   if (info_node != header_node) {
      failed++;
      sprintf(info_buf, "(%d): Ring %d Msg %d: *** FAILED *** info_node != header_node, %ld != %ld\n",
	      MPITEST_me, ring, msg, info_node, header_node);
      MPITEST_message(MPITEST_NONFATAL, info_buf);
   }

   if (info_type != header_type) {
      failed++;
      sprintf(info_buf, "(%d): Ring %d Msg %d: *** FAILED *** info_type != header_type, %ld != %ld\n",
	      MPITEST_me, ring, msg, info_type, header_type);
      MPITEST_message(MPITEST_NONFATAL, info_buf);
   }

   /* Check for proper number of fill_chars */
   start = buf + sizeof(struct header);
   end = start + header_len;
   fill_char = header_len % 95 + 32;
   error_cnt = failed;

   for (buf_ptr = start; buf_ptr < end; buf_ptr++) {
      if (*buf_ptr != fill_char) {
	 failed++;
      }
   }

   if (failed > error_cnt) {
      sprintf(info_buf, "(%d): Ring %d Msg %d: *** FAILED *** %ld bad fill_char(s) in buf\n",
	      MPITEST_me, ring, msg, failed - error_cnt);
      MPITEST_message(MPITEST_NONFATAL, info_buf);
   }

   /* Check unused part of buffer for zeros */
   start = buf + sizeof(struct header) + header_len;
   end = buf + buf_size;
   error_cnt = failed;

   for (buf_ptr = start; buf_ptr < end; buf_ptr++) {
      if (*buf_ptr != 0) {
	 failed++;
      }
   }

   if (failed > error_cnt) {
      sprintf(info_buf, "(%d): Ring %d Msg %d: *** FAILED *** %ld missing zero(s) in buf\n",
	      MPITEST_me, ring, msg, failed - error_cnt);
      MPITEST_message(MPITEST_NONFATAL, info_buf);
   }
}				/* check_buf */


/*****************************************************************************
 *
 *   fill_buf(): Two arrays, rcv_buf[] and snd_buf[] are filled with the
 *               appropritate values. First, both buffers are filled with
 *               zeros. Then the first msg_len bytes of snd_buf[] are filled
 *               with (msg_len%95 + 32). Note that the bytes for the header
 *               are is skipped. These are filled in before each send.
 *
 ****************************************************************************/
void 
fill_buf(char *rcv_buf, char *snd_buf, long msg_len)
{
   char fill_char;

   fill_char = msg_len % 95 + 32;

   memset(rcv_buf, 0, buf_size);
   memset(snd_buf, 0, buf_size);
   memset(snd_buf + sizeof(struct header), fill_char, msg_len);
}				/* fill_buf */


/*****************************************************************************
 *
 *   get_cmd_opt(): Parse command line options, setting global
 *                  variables accordingly
 *
 *****************************************************************************/
void 
get_cmd_opt(int argc, char *argv[])
{
   int i = 0;

   while (i < argc) {
      if (!strcmp(argv[i], "-b")) {
	 use_buffers = 1;
	 i++;
      }
      else if (!strcmp(argv[i], "-d")) {
	 if (MPITEST_me == 0) {
	    sprintf(info_buf, "Defaults for this partition:\n");
	    MPITEST_message(MPITEST_INFO0, info_buf);

	    sprintf(info_buf, "\terror_chk   = %d\n", error_chk);
	    MPITEST_message(MPITEST_INFO0, info_buf);

	    sprintf(info_buf, "\tmax_len     = %d\n", max_len);
	    MPITEST_message(MPITEST_INFO0, info_buf);

	    sprintf(info_buf, "\tmax_loops   = %d\n", max_loops);
	    MPITEST_message(MPITEST_INFO0, info_buf);

	    sprintf(info_buf, "\tminutes     = %d\n", minutes);
	    MPITEST_message(MPITEST_INFO0, info_buf);

	    sprintf(info_buf, "\tnum_nodes   = %d\n", MPITEST_nump);
	    MPITEST_message(MPITEST_INFO0, info_buf);

	    sprintf(info_buf, "\tnum_rings   = %d\n", num_rings);
	    MPITEST_message(MPITEST_INFO0, info_buf);

	    sprintf(info_buf, "\trandom      = %d\n", useRandom);
	    MPITEST_message(MPITEST_INFO0, info_buf);

	    sprintf(info_buf, "\tseed        = %u\n", seed);
	    MPITEST_message(MPITEST_INFO0, info_buf);

	    sprintf(info_buf, "\tuse_buffers = %d\n", use_buffers);
	    MPITEST_message(MPITEST_INFO0, info_buf);
	 }
	 exit(1);
      }
      else if (!strcmp(argv[i], "-e")) {
	 error_chk = 1;
	 i++;
      }
      else if  (!strcmp(argv[i], "-l")) {
	 i++;
	 if (i >= argc) {
	    sprintf(info_buf, "Option -l requires an argument");
	    MPITEST_message(MPITEST_FATAL, info_buf);
	 }
	 else {
	    max_loops = atoi(argv[i]);
	    minutes = 0;
	    i++;
	 }
      }
      else if (!strcmp(argv[i], "-m")) {
	 i++;
	 if (i >= argc) {
	    sprintf(info_buf, "Option -m requires an argument");
	    MPITEST_message(MPITEST_FATAL, info_buf);
	 }
	 else {
	    max_len = atol(argv[i]);
	    i++;
	 }
      }
      else if (!strcmp(argv[i], "-n")) {
	 i++;
	 if (i >= argc) {
	    sprintf(info_buf, "Option -n requires an argument");
	    MPITEST_message(MPITEST_FATAL, info_buf);
	 }
	 else {
	    num_rings = atoi(argv[i]);
	    i++;
	 }
      }
      else if (!strcmp(argv[i], "-r")) {
	 i++;
	 if (i >= argc) {
	    sprintf(info_buf, "Option -r requires an argument");
	    MPITEST_message(MPITEST_FATAL, info_buf);
	 }
	 else {
	    useRandom = 1;
	    i++;
	 }
      }
      else if (!strcmp(argv[i], "-s")) {
	 i++;
	 if (i >= argc) {
	    sprintf(info_buf, "Option -s requires an argument");
	    MPITEST_message(MPITEST_FATAL, info_buf);
	 }
	 else {
	    seed = atol(argv[i]);
	    i++;
	 }
      }
      else if (!strcmp(argv[i], "-t")) {
	 i++;
	 if (i >= argc) {
	    sprintf(info_buf, "Option -t requires an argument");
	    MPITEST_message(MPITEST_FATAL, info_buf);
	 }
	 else {
	    minutes = atoi(argv[i]);
	    max_loops = -1;
	    i++;
	 }
      }
      else if ((!strcmp(argv[i], "-h")) || (!strcmp(argv[i], "-?"))) {
	 if (MPITEST_me == 0) {
	    sprintf(info_buf, "Usage: %s\t[-b] [-d] [-e] [-h] [-l max_loops] [-m max_len ] [-n num_rings]\n\t[-r] [-s seed] [-t minutes] [-v] [-V]\n\n", argv[0]);
	    strcat(info_buf, "-b   Force System Buffer Usage\n");
	    strcat(info_buf, "-d   Print Defaults\n");
	    strcat(info_buf, "-e   Enable error checking of messages\n");
	    strcat(info_buf, "-h   Print Usage Message\n");
	    strcat(info_buf, "-l   Number of shifts around the rings\n");
	    strcat(info_buf, "-m   Message length\n");
	    strcat(info_buf, "-n   Number of rings\n");
	    strcat(info_buf, "-r   Enable random message lengths up to max_len long\n");
	    strcat(info_buf, "-s   Specify the seed for the random number generator\n");
	    strcat(info_buf, "-t   Specify the number of minutes the test should run, overides -l value\n");
	    MPITEST_message(MPITEST_INFO0, info_buf);
	 }
	 exit(1);
      }
      else {
	 /* Ignore any unrecognised option */
	 i++;
      }
   }
}



/***************************************************************************
 *
 *   init_rings(): Each ring contains numnode() nodes arranged in a
 *                 random order. First a random list of nodes is generated.
 *                 This list is used to determine a node's send to and
 *                 receive from nodes. Example: 4 nodes, randomly arranged
 *                 2 1 3 0, node 3 sends to node 0 and receives from node 1.
 *                 This is repeated for each ring and all to/from info is
 *                 stored in rings[][] by ring and node number.
 *                 The rings array must be common to all nodes, therefore
 *                 node 0 initializes rings[][] using this function and then
 *                 broadcasts rings[][] to all nodes.
 *
 ***************************************************************************/
void 
init_rings()
{
   int i, j, k;
   int dup_found, *r_ring;

   long r_node;

   r_ring = (int *) calloc(MPITEST_nump, sizeof(MPITEST_nump));
   if (r_ring == NULL) {
      sprintf(info_buf, "allocation of r_ring[] *** FAILED ***");
      MPITEST_message(MPITEST_FATAL, info_buf);
   }

   /* Create rings of random nodes to send to */
   for (i = 0; i < num_rings; i++) {

      /* Generate random ring */
      for (j = 0; j < MPITEST_nump; j++) {
	 do {
	    dup_found = 0;
            /* should yield a sufficiently random number in the range [0,MPITEST_nump) */
            r_node = (long) (MPITEST_nump * (rand() / (RAND_MAX + 1.0)));

	    for (k = 0; k < j; k++) {
	       if (r_ring[k] == r_node) {
		  dup_found = 1;
		  break;
	       }
	    }
	 } while (dup_found);

	 r_ring[j] = r_node;
      }

      sprintf(info_buf, "(%d): ring %d: ", MPITEST_me, i);
      MPITEST_message(MPITEST_INFO1, info_buf);

      strcpy(info_buf, "");

      for (j = 0; j < MPITEST_nump; j++) {
	 sprintf(info_buf1, "%d ", r_ring[j]);
	 strcat(info_buf, info_buf1);
      }

      MPITEST_message(MPITEST_INFO1, info_buf);
      

      /* Assign to and from according to random ring */
      rings[i][r_ring[0]].from_node = r_ring[MPITEST_nump - 1];
      rings[i][r_ring[0]].to_node = r_ring[1];

      for (j = 1; j < MPITEST_nump - 1; j++) {
	 rings[i][r_ring[j]].from_node = r_ring[j - 1];
	 rings[i][r_ring[j]].to_node = r_ring[j + 1];
      }

      rings[i][r_ring[MPITEST_nump - 1]].from_node = r_ring[MPITEST_nump - 2];
      rings[i][r_ring[MPITEST_nump - 1]].to_node = r_ring[0];
   }				/* for(i) */

   free(r_ring);
}				/* init_rings */


/*****************************************************************************
 *
 *   snd_rcv_nbuf(): Shift all rings once. Avoid explicit use of system
 *                   buffers by shifting rings one at a time. Each shift has
 *                   a unique message type. For each ring each node posts an
 *                   irecv for this message type, csends to its to_node using
 *                   this message type, and then waits for the irecv to
 *                   complete. Message length maybe fixed or random. If
 *                   enabled message content is checked and compared with
 *                   the values returned from info*()
 *
 *****************************************************************************/
void 
snd_rcv_nbuf()
{
   int ring;

   long header_node, header_len, header_type;
   long mid, from_node, rcv_len, rcv_type, snd_len;
   char error_string[MPI_MAX_ERROR_STRING];

   /* Send and receive messages */
   for (ring = 0; ring < num_rings; ring++) {
      /* Get message length */
      if (useRandom) {
         /* should produce a random number in the range [0,max_len) */
         snd_len = (long) (max_len * (rand() / (RAND_MAX + 1.0)));
      }
      else {
	 snd_len = max_len;
      }

      /* If error checking enabled, initialize buffers */
      if (error_chk) {
	 fill_buf(rcv_buf, snd_buf, snd_len);
	 ((struct header *) snd_buf)->from_node = MPITEST_me;
	 ((struct header *) snd_buf)->from_msg_type = MSG_TYPE + ring;
	 ((struct header *) snd_buf)->data_length = snd_len;
      }

      /* Send/receive messages */

      ierr = MPI_Irecv(rcv_buf, buf_size, MPI_BYTE, MPI_ANY_SOURCE, 10000 + ring,
		       MPI_COMM_WORLD, &mpi_request);
      if (ierr != MPI_SUCCESS) {
	 sprintf(info_buf, "MPI_Irecv() returned %d", ierr);
	 MPITEST_message(MPITEST_NONFATAL, info_buf);
	 MPI_Error_string(ierr, error_string, &size);
	 MPITEST_message(MPITEST_FATAL, error_string);
      }

      ierr = MPI_Send(snd_buf, snd_len + sizeof(struct header), MPI_BYTE,
		      rings[ring][MPITEST_me].to_node, 10000 + ring, MPI_COMM_WORLD);
      if (ierr != MPI_SUCCESS) {
	 sprintf(info_buf, "MPI_Send() returned %d", ierr);
	 MPITEST_message(MPITEST_NONFATAL, info_buf);
	 MPI_Error_string(ierr, error_string, &size);
	 MPITEST_message(MPITEST_FATAL, error_string);
      }

      ierr = MPI_Wait(&mpi_request, &mpi_status);
      if (ierr != MPI_SUCCESS) {
	 sprintf(info_buf, "MPI_Wait() returned %d", ierr);
	 MPITEST_message(MPITEST_NONFATAL, info_buf);
	 MPI_Error_string(ierr, error_string, &size);
	 MPITEST_message(MPITEST_FATAL, error_string);
      }

      ierr = MPI_Get_count(&mpi_status, MPI_BYTE, &mpi_count);
      if (ierr != MPI_SUCCESS) {
	 sprintf(info_buf, "MPI_Get_count() returned %d", ierr);
	 MPITEST_message(MPITEST_NONFATAL, info_buf);
	 MPI_Error_string(ierr, error_string, &size);
	 MPITEST_message(MPITEST_FATAL, error_string);
      }

      sprintf(info_buf, "(%d): Ring %d Msg %d: Sent to: %ld, %ld bytes\n",
	      MPITEST_me, ring, cnt,
	      rings[ring][MPITEST_me].to_node, snd_len);
      MPITEST_message(MPITEST_INFO1, info_buf);

      sprintf(info_buf, "(%d): Ring %d Msg %d: Received from: %d, %d bytes\n",
	     MPITEST_me, ring, cnt, mpi_status.MPI_SOURCE,
	     mpi_count);
      MPITEST_message(MPITEST_INFO1, info_buf);

      /* If enabled, check received message */
      if (error_chk) {
	 check_buf(rcv_buf, ring, cnt);
      }
   }
}				/* snd_rcv_nbuf */


/*****************************************************************************
 *
 *   snd_rcv_buf(): Shift all rings once. Explicilty use system buffers by
 *                  doing all sends before receives. Up to numnodes() number
 *                  of messages maybe buffered to avoid blocking sends.
 *
 *****************************************************************************/
void 
snd_rcv_buf()
{
   int i, j;
   int num_mod, start, end;

   long snd_len, from_node, rcv_len, rcv_type;
   long header_node, header_len, header_type;
   char error_string[MPI_MAX_ERROR_STRING];

   num_mod = num_rings / MPITEST_nump + 1;

   for (i = 0; i < num_mod; i++) {

      /* Limit the number of buffered messages to num_nodes */
      start = MPITEST_nump * i;
      end = start + MPITEST_nump;

      /* Last loop picks up remaining messages */
      if (end > num_rings) {
	 end = num_rings;
      }

      /* Send messages all at once */
      for (j = start; j < end; j++) {
	 /* Get message length */
	 if (useRandom) {
            /* should produce a random number in the range [0,max_len) */
            snd_len = (long) (max_len * (rand() / (RAND_MAX + 1.0)));
	 }
	 else {
	    snd_len = max_len;
	 }

	 /* If error checking enabled, initialize buffers */
	 if (error_chk) {
	    fill_buf(rcv_buf, snd_buf, snd_len);
	    ((struct header *) snd_buf)->from_node = MPITEST_me;
	    ((struct header *) snd_buf)->from_msg_type = MSG_TYPE + j;
	    ((struct header *) snd_buf)->data_length = snd_len;
	 }

	 ierr = MPI_Send(snd_buf, snd_len + sizeof(struct header), MPI_BYTE,
			 rings[j][MPITEST_me].to_node, 10000 + j, MPI_COMM_WORLD);
	 if (ierr != MPI_SUCCESS) {
	    sprintf(info_buf, "MPI_Send() returned %d", ierr);
	    MPITEST_message(MPITEST_NONFATAL, info_buf);
	    MPI_Error_string(ierr, error_string, &size);
	    MPITEST_message(MPITEST_FATAL, error_string);
	 }

	 sprintf(info_buf, "(%d): Ring %d Msg %d: Sent to: %ld, %ld bytes\n",
		 MPITEST_me, j, cnt, rings[j][MPITEST_me].to_node, snd_len);

	 MPITEST_message(MPITEST_INFO1, info_buf);

      }				/* send loop */

      /* Receive messages in reverse */
      for (j = end - 1; j >= start; j--) {
	 /* If error checking enable, clear receive buffer */
	 if (error_chk) {
	    memset(rcv_buf, 0, buf_size);
	 }

	 ierr = MPI_Recv(rcv_buf, buf_size, MPI_BYTE, MPI_ANY_SOURCE,
			 10000 + j, MPI_COMM_WORLD, &mpi_status);
	 if (ierr != MPI_SUCCESS) {
	    sprintf(info_buf, "MPI_Recv() returned %d", ierr);
	    MPITEST_message(MPITEST_NONFATAL, info_buf);
	    MPI_Error_string(ierr, error_string, &size);
	    MPITEST_message(MPITEST_FATAL, error_string);
	 }

	 ierr = MPI_Get_count(&mpi_status, MPI_BYTE, &mpi_count);
	 if (ierr != MPI_SUCCESS) {
	    sprintf(info_buf, "MPI_Get_count() returned %d", ierr);
	    MPITEST_message(MPITEST_NONFATAL, info_buf);
	    MPI_Error_string(ierr, error_string, &size);
	    MPITEST_message(MPITEST_FATAL, error_string);
	 }

	 sprintf(info_buf, "(%d): Ring %d Msg %d: Received from: %d, %d bytes\n",
		 MPITEST_me, j, cnt, mpi_status.MPI_SOURCE,
		 mpi_count);
	 MPITEST_message(MPITEST_INFO1, info_buf);

	 /* If enabled, check received message */
	 if (error_chk) {
	    check_buf(rcv_buf, j, cnt);
	 }
      }				/* receive loop */
   }				/* rings loop */
}				/* snd_rcv_buf */


/*****************************************************************************
 *
 *   tod(): returns a pointer to a formated string containing the current
 *          system date and time
 *
 *****************************************************************************/
char *
tod(void)
{
   time_t tp;
   struct tm *stm;

   tp = time((time_t *) NULL);
   stm = localtime(&tp);
   strftime(timebuf, 20, "%H:%M:%S", stm);
   return timebuf;
}				/* tod */


/*****************************************************************************
 *
 *   timeout(): Interrupt handler for SIGALRM and SIGINT, calls longjmp
 *
 *****************************************************************************/
void 
timeout(int sig)
{
   sig_caught = sig;
   longjmp(to_vec, 1);
}				/* timeout */
