/* -*- Mode: C; c-basic-offset:4 ; -*- */
/*
 *   Copyright (C) 2007 University of Chicago
 *   See COPYRIGHT notice in top-level directory.
 */

#include "mpi.h"
#include <stdio.h>
#include <pthread.h>
#include <stdlib.h>
#include <strings.h>

#define SHORT_SIZE (128)
#define LONG_SIZE (1048576*4)
#define NTIMES_SHORT 96
#define NTIMES_LONG 1

/* MT version of shortlong.c. Rank 0 comprises 2 threads: one sends a
 * long message to rank 1, the other sends a series of short messages
 * to rank 2. The 3 processes are run on different nodes. */

void *runfunc(void *);

int main(int argc,char *argv[])
{
    int rank, nprocs, tag, i, provided;
    char *buf;
    pthread_t id;

    MPI_Init_thread(&argc, &argv, MPI_THREAD_MULTIPLE, &provided);
    if (provided != MPI_THREAD_MULTIPLE) {
	printf("Thread multiple needed\n");
	MPI_Abort(MPI_COMM_WORLD, 1);
    }

    MPI_Comm_size(MPI_COMM_WORLD,&nprocs);
    if (nprocs != 3) {
	printf("Run with 3 processes\n");
	MPI_Abort(MPI_COMM_WORLD, 1);
    }
    MPI_Comm_rank(MPI_COMM_WORLD,&rank);

    MPI_Barrier(MPI_COMM_WORLD);
    tag = 0;
    if (rank == 0) {

	/* create a separate thread that sends a series of short
	   messages to rank 2 */
	pthread_create(&id, NULL, runfunc, NULL);

	/* send a long message to rank 1 */
	buf = (char *) malloc(LONG_SIZE);
	if (!buf) {
	    printf("Cannot allocate buffer\n");
	    MPI_Abort(MPI_COMM_WORLD, 1);
	}

	for (i=0; i<NTIMES_LONG; i++) {
	    MPI_Send(buf, LONG_SIZE, MPI_BYTE, 1, tag, MPI_COMM_WORLD);
	    tag++;
	}

	pthread_join(id, NULL);
    }

    if (rank == 1) {
	/* recv a long message from rank 0 */

	buf = (char *) malloc(LONG_SIZE);
	if (!buf) {
	    printf("Cannot allocate buffer\n");
	    MPI_Abort(MPI_COMM_WORLD, 1);
	}

	for (i=0; i<NTIMES_LONG; i++) {
	    MPI_Recv(buf, LONG_SIZE, MPI_BYTE, 0, tag, MPI_COMM_WORLD, 
                     MPI_STATUS_IGNORE);  
	    tag++;
	}
    }

    if (rank == 2) {
	/* recv a series of short messages from rank 0 */

	buf = (char *) malloc(SHORT_SIZE);
	if (!buf) {
	    printf("Cannot allocate buffer\n");
	    MPI_Abort(MPI_COMM_WORLD, 1);
	}

	for (i=0; i<NTIMES_SHORT; i++) {
	    MPI_Recv(buf, SHORT_SIZE, MPI_BYTE, 0, tag, MPI_COMM_WORLD, 
		     MPI_STATUS_IGNORE);  
	    tag++;
	}
    }

    free(buf);

    MPI_Finalize();
    return 0;
}


void *runfunc(void *foo) {
    int tag, i;
    double stime, etime;
    char *buf;

    /* Ssend a series of short messages to rank 2 */

    buf = (char *) malloc(SHORT_SIZE);
    if (!buf) {
	printf("Cannot allocate buffer\n");
	MPI_Abort(MPI_COMM_WORLD, 1);
    }

    tag = 0;
    printf("Iteration \t Time (us)\n");
    for (i=0; i<NTIMES_SHORT; i++) {
	stime = MPI_Wtime();
	MPI_Ssend(buf, SHORT_SIZE, MPI_BYTE, 2, tag, MPI_COMM_WORLD);
	etime = MPI_Wtime();
	printf("%d \t %f \n", i+1, (etime-stime)*1000000);
	tag++;
    }

    free(buf);
    pthread_exit(NULL);
    return 0;
}
