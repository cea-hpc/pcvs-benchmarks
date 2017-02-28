/* -*- Mode: C; c-basic-offset:4 ; -*- */
/*
 *   Copyright (C) 2007 University of Chicago
 *   See COPYRIGHT notice in top-level directory.
 */

#include "mpi.h"
#include <stdio.h>
#include <stdlib.h>
#include <strings.h>

#define SHORT_SIZE (128)
#define LONG_SIZE (1048576*4)
#define NTIMES_SHORT 96
#define NTIMES_LONG 1

/* 4 processes: ranks 0 and 1 run on 1 node, rank 2 on another node,
 * rank 3 on a 3rd node. Rank 0 sends a very long message to rank
 * 2. Rank 1 sends (using MPI_Ssend) a series of short messages to
 * rank 3. Measure the variation of the time taken by the short
 * messages. */    


int main(int argc,char *argv[])
{
    int rank, nprocs, tag, i;
    double stime, etime;
    char *buf;

    MPI_Init(&argc,&argv);

    MPI_Comm_size(MPI_COMM_WORLD,&nprocs);
    if (nprocs != 4) {
	printf("Run with 4 processes\n");
	MPI_Abort(MPI_COMM_WORLD, 1);
    }
    MPI_Comm_rank(MPI_COMM_WORLD,&rank);

    tag = 0;

    MPI_Barrier(MPI_COMM_WORLD);

    if (rank == 0) {
	/* send a long message to rank 2 */

	buf = (char *) malloc(LONG_SIZE);
	if (!buf) {
	    printf("Cannot allocate buffer\n");
	    MPI_Abort(MPI_COMM_WORLD, 1);
	}

	for (i=0; i<NTIMES_LONG; i++) {
	    MPI_Send(buf, LONG_SIZE, MPI_BYTE, 2, tag, MPI_COMM_WORLD);
	    tag++;
	}
    }

    if (rank == 1) {
	/* Ssend a series of short messages to rank 3 */

	buf = (char *) malloc(SHORT_SIZE);
	if (!buf) {
	    printf("Cannot allocate buffer\n");
	    MPI_Abort(MPI_COMM_WORLD, 1);
	}

	printf("Iteration \t Time (us)\n");
	for (i=0; i<NTIMES_SHORT; i++) {
	    stime = MPI_Wtime();
	    MPI_Ssend(buf, SHORT_SIZE, MPI_BYTE, 3, tag, MPI_COMM_WORLD);
	    etime = MPI_Wtime();
	    printf("%d \t %f \n", i+1, (etime-stime)*1000000);
	    tag++;
	}
    }

    if (rank == 2) {
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

    if (rank == 3) {
	/* recv a series of short messages from rank 1 */

	buf = (char *) malloc(SHORT_SIZE);
	if (!buf) {
	    printf("Cannot allocate buffer\n");
	    MPI_Abort(MPI_COMM_WORLD, 1);
	}

	for (i=0; i<NTIMES_SHORT; i++) {
	    MPI_Recv(buf, SHORT_SIZE, MPI_BYTE, 1, tag, MPI_COMM_WORLD, 
		     MPI_STATUS_IGNORE); 
	    tag++;
	}
    }

    free(buf);

    MPI_Finalize();
    return 0;
}
