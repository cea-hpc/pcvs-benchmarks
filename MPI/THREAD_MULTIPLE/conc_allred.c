/* -*- Mode: C; c-basic-offset:4 ; -*- */
/*
 *   Copyright (C) 2007 University of Chicago
 *   See COPYRIGHT notice in top-level directory.
 */

#include "mpi.h"
#include <stdio.h>
#include <stdlib.h>
#include <strings.h>

#define SIZE 512
#define NTIMES 50

/* Measures the time taken by concurrent calls to MPI_Allreduce 
   by multiple processes on a node. 
 */

int main(int argc,char *argv[])
{
    int rank, i, nprocs_per_node;
    double stime, etime;
    int *inbuf, *outbuf;
    MPI_Comm comm;

    MPI_Init(&argc,&argv);

    MPI_Comm_rank(MPI_COMM_WORLD,&rank);

    if (!rank) {
	if (argc != 2) {
	    printf("Error: a.out nprocs_per_node\n");
	    MPI_Abort(MPI_COMM_WORLD, 1);
	}

	printf("Time (ms)\n");
	nprocs_per_node = atoi(argv[1]);
	MPI_Bcast(&nprocs_per_node, 1, MPI_INT, 0, MPI_COMM_WORLD);
    }
    else
	MPI_Bcast(&nprocs_per_node, 1, MPI_INT, 0, MPI_COMM_WORLD);

    inbuf = (int *) malloc(SIZE*sizeof(int));
    if (!inbuf) {
	printf("Cannot allocate buffer\n");
	MPI_Abort(MPI_COMM_WORLD, 1);
    }

    outbuf = (int *) malloc(SIZE*sizeof(int));
    if (!outbuf) {
	printf("Cannot allocate buffer\n");
	MPI_Abort(MPI_COMM_WORLD, 1);
    }

    MPI_Comm_split(MPI_COMM_WORLD, rank % nprocs_per_node, rank, &comm);

    MPI_Barrier(MPI_COMM_WORLD);

    stime = MPI_Wtime();
    for (i=0; i<NTIMES; i++) {
	MPI_Allreduce(inbuf, outbuf, SIZE, MPI_INT, MPI_MAX, comm);
    }
    MPI_Barrier(MPI_COMM_WORLD);
    etime = MPI_Wtime();

    printf("%f\n", ((etime-stime)*1000)/NTIMES);

    free(inbuf);
    free(outbuf);
    MPI_Comm_free(&comm);

    MPI_Finalize();
    return 0;
}
