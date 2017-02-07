/* -*- Mode: C; c-basic-offset:4 ; -*- */
/*
 *   Copyright (C) 2007 University of Chicago
 *   See COPYRIGHT notice in top-level directory.
 */

#include "mpi.h"
#include <stdio.h>

#define NTIMES 10000

/* Measures message rate for 0-byte messages. All even ranks send to 
   rank i+1. Run on 2 nodes (with multiple processes).
 */

int main(int argc,char *argv[])
{
    int rank, nprocs, src, dest, tag=0, i;
    double stime, etime, ttime;
    char sendbuf, recvbuf;

    MPI_Init(&argc,&argv);

    MPI_Comm_size(MPI_COMM_WORLD,&nprocs);
    MPI_Comm_rank(MPI_COMM_WORLD,&rank);

    MPI_Barrier(MPI_COMM_WORLD);
    /* All even ranks send to (and recv from) rank i+1 many times */

    if ((rank % 2) == 0) { /* even */
	dest = rank + 1;

	stime = MPI_Wtime();
	for (i=0; i<NTIMES; i++) {
	    MPI_Send(&sendbuf, 0, MPI_BYTE, dest, tag, MPI_COMM_WORLD);

	    if (i % 256 == 0) {
		MPI_Recv(&recvbuf, 0, MPI_BYTE, dest, tag, MPI_COMM_WORLD, 
			 MPI_STATUS_IGNORE);
	    }
	}
	MPI_Recv(&recvbuf, 0, MPI_BYTE, dest, tag, MPI_COMM_WORLD, 
		 MPI_STATUS_IGNORE);
	etime = MPI_Wtime();

	ttime = (etime - stime)/NTIMES;

	printf("Rank %d, message rate %f messages/sec\n", rank, 1/ttime);
    }
    else {  /* odd */
	src = rank - 1;

	for (i=0; i<NTIMES; i++) {
	    MPI_Recv(&recvbuf, 0, MPI_BYTE, src, tag, MPI_COMM_WORLD, 
		     MPI_STATUS_IGNORE);    

	    if (i % 256 == 0)
		MPI_Send(&sendbuf, 0, MPI_BYTE, src, tag, MPI_COMM_WORLD);

	}
	MPI_Send(&sendbuf, 0, MPI_BYTE, src, tag, MPI_COMM_WORLD);
    }


    MPI_Finalize();
    return 0;
}
