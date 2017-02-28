/* -*- Mode: C; c-basic-offset:4 ; -*- */
/*
 *   Copyright (C) 2007 University of Chicago
 *   See COPYRIGHT notice in top-level directory.
 */

#include "mpi.h"
#include <stdio.h>
#include <stdlib.h>
#include <strings.h>

#define MAXSIZE 1024
#define NTIMES 1000

/* Measures concurrent latency (for short messages). All even ranks 
   send to rank i+1 and get a reply. Run on 2 nodes (with multiple processes).
 */

int main(int argc,char *argv[])
{
    int rank, nprocs, src, dest, tag, i, size, incr;
    double stime, etime, ttime;
    char *sendbuf, *recvbuf;
    int provided;

    MPI_Init(&argc,&argv);

/*    MPI_Init_thread(&argc, &argv, MPI_THREAD_MULTIPLE, &provided);
    if (provided != MPI_THREAD_MULTIPLE) {
	printf("Thread multiple needed\n");
	MPI_Abort(MPI_COMM_WORLD, 1);
    }
*/

    MPI_Comm_size(MPI_COMM_WORLD,&nprocs);
    MPI_Comm_rank(MPI_COMM_WORLD,&rank);

    sendbuf = (char *) malloc(MAXSIZE);
    if (!sendbuf) {
	printf("Cannot allocate buffer\n");
	MPI_Abort(MPI_COMM_WORLD, 1);
    }

    recvbuf = (char *) malloc(MAXSIZE);
    if (!recvbuf) {
	printf("Cannot allocate buffer\n");
	MPI_Abort(MPI_COMM_WORLD, 1);
    }

    incr = 16;
    if (rank == 0) printf("Size (bytes) \t Time (us)\n");
    MPI_Barrier(MPI_COMM_WORLD);
    /* All even ranks send to (and recv from) rank i+1 many times */

    if ((rank % 2) == 0) { /* even */
	dest = rank + 1;

	for (size=0; size<=MAXSIZE; size+=incr) {
	    tag = 0;
	    stime = MPI_Wtime();
	    for (i=0; i<NTIMES; i++) {
		MPI_Send(sendbuf, size, MPI_BYTE, dest, tag, MPI_COMM_WORLD);
		tag++;
		MPI_Recv(recvbuf, size, MPI_BYTE, dest, tag, MPI_COMM_WORLD, 
			 MPI_STATUS_IGNORE);
	    }
	    etime = MPI_Wtime();

	    ttime = (etime - stime)/(2*NTIMES);
	    if (rank == 0) printf("%d \t %f\n", size, ttime*1000000);

	    if (size == 256) incr = 64;
	}
    }
    else {  /* odd */
	src = rank - 1;

	for (size=0; size<=MAXSIZE; size+=incr) {
	    tag = 0;
	    for (i=0; i<NTIMES; i++) {
		MPI_Recv(recvbuf, size, MPI_BYTE, src, tag, MPI_COMM_WORLD, 
			 MPI_STATUS_IGNORE);    
		tag++;
		MPI_Send(sendbuf, size, MPI_BYTE, src, tag, MPI_COMM_WORLD);
	    }
	    if (size == 256) incr = 64;
	}
    }

    free(sendbuf);
    free(recvbuf);

    MPI_Finalize();
    return 0;
}
