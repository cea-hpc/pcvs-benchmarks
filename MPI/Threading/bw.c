/* -*- Mode: C; c-basic-offset:4 ; -*- */
/*
 *   Copyright (C) 2007 University of Chicago
 *   See COPYRIGHT notice in top-level directory.
 */

#include "mpi.h"
#include <stdio.h>
#include <stdlib.h>
#include <strings.h>

#define SIZE (1048576*8)
#define NTIMES 25

/* Measures concurrent bandwidth. All even ranks send to rank i+1 many
 * times and wait for final ack. Run on 2 nodes (with multiple processes).
 */

int main(int argc,char *argv[])
{
    int rank, nprocs, src, dest, tag, i;
    double stime, etime, ttime;
    char *buf;

    MPI_Init(&argc,&argv);

    MPI_Comm_size(MPI_COMM_WORLD,&nprocs);
    MPI_Comm_rank(MPI_COMM_WORLD,&rank);

    buf = (char *) malloc(SIZE);
    if (!buf) {
	printf("Cannot allocate buffer\n");
	MPI_Abort(MPI_COMM_WORLD, 1);
    }

    if (rank == 0) printf("Bandwidth (Mbits/s)\n");

    /* All even ranks send to rank i+1 many times and wait for final ack */

    tag = 0;
    if ((rank % 2) == 0) { /* even */
	dest = rank + 1;
	stime = MPI_Wtime();
	for (i=0; i<NTIMES; i++) {
	    MPI_Send(buf, SIZE, MPI_BYTE, dest, tag, MPI_COMM_WORLD);
	    tag++;
	}
	MPI_Recv(buf, 0, MPI_BYTE, dest, tag, MPI_COMM_WORLD, MPI_STATUS_IGNORE);
	etime = MPI_Wtime();

	ttime = etime - stime;
	printf("%f\n", (SIZE*NTIMES*8/(ttime*1024*1024)) );

    }
    else {  /* odd */
	src = rank - 1;
	for (i=0; i<NTIMES; i++) {
	    MPI_Recv(buf, SIZE, MPI_BYTE, src, tag, MPI_COMM_WORLD, MPI_STATUS_IGNORE);
	    tag++;
	}
	MPI_Send(buf, 0, MPI_BYTE, src, tag, MPI_COMM_WORLD);
    }

    free(buf);
    MPI_Finalize();
    return 0;
}
