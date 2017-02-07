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

#define SIZE (1048576*8)
#define NTIMES 25
#define MAX_THREADS 20

/* multithreaded version of bw.c */

/* two processes. Each creates nthreads threads. 
 * Each thread sends to corresponding thread on other process
 * many times and waits for final ack. */

void *runfunc(void *);

int main(int argc,char *argv[])
{
    int nprocs, i, nthreads, provided, rank;
    pthread_t id[MAX_THREADS];

    MPI_Init_thread(&argc, &argv, MPI_THREAD_MULTIPLE, &provided);
    if (provided != MPI_THREAD_MULTIPLE) {
	printf("Thread multiple needed\n");
	MPI_Abort(MPI_COMM_WORLD, 1);
    }

    MPI_Comm_size(MPI_COMM_WORLD,&nprocs);
    if (nprocs != 2) {
	printf("Run with 2 processes\n");
	MPI_Abort(MPI_COMM_WORLD, 1);
    }
    MPI_Comm_rank(MPI_COMM_WORLD,&rank);

    if (!rank) {
	if (argc != 2) {
	    printf("Error: a.out nthreads\n");
	    MPI_Abort(MPI_COMM_WORLD, 1);
	}
	
	printf("Bandwidth (Mbits/s)\n");

	nthreads = atoi(argv[1]);
	MPI_Send(&nthreads, 1, MPI_INT, 1, 0, MPI_COMM_WORLD);
    }
    else
	MPI_Recv(&nthreads, 1, MPI_INT, 0, 0, MPI_COMM_WORLD, MPI_STATUS_IGNORE);
	
    for (i=0; i<nthreads; i++) {
	pthread_create(&id[i], NULL, runfunc, NULL);
    }
	
    for (i=0; i<nthreads; i++)
	pthread_join(id[i], NULL);

    MPI_Finalize();
    return 0;
}


void *runfunc(void *foo) {
    int rank, src, dest, tag, i;
    double stime, etime, ttime;
    char *buf;

    MPI_Comm_rank(MPI_COMM_WORLD,&rank);

    buf = (char *) malloc(SIZE);
    if (!buf) {
	printf("Cannot allocate buffer\n");
	MPI_Abort(MPI_COMM_WORLD, 1);
    }

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
	printf("%f\n", (SIZE*NTIMES*8/(ttime*1024*1024)));
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
    pthread_exit(NULL);
    return 0;
}
