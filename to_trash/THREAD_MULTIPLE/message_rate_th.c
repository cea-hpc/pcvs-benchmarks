/* -*- Mode: C; c-basic-offset:4 ; -*- */
/*
 *   Copyright (C) 2007 University of Chicago
 *   See COPYRIGHT notice in top-level directory.
 */

#include "mpi.h"
#include <stdio.h>
#include <pthread.h>
#include "stdlib.h"

#define NTIMES 10000
#define MAX_THREADS 10

/* multithreaded version of message_rate.c */

/* two processes. each has NTHREADS threads including the main
 * thread. Each thread sends to corresponding thread 
 * on other process many times. */

void *runfunc(void *);

int main(int argc,char *argv[])
{
    int rank, nprocs, i, nthreads, provided;
    pthread_t id[MAX_THREADS];
    int thread_ranks[MAX_THREADS];

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
	
	nthreads = atoi(argv[1]);
	MPI_Send(&nthreads, 1, MPI_INT, 1, 0, MPI_COMM_WORLD);
    }
    else
	MPI_Recv(&nthreads, 1, MPI_INT, 0, 0, MPI_COMM_WORLD, MPI_STATUS_IGNORE);

	
    for (i=0; i<nthreads; i++) {
	thread_ranks[i] = i;
	pthread_create(&id[i], NULL, runfunc, (void *) &thread_ranks[i]);
    }
	
    for (i=0; i<nthreads; i++)
	pthread_join(id[i], NULL);

    MPI_Finalize();
    return 0;
}


void *runfunc(void *thread_rank) {
    int rank, src, dest, tag, i;
    double stime, etime, ttime;
    char sendbuf, recvbuf;

    MPI_Comm_rank(MPI_COMM_WORLD,&rank);

    /* All even ranks send to (and recv from) rank i+1 many times */
    tag = * (int *)thread_rank;
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

	printf("Thread %d, message rate %f messages/sec\n", tag, 1/ttime);
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

    pthread_exit(NULL);
    return 0;
}
