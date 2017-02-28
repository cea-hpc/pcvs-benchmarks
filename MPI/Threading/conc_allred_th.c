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

#define SIZE 512
#define NTIMES 50
#define MAX_THREADS 10

/* multithreaded version of conc_allred_th.c */
/* Measures the time taken by concurrent calls to MPI_Allreduce 
   by multiple threads on a node. 
 */

void *runfunc(void *);

int main(int argc,char *argv[])
{
    int rank, i, provided;
    pthread_t id[MAX_THREADS];
    MPI_Comm comm[MAX_THREADS];
    int nthreads;

    MPI_Init_thread(&argc, &argv, MPI_THREAD_MULTIPLE, &provided);
    if (provided != MPI_THREAD_MULTIPLE) {
	printf("Thread multiple needed\n");
	MPI_Abort(MPI_COMM_WORLD, 1);
    }

    MPI_Comm_rank(MPI_COMM_WORLD, &rank);

    if (!rank) {
	if (argc != 2) {
	    printf("Error: a.out nthreads\n");
	    MPI_Abort(MPI_COMM_WORLD, 1);
	}
	
	printf("Time (ms)\n");
	nthreads = atoi(argv[1]);
	MPI_Bcast(&nthreads, 1, MPI_INT, 0, MPI_COMM_WORLD);
    }
    else
	MPI_Bcast(&nthreads, 1, MPI_INT, 0, MPI_COMM_WORLD);

    MPI_Barrier(MPI_COMM_WORLD);

    for (i=0; i<nthreads; i++) {
	MPI_Comm_dup(MPI_COMM_WORLD, &comm[i]);
	pthread_create(&id[i], NULL, runfunc, (void *) &comm[i]);
    }
	
    for (i=0; i<nthreads; i++)
	pthread_join(id[i], NULL);

    MPI_Finalize();
    return 0;
}



void *runfunc(void *comm) {
    int rank, i, color;
    double stime, etime;
    int *inbuf, *outbuf;

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

    stime = MPI_Wtime();
    for (i=0; i<NTIMES; i++) {
	MPI_Allreduce(inbuf, outbuf, SIZE, MPI_INT, MPI_MAX, *(MPI_Comm *)comm);
    }
    etime = MPI_Wtime();

    printf("%f\n", ((etime-stime)*1000)/NTIMES);

    free(inbuf);
    free(outbuf);

    pthread_exit(NULL);
    return 0;
}
