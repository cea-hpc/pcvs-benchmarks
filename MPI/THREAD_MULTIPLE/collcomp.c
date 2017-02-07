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

#define SIZE 1048576*4
#define NTIMES 10
#define MAX_THREADS 10
#define MAT_SIZE (SIZE/4)

/* Evaluates the ability of using a thread to overlap collective
 * communication with computation in other threads */ 

void *runfunc(void *);
int nthreads;
volatile int flag=0;

int main(int argc,char *argv[])
{
    int rank, i, provided;
    pthread_t id[MAX_THREADS];
    int thread_ranks[MAX_THREADS];

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
	
	printf("Iterations per sec\n");

	nthreads = atoi(argv[1]);
	MPI_Bcast(&nthreads, 1, MPI_INT, 0, MPI_COMM_WORLD);
    }
    else
	MPI_Bcast(&nthreads, 1, MPI_INT, 0, MPI_COMM_WORLD);

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
    int rank, i, count;
    double stime, etime;
    int *inbuf, *outbuf;
    MPI_Comm comm;
    int *A, *B, *C, k;
    float results_per_sec;

    if ((* (int *)thread_rank) == 0) {
	/* do collective communication */

	MPI_Comm_rank(MPI_COMM_WORLD, &rank);

	MPI_Comm_split(MPI_COMM_WORLD, 0, rank, &comm);

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
	
	for (i=0; i<NTIMES; i++) {
	    MPI_Allreduce(inbuf, outbuf, SIZE, MPI_INT, MPI_MAX, comm);
	}

	/* set flag to 1 to indicate completion */

	flag = 1;
	free(inbuf);
	free(outbuf);
	MPI_Comm_free(&comm);
    }

    else {
	/* do computation */

	A = (int *) malloc(SIZE);
	if (!A) {
	    printf("Cannot allocate buffer\n");
	    MPI_Abort(MPI_COMM_WORLD, 1);
	}
	
	B = (int *) malloc(SIZE);
	if (!B) {
	    printf("Cannot allocate buffer\n");
	    MPI_Abort(MPI_COMM_WORLD, 1);
	}
	
	C = (int *) malloc(SIZE);
	if (!C) {
	    printf("Cannot allocate buffer\n");
	    MPI_Abort(MPI_COMM_WORLD, 1);
	}

	k = 0;
	stime = MPI_Wtime();
        do { 
	    k++;

            for (i=0; i<MAT_SIZE; i++) {
                A[i] = B[i] * C[i];
            }
            
            for (i=0; i<MAT_SIZE; i++) {
                B[i] = A[i] + C[i];
            }
            
            for (i=0; i<MAT_SIZE; i++) {
                A[i] = B[i] * C[i];
            }
            
            for (i=0; i<MAT_SIZE; i++) {
		count += A[i];
            }
	    sched_yield();
        } while (!flag);
	etime = MPI_Wtime();

	results_per_sec = k/(etime-stime);

	printf("%f\n", results_per_sec);
    }

    pthread_exit(NULL);
    return 0;
}
