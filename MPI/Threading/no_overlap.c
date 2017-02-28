/* -*- Mode: C; c-basic-offset:4 ; -*- */
/*
 *   Copyright (C) 2007 University of Chicago
 *   See COPYRIGHT notice in top-level directory.
 */

#include "mpi.h"
#include <stdio.h>
#include <stdlib.h>
#include <strings.h>

#define SIZE (1048576*4)
#define MAT_SIZE 1048576
#define NTIMES 10
#define NEIGHBORS 4

/* simple nearest neighbor communication with Isend/Irecv, followed by
 * computation, and a Waitall */

int main(int argc,char *argv[])
{
    int nprocs, i, rank, count, dest[10], src[10], j;
    char *buf1[10], *buf2[10];
    int *A, *B, *C, cnt, k;
    MPI_Request req[20];
    double start_ctime, end_ctime, total_time, compute_time=0.0, comm_time;
    double stime, etime;

    MPI_Init(&argc, &argv);

    MPI_Comm_size(MPI_COMM_WORLD,&nprocs);
    MPI_Comm_rank(MPI_COMM_WORLD,&rank);

    for (i=0; i<NEIGHBORS; i++) {
        buf1[i] = (char *) malloc(SIZE);
        if (!buf1[i]) {
            printf("Cannot allocate buffer\n");
            MPI_Abort(MPI_COMM_WORLD, 1);
        }
    }

    for (i=0; i<NEIGHBORS; i++) {
        buf2[i] = (char *) malloc(SIZE);
        if (!buf2[i]) {
            printf("Cannot allocate buffer\n");
            MPI_Abort(MPI_COMM_WORLD, 1);
        }
    }

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

    for (i=0; i<NEIGHBORS; i++) {
        dest[i] = (rank + i + 1) % nprocs;
        src[i] = (rank - i - 1 + nprocs) % nprocs;
    }


    if (rank == 0) {
	printf("Time per iteration on each process (ms)\n");
	printf("Time \t Compute time \t Comm time\n");
    }

    MPI_Barrier(MPI_COMM_WORLD);
    stime = MPI_Wtime();

    for (j=0; j<NTIMES; j++) {
        cnt = 0;
        for (i=0; i<NEIGHBORS; i++) {
            MPI_Irecv(buf2[i], SIZE, MPI_BYTE, src[i], 100, MPI_COMM_WORLD, &req[cnt]);
            cnt++;
        }

        for (i=0; i<NEIGHBORS; i++) {
            MPI_Isend(buf1[i], SIZE, MPI_BYTE, dest[i], 100, MPI_COMM_WORLD, &req[cnt]);
            cnt++;
        }

        start_ctime = MPI_Wtime();

        for (k=0; k<50; k++) {
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
        }

        end_ctime = MPI_Wtime();
        compute_time += end_ctime - start_ctime;
        
        MPI_Waitall(cnt, req, MPI_STATUSES_IGNORE);
    }

    MPI_Barrier(MPI_COMM_WORLD);
    etime = MPI_Wtime();

    MPI_Finalize();

    total_time = ((etime - stime)/NTIMES)*1000;   /* millisec */
    compute_time = (compute_time/NTIMES)*1000;
    comm_time = total_time - compute_time;

    printf("%.2f \t %.2f \t %.2f\n", 
	   total_time, compute_time, comm_time);

    free(A);
    free(B);
    free(C);

    return 0;
}
