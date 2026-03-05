#include <stdio.h>
#include <stdlib.h>
#include "mpitest.h"
#include <math.h>

void bench_p2p(MPI_Comm comm, int src, int dst, void *buf, int size);
int bench_warmup(MPI_Comm comm, int dst, void *buf, int size);
double bench_send(int iter, MPI_Comm comm, int dst, void *buf, int size);
void bench_recv(MPI_Comm comm, int src, void *buf, int size);

int grank;
int gsize;
int gsrc;
int gdst;
MPI_Win win;
#define WINDOW_SIZE 100
#define MAX_BUFSIZE 5000000
#define NUM_REPEAT 20
#define TAG 0
#define SYNC_TAG 100

int main(int argc, char** argv)
{
    MTestArgList *head;

    MTest_Init(NULL, NULL);

    MPI_Comm_rank(MPI_COMM_WORLD, &grank);
    MPI_Comm_size(MPI_COMM_WORLD, &gsize);

    if (gsize % 2 == 1) {
        printf("! Test put_bw requires even number of processes to form even/odd pairs !\n");
    }
    if (grank % 2 == 0) {
        gsrc = grank;
        gdst = grank + 1;
    } else {
        gsrc = grank - 1;
        gdst = grank;
    }

    MPI_Comm comm = MPI_COMM_WORLD;

    void *buf;
    head = MTestArgListCreate(argc, argv);
    if (grank == gsrc) {
        mtest_mem_type_e memtype;
        int device;
        memtype = MTestArgListGetMemType(head, "sendmem");
        device = MTestArgListGetInt_with_default(head, "senddev", grank);
        MTestMalloc(MAX_BUFSIZE, memtype, NULL, &buf, device);
        MTestPrintfMsg(1, "[%d] Allocating buffer: memtype=%s, device=%d, size=%d\n", grank, MTest_memtype_name(memtype), device, MAX_BUFSIZE);
    } else if (grank == gdst) {
        mtest_mem_type_e memtype;
        int device;
        memtype = MTestArgListGetMemType(head, "recvmem");
        device = MTestArgListGetInt_with_default(head, "recvdev", grank);
        MTestMalloc(MAX_BUFSIZE, memtype, NULL, &buf, device);
        MTestPrintfMsg(1, "[%d] Allocating buffer: memtype=%s, device=%d, size=%d\n", grank, MTest_memtype_name(memtype), device, MAX_BUFSIZE);
    }
    MTestArgListDestroy(head);
    if (!buf) {
        printf("! Failed to allocate buffer (size=%d)\n", MAX_BUFSIZE);
        return 1;
    }

    if (grank == 0) {
        MPI_Win_create(NULL, 0, 1, MPI_INFO_NULL, MPI_COMM_WORLD, &win);
    } else {
        MPI_Win_create(buf, MAX_BUFSIZE, 1, MPI_INFO_NULL, MPI_COMM_WORLD, &win);
    }

    if (grank == 0) {
        printf("TEST put_bw:\n");
        printf("%12s %10s(us) %6s(us) %12s(MB/s)\n", "msgsize", "latency", "sigma", "bandwidth");
    }
    for (int size = 1; size < MAX_BUFSIZE; size *= 2) {
        bench_p2p(comm, 0, 1, buf, size);
    }
    if (grank == 0) {
        printf("\n");
    }

    MPI_Win_free(&win);

    MTest_Finalize(0);
    return 0;
}

void bench_p2p(MPI_Comm comm, int src, int dst, void *buf, int size)
{
    int iter;
    double tf_latency;

    int rank;
    MPI_Comm_rank(comm, &rank);

    if (rank == src) {
        iter = bench_warmup(comm, dst, buf, size);
        double sum1 = 0;
        double sum2 = 0;
        for (int i = 0; i<NUM_REPEAT; i++) {
            tf_latency = bench_send(iter, comm, dst, buf, size);
            tf_latency /= iter;
            sum1 += tf_latency;
            sum2 += tf_latency * tf_latency;
        }
        sum1 /= NUM_REPEAT;
        sum2 /= NUM_REPEAT;
        sum2 = sqrt(sum2 - sum1 * sum1);
        double tf_latency;
        double tf_sigma;
        double tf_bw;
        tf_latency = sum1 / (WINDOW_SIZE) * 1e6;
        tf_sigma = sum2 / (WINDOW_SIZE) * 1e6;
        tf_bw = size / tf_latency;
        printf("%12d %10.3f     %6.3f     %12.3f\n", size, tf_latency, tf_sigma, tf_bw);
        iter = 0;
        MPI_Send(&iter, 1, MPI_INT, dst, SYNC_TAG, comm);
    } else if (rank == dst) {
        bench_recv(comm, src, buf, size);
    }
}

int bench_warmup(MPI_Comm comm, int dst, void *buf, int size)
{
    int iter;
    double tf_dur;

    iter = 2;
    double last_dur = 1.0;
    int num_best = 0;
    while (num_best < 10) {
        tf_dur = bench_send(iter, comm, dst, buf, size);
        int min_iter = (int) (iter * 0.001 / tf_dur);
        if (iter < 10000 && iter < min_iter) {
            iter = min_iter;
            num_best = 0;
            continue;
        }
        if (tf_dur > last_dur) {
            num_best++;
        }
        last_dur = tf_dur;
    }
    return iter;
}

double bench_send(int iter, MPI_Comm comm, int dst, void *buf, int size)
{
    double tf_start;
    double tf_dur;

    MPI_Send(&iter, 1, MPI_INT, dst, SYNC_TAG, comm);

    tf_start = MPI_Wtime();
    for (int i = 0; i<iter; i++) {
        MPI_Win_fence(0, win);
        for (int j = 0; j<WINDOW_SIZE; j++) {
            MPI_Put(buf, size, MPI_CHAR, dst, 0, size, MPI_CHAR, win);
        }
        MPI_Win_fence(0, win);
    }
    tf_dur = MPI_Wtime() - tf_start;

    return tf_dur;
}

void bench_recv(MPI_Comm comm, int src, void *buf, int size)
{
    while (1) {
        int iter;
        MPI_Recv(&iter, 1, MPI_INT, src, SYNC_TAG, comm, MPI_STATUS_IGNORE);
        if (iter == 0) {
            break;
        }
        for (int i = 0; i<iter; i++) {
            MPI_Win_fence(0, win);
            MPI_Win_fence(0, win);
        }
    }
}
