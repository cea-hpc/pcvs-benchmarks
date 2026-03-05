#include <stdio.h>
#include <stdlib.h>
#include "mpitest.h"

int grank;
int gsize;
int gsrc;
int gdst;
#define TAG 0
#define MAX_BUFSIZE 1000000000
#define NUM_REPEAT 20

int main(int argc, char** argv)
{
    MTestArgList *head;
    int size;
    double tf_start;
    double tf_latency;
    double tf_bw;

    MTest_Init(NULL, NULL);

    MPI_Comm_rank(MPI_COMM_WORLD, &grank);
    MPI_Comm_size(MPI_COMM_WORLD, &gsize);

    if (gsize % 2 == 1) {
        printf("! Test p2p_one requires even number of processes to form even/odd pairs !\n");
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
        printf("TEST p2p_one:\n");
        printf("%12s %10s %12s\n", "msgsize", "latency(sec)", "bandwidth(GB/s)");
    }
    size = MAX_BUFSIZE;

    for (int iter = 0; iter<5; iter++) {
        if (grank == gsrc) {
            tf_start = MPI_Wtime();
            MPI_Send(buf, size, MPI_CHAR, gdst, TAG, comm);
            MPI_Recv(NULL, 0, MPI_DATATYPE_NULL, gdst, TAG, comm, MPI_STATUS_IGNORE);
            tf_latency = MPI_Wtime() - tf_start;
            tf_bw = size / tf_latency / 1e9;
            printf("%12d %10.3f %12.3f\n", size, tf_latency, tf_bw);
        } else if (grank == gdst) {
            MPI_Recv(buf, size, MPI_CHAR, gsrc, TAG, comm, MPI_STATUS_IGNORE);
            MPI_Send(NULL, 0, MPI_DATATYPE_NULL, gsrc, TAG, comm);
        }
    }
    if (grank == 0) {
        printf("\n");
    }

    MTest_Finalize(0);
    return 0;
}
