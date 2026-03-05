#include <stdio.h>
#include <stdlib.h>
#include "mpitest.h"
#define MAX_BUFSIZE 1000000000
#define NUM_REPEAT 20

int main(int argc, char** argv)
{
    MTestArgList *head;
    MPI_Comm comm;
    double tf_start;
    double tf_latency;
    double tf_bw;

    MTest_Init(NULL, NULL);

    printf("TEST p2p_self:\n");
    printf("%12s %10s %12s\n", "msgsize", "latency(sec)", "bandwidth(GB/s)");
    void *send_buf;
    void *recv_buf;
    head = MTestArgListCreate(argc, argv);
    mtest_mem_type_e memtype;
    int device;
    memtype = MTestArgListGetMemType(head, "sendmem");
    device = MTestArgListGetInt_with_default(head, "senddev", 0);
    MTestMalloc(MAX_BUFSIZE, memtype, NULL, &send_buf, device);
    memtype = MTestArgListGetMemType(head, "recvmem");
    device = MTestArgListGetInt_with_default(head, "recvdev", 0);
    MTestMalloc(MAX_BUFSIZE, memtype, NULL, &recv_buf, device);
    MTestArgListDestroy(head);
    if (!send_buf || !recv_buf) {
        printf("! Failed to allocate buffers (size=%d)\n", MAX_BUFSIZE);
        return 1;
    }

    comm = MPI_COMM_SELF;
    int tag = 0;
    int size = 1000000000;

    MPI_Request reqs[2];
    for (int iter = 0; iter<5; iter++) {
        tf_start = MPI_Wtime();
        MPI_Isend(send_buf, size, MPI_CHAR, 0, tag, comm, &reqs[0]);
        MPI_Irecv(recv_buf, size, MPI_CHAR, 0, tag, comm, &reqs[1]);
        MPI_Waitall(2, reqs, MPI_STATUSES_IGNORE);
        tf_latency = MPI_Wtime() - tf_start;
        tf_bw = size / tf_latency / 1e9;
        printf("%12d %10.3f %12.3f\n", size, tf_latency, tf_bw);
    }
    printf("\n");

    MTest_Finalize(0);
    return 0;
}
