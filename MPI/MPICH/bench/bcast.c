#include <stdio.h>
#include <stdlib.h>
#include "mpitest.h"
#include <math.h>

void measure_bcast(int iter, int root, MPI_Comm comm, unsigned char *buf, int size, double *pf_min, double *pf_max, double *pf_avg, double *pf_sigma);

int grank;
int gsize;
int root = 0;
#define MAX_BUFSIZE 5000000
#define NUM_REPEAT 20

int main(int argc, char** argv)
{
    MTestArgList *head;
    int iter;
    double tf_dur;

    MTest_Init(NULL, NULL);

    MPI_Comm_rank(MPI_COMM_WORLD, &grank);
    MPI_Comm_size(MPI_COMM_WORLD, &gsize);

    MPI_Comm comm = MPI_COMM_WORLD;

    void *buf;
    head = MTestArgListCreate(argc, argv);
    mtest_mem_type_e memtype;
    int device;
    memtype = MTestArgListGetMemType(head, "memtype");
    device = MTestArgListGetInt_with_default(head, "device", grank);
    MTestMalloc(MAX_BUFSIZE, memtype, NULL, &buf, device);
    MTestPrintfMsg(1, "[%d] Allocating buffer: memtype=%s, device=%d, size=%d\n", grank, MTest_memtype_name(memtype), device, MAX_BUFSIZE);
    MTestArgListDestroy(head);
    if (!buf) {
        printf("! Failed to allocate buffer (size=%d)\n", MAX_BUFSIZE);
        return 1;
    }

    if (grank == 0) {
        printf("TEST bcast:\n");
    }
    if (grank == 0) {
        printf("%12s %8s %8s %8s     %6s  (in microseconds)\n", "msgsize", "min", "max", "avg", "sigma");
    }
    for (int size = 0; size < 5000000; size = (size==0)?1:size*2) {
        double tf_min;
        double tf_max;
        double tf_avg;
        double tf_sigma;
        if (grank == 0) {
            iter = 2;
            double last_dur = 1.0;
            int num_best = 0;
            while (num_best < 10) {
                MPI_Bcast(&iter, 1, MPI_INT, 0, comm);
                measure_bcast(iter, root, comm, buf, size, &tf_min, &tf_max, &tf_avg, &tf_sigma);
                tf_dur = tf_max;
                int min_iter = 0.001/tf_max;
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
            int tn_zero = 0;
            MPI_Bcast(&tn_zero, 1, MPI_INT, 0, comm);
        } else {
            while (1) {
                MPI_Bcast(&iter, 1, MPI_INT, 0, comm);
                if (iter == 0) {
                    break;
                }
                measure_bcast(iter, root, comm, buf, size, &tf_min, &tf_max, &tf_avg, &tf_sigma);
                tf_dur = tf_max;
            }
        }

        MPI_Bcast(&iter, 1, MPI_INT, 0, comm);
        if (iter < 100) {
            iter = 100;
        }
        measure_bcast(iter, root, comm, buf, size, &tf_min, &tf_max, &tf_avg, &tf_sigma);
        if (grank == 0) {
            printf("%12d %8.3f %8.3f %8.3f     %6.3f\n", size, tf_min * 1e6, tf_max * 1e6, tf_avg * 1e6, tf_sigma * 1e6);
        }
    }
    if (grank == 0) {
        printf("\n");
    }

    MTest_Finalize(0);
    return 0;
}

void measure_bcast(int iter, int root, MPI_Comm comm, unsigned char *buf, int size, double *pf_min, double *pf_max, double *pf_avg, double *pf_sigma)
{
    double tf_start;
    double tf_latency;

    double tf_max;
    double tf_min;
    double tf_avg;
    double tf_sigma;

    double sum1 = 0;
    double sum2 = 0;
    for (int i = 0; i<iter; i++) {
        MPI_Barrier(comm);
        tf_start = MPI_Wtime();
        MPI_Bcast(buf, size, MPI_CHAR, root, comm);
        tf_latency = MPI_Wtime() - tf_start;
        sum1 += tf_latency;
        sum2 += tf_latency * tf_latency;
    }
    sum1 /= iter;
    sum2 /= iter;
    sum2 = sum2 - sum1 * sum1;

    MPI_Reduce(&sum1, &tf_max, 1, MPI_DOUBLE, MPI_MAX, 0, comm);
    MPI_Reduce(&sum1, &tf_min, 1, MPI_DOUBLE, MPI_MIN, 0, comm);
    MPI_Reduce(&sum1, &tf_avg, 1, MPI_DOUBLE, MPI_SUM, 0, comm);
    MPI_Reduce(&sum2, &tf_sigma, 1, MPI_DOUBLE, MPI_SUM, 0, comm);
    tf_avg /= gsize;
    tf_sigma = sqrt(tf_sigma / gsize);
    *pf_min = tf_min;
    *pf_max = tf_max;
    *pf_avg = tf_avg;
    *pf_sigma = tf_sigma;
}
