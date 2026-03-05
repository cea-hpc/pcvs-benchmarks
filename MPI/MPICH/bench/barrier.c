#include <stdio.h>
#include <stdlib.h>
#include <mpi.h>
#include <math.h>

double bench_barrier(MPI_Comm comm);

int grank;
int gsize;
#define MAX_BUFSIZE 5000000
#define NUM_REPEAT 20

int main(int argc, char** argv)
{
    MPI_Init(NULL, NULL);

    MPI_Comm_rank(MPI_COMM_WORLD, &grank);
    MPI_Comm_size(MPI_COMM_WORLD, &gsize);

    MPI_Comm comm = MPI_COMM_WORLD;

    void *buf;
    buf = malloc(MAX_BUFSIZE);
    if (!buf) {
        printf("! Failed to allocate buffer (size=%d)\n", MAX_BUFSIZE);
        return 1;
    }

    if (grank == 0) {
        printf("TEST barrier:\n");
    }
    for (int i = 0; i<10; i++) {
        bench_barrier(comm);
    }
    if (grank == 0) {
        printf("\n");
    }

    MPI_Finalize();
    return 0;
}

double bench_barrier(MPI_Comm comm)
{
    int iter;
    double tf_start;
    double tf_dur;
    double tf_latency;

    if (grank == 0) {
        iter = 2;
        double last_dur = 1.0;
        int num_best = 0;
        while (num_best < 20) {
            MPI_Bcast(&iter, 1, MPI_INT, 0, comm);
            tf_start = MPI_Wtime();
            for (int i = 0; i<iter; i++) {
                MPI_Barrier(comm);
            }
            tf_dur = MPI_Wtime() - tf_start;
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
        int tn_zero = 0;
        MPI_Bcast(&tn_zero, 1, MPI_INT, 0, comm);
    } else {
        while (1) {
            MPI_Bcast(&iter, 1, MPI_INT, 0, comm);
            if (iter == 0) {
                break;
            }
            tf_start = MPI_Wtime();
            for (int i = 0; i<iter; i++) {
                MPI_Barrier(comm);
            }
            tf_dur = MPI_Wtime() - tf_start;
        }
    }

    MPI_Bcast(&iter, 1, MPI_INT, 0, comm);

    double sum1 = 0;
    double sum2 = 0;
    for (int i = 0; i<NUM_REPEAT; i++) {
        tf_start = MPI_Wtime();
        for (int i2 = 0; i2<iter; i2++) {
            MPI_Barrier(comm);
        }
        tf_dur = MPI_Wtime() - tf_start;
        tf_latency = tf_dur / iter;
        sum1 += tf_latency;
        sum2 += tf_latency * tf_latency;
    }
    sum1 /= NUM_REPEAT;
    sum2 /= NUM_REPEAT;
    sum2 = sqrt(sum2 - sum1 * sum1);

    if (grank == 0) {
        printf("Barrier latency %.3f +/- %.3f us\n", sum1 * 1e6, sum2 * 1e6);
    }
    return sum1;
}
