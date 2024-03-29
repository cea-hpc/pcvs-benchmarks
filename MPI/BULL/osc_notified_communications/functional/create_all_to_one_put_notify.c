/*
 *  (C) 2001 by Argonne National Laboratory.
 *  (C) 2019 by Bull S. A. S.  All rights reserved.
 *
 * All-to-one RMA Put_notify test -- Marc Sergent <marc.sergent@atos.net>
 *
 * Each process issues BUFSIZE Put_notify operations to non-overlapping
 * locations on process ROOT_RANK.
 */

#define BENCHMARK   "MPI Notifications Functionnal Test - All-to-one RMA " \
                    "Put_Notify"
#define BUFSIZE     16
#define ROOT_RANK   0

#include <stdio.h>
#include <stdlib.h>
#include <mpi.h>
#include <mpi-ext.h>
#include "util/common.h"
#include "util/common_output.h"

MPI_Win win;

/* Send and receive buffers associated to MPI windows */
double *sbuf=NULL, *rbuf=NULL;

static void run_test(int rank, int nprocs, enum WINDOW type)
{
    int errs = 0, proc, i;
    size_t offset;

    win_and_memory_allocate(rank, (void **)&sbuf, (void **)&rbuf,
                nprocs * BUFSIZE * sizeof(double), type, &win);

    /* Initialize send buffer */
    for (i = 0; i < BUFSIZE; i++) {
        sbuf[i] = rank + 1.0;
    }

    /* Initialize reception buffer */
    if (ROOT_RANK == rank) {
        for (i = 0; i < nprocs * BUFSIZE; i++) {
            rbuf[i] = 0.0;
        }
    }

    MPI_CHECK(MPI_Win_lock_all(0, win));

    for (i = 0; i < BUFSIZE; i++) {
#if (DEBUG == 1)
        fprintf(stderr, "%2d -> %2d [%2d]\n", rank, ROOT_RANK, i);
#endif
        offset = rank * BUFSIZE + i;
        MPI_CHECK(MPIX_Put_notify(&sbuf[i], 1, MPI_DOUBLE, ROOT_RANK,
                        offset * sizeof(double), 1,
                        MPI_DOUBLE, win, offset + 1));
    }

    /* Check that the correct data was returned.  This assumes that the systems
     * have the same data representations */
    if (ROOT_RANK == rank) {
        for (proc = 0; proc < nprocs; proc++) {
            for (i = 0; i < BUFSIZE; i++) {
                offset = proc * BUFSIZE + i;
                MPI_CHECK(MPIX_Win_wait_notify(win, offset + 1));
                if (rbuf[offset] != 1.0 + proc) {
                    errs++;
#if (DEBUG == 1)
                    fprintf(stderr, "rbuf[%d] = %e, expected %e\n",
                                proc * BUFSIZE + i, rbuf[proc * BUFSIZE + i],
                                1.0 + proc);
#endif
                }
            }
        }
    }

    MPI_CHECK(MPI_Win_unlock_all(win));

    win_and_memory_free(sbuf, rbuf, type, win);

    print_result(rank, errs, EXPECTED_ERRORS);
}

int main (int argc, char *argv[])
{
    int rank, nprocs;

    set_header(HEADER);

    options.win = WIN_CREATE_NOTIFY;
    options.sync = NOTIFICATIONS;
    options.bench = FUNCTIONAL_OK;

    MPI_CHECK(MPI_Init(&argc, &argv));
    MPI_CHECK(MPI_Comm_size(MPI_COMM_WORLD, &nprocs));
    MPI_CHECK(MPI_Comm_rank(MPI_COMM_WORLD, &rank));

    print_header(rank, options.win, options.sync, options.bench);

    run_test(rank, nprocs, options.win);

    MPI_CHECK(MPI_Finalize());

    return EXIT_SUCCESS;
}
