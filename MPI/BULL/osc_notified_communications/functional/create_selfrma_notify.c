/*
 *  (C) 2001 by Argonne National Laboratory.
 *  (C) 2019 by Bull S. A. S.  All rights reserved.
 *
 *  This test checks if the MPI implementation properly handles a Put Notify on
 *  itself and returns without error.
 */

#define BENCHMARK   "MPI Notifications Functionnal Test - Put Notify on " \
                    "itself"
#define BUFSIZE     256

#include <stdio.h>
#include <stdlib.h>
#include <mpi.h>
#include <mpi-ext.h>
#include "util/common.h"
#include "util/common_output.h"

MPI_Win win;

/* Send and receive buffers associated to MPI windows */
int *sbuf=NULL, *rbuf=NULL;

static void run_test(int rank, enum WINDOW type)
{
    int errs = 0, i;

    win_and_memory_allocate(rank, (void **)&sbuf, (void **)&rbuf,
                BUFSIZE * sizeof(int), type, &win);

    /* Initialize buffers */
    for (i = 0; i < BUFSIZE; i++) {
      sbuf[i] = 1 + rank;
    }

    memset(rbuf, 0, BUFSIZE * sizeof(int));

    MPI_CHECK(MPI_Win_lock_all(0, win));

    MPI_CHECK(MPIX_Put_notify(sbuf, BUFSIZE, MPI_INT, rank, 0, BUFSIZE, MPI_INT,
                    win, NOTIF_ID));
    MPI_CHECK(MPIX_Win_wait_notify(win, NOTIF_ID));

    /* Check that the correct data was returned */
    for (i = 0; i < BUFSIZE; i++) {
        if (rbuf[i] != sbuf[i]) {
            errs++;
#if (DEBUG == 1)
            fprintf(stderr, "rbuf[%d] = %d, expected %d\n", i, rbuf[i],
                        sbuf[i]);
#endif // DEBUG
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

    run_test(rank, options.win);

    MPI_CHECK(MPI_Finalize());

    return EXIT_SUCCESS;
}
