/*
 *  (C) 2001 by Argonne National Laboratory.
 *  (C) 2019 by Bull S. A. S.  All rights reserved.
 *
 *  This test checks if the MPI implementation can send notifications through an
 *  MPI window of zero size.
 */

#define BENCHMARK   "MPI Notifications Functionnal Test - Put Notify on Window " \
                    "of zero size"

#include <mpi.h>
#include <mpi-ext.h>
#include "util/common.h"
#include "util/common_output.h"

int main (int argc, char *argv[])
{
    int rank, nprocs, next, next_id, prev_id;
    MPI_Win win;
    /* Send and receive buffers associated to MPI windows */
    int *sbuf=NULL, *rbuf=NULL;

    set_header(HEADER);

    options.win = WIN_CREATE_NOTIFY;
    options.sync = NOTIFICATIONS;
    options.bench = FUNCTIONAL_OK;

    MPI_CHECK(MPI_Init(&argc, &argv));
    MPI_CHECK(MPI_Comm_size(MPI_COMM_WORLD, &nprocs));
    MPI_CHECK(MPI_Comm_rank(MPI_COMM_WORLD, &rank));

    print_header(rank, options.win, options.sync, options.bench);

    /* All ranks allocate zero bytes */
    win_and_memory_allocate(rank, (void **)&sbuf, (void **)&rbuf,
                0, options.win, &win);

    /* All ranks exchange a single notification in a ring fashion.
     * Notification IDs */
    next = (rank + 1) % nprocs;
    next_id = 1 + rank;
    prev_id = 1 + (rank + nprocs - 1) % nprocs;

    MPI_CHECK(MPI_Win_lock_all(0, win));
    MPI_CHECK(MPIX_Put_notify(sbuf, 0, MPI_INT, next, 0, 0, MPI_INT, win,
                    next_id));
    MPI_CHECK(MPIX_Win_wait_notify(win, prev_id));
    MPI_CHECK(MPI_Win_unlock_all(win));

    MPI_CHECK(MPI_Win_free(&win));

    print_result(rank, 0, EXPECTED_ERRORS);

    MPI_CHECK(MPI_Finalize());

    return EXIT_SUCCESS;
}
