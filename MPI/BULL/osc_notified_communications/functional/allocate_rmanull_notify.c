/*
 *  (C) 2001 by Argonne National Laboratory.
 *  (C) 2019 by Bull S. A. S.  All rights reserved.
 *
 *  This test checks if the MPI implementation properly handles a Put Notify on
 *  MPI_PROC_NULL and returns without error.
 */

#define BENCHMARK   "MPI Notifications Functionnal Test - Put Notify on " \
                    "MPI_PROC_NULL"
#define BUFSIZE     256

#include <stdio.h>
#include <stdlib.h>
#include <mpi.h>
#include <mpi-ext.h>
#include "util/common.h"
#include "util/common_output.h"

MPI_Win win;
/* Send and receive buffers associated to MPI windows */
void *sbuf=NULL, *rbuf=NULL;

static void run_test(int rank, enum WINDOW type)
{
    size_t size_rma_window = BUFSIZE * sizeof(int);

    DBG("Win allocate\n");
    win_and_memory_allocate(rank, &sbuf, &rbuf, size_rma_window, type, &win);

    DBG("Win lockall\n");
    MPI_CHECK(MPI_Win_lock_all(0, win));

    DBG("Put notif on MPI_PROC_NULL\n");
    MPI_CHECK(MPIX_Put_notify(sbuf, BUFSIZE, MPI_INT, MPI_PROC_NULL, 0, BUFSIZE,
                    MPI_INT, win, NOTIF_ID));

    DBG("Win unlockall\n");
    MPI_CHECK(MPI_Win_unlock_all(win));

    win_and_memory_free(sbuf, rbuf, type, win);

    print_result(rank, 0, EXPECTED_ERRORS);
}

int main (int argc, char *argv[])
{
    int rank, nprocs;

    set_header(HEADER);

    options.win = WIN_ALLOCATE_NOTIFY;
    options.sync = LOCK_ALL;
    options.bench = FUNCTIONAL_OK;

    MPI_CHECK(MPI_Init(&argc, &argv));
    MPI_CHECK(MPI_Comm_size(MPI_COMM_WORLD, &nprocs));
    MPI_CHECK(MPI_Comm_rank(MPI_COMM_WORLD, &rank));

    print_header(rank, options.win, options.sync, options.bench);

    run_test(rank, options.win);

    MPI_CHECK(MPI_Finalize());

    return EXIT_SUCCESS;
}
