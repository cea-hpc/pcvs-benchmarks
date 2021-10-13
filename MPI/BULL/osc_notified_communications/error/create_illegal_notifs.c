/*
 *  (C) 2001 by Argonne National Laboratory.
 *  (C) 2019 by Bull S. A. S.  All rights reserved.
 *
 *  This test checks if the range of notifications is properly
 *  handled by the MPI implementation, by checking if errors are
 *  raised when calling notification routines with notification IDs
 *  outside the legal range and at its boundaries.
 *  The legal range of MPI notifications routines is:
 *   - ]0:MPI_MAX_NOTIFICATIONS] for MPIX_Win_wait/test_notify;
 *   - [0:MPI_MAX_NOTIFICATIONS] for MPIX_Put_notify.
 */

#define BENCHMARK "MPI Notifications Robustness Test - Notifications Outside " \
                  "Legal Range"
#define BUFSIZE         100
#define EXPECTED_ERRORS 6

#include <mpi.h>
#include <mpi-ext.h>
#include <stdio.h>
#include "util/common.h"
#include "util/common_output.h"

MPI_Win win;

/* Send and receive buffers associated to MPI windows */
void *sbuf=NULL, *rbuf=NULL;

static void run_test(int rank, enum WINDOW type)
{
    int errs = 0, flag = -1;

    win_and_memory_allocate(rank, &sbuf, &rbuf, BUFSIZE * sizeof(int), type,
                &win);

    /* MPI errors should not abort the program in this test */
    MPI_CHECK(MPI_Win_set_errhandler(win, MPI_ERRORS_RETURN));

    MPI_CHECK(MPI_Win_lock_all(0, win));

    if (0 == rank) {
        int max_notif_idx = get_max_notification_id();

        DBG("put notif %d\n", max_notif_idx + 1);
        MPI_COUNT_ERROR(MPIX_Put_notify(sbuf, 1, MPI_INT, 1, 0, 1, MPI_INT, win,
                        max_notif_idx + 1), &errs);
        DBG("put notif -1\n");
        MPI_COUNT_ERROR(MPIX_Put_notify(sbuf, 1, MPI_INT, 1, 0, 1, MPI_INT, win,
                        -1), &errs);

        DBG("wait notif %d\n", max_notif_idx + 1);
        MPI_COUNT_ERROR(MPIX_Win_wait_notify(win, max_notif_idx + 1),
                    &errs);
        DBG("wait notif -1\n");
        MPI_COUNT_ERROR(MPIX_Win_wait_notify(win, -1), &errs);

        DBG("test notif %d\n", max_notif_idx + 1);
        MPI_COUNT_ERROR(MPIX_Win_test_notify(win, max_notif_idx + 1,
                        &flag), &errs);
        DBG("test notif -1\n");
        MPI_COUNT_ERROR(MPIX_Win_test_notify(win, -1, &flag), &errs);
    }

    MPI_CHECK(MPI_Win_unlock_all(win));

    win_and_memory_free(sbuf, rbuf, type, win);

    print_result(rank, errs, EXPECTED_ERRORS);
}

int main (int argc, char *argv[])
{
    int rank, nprocs;

    set_header(HEADER);

    options.win = WIN_ALLOCATE_NOTIFY;
    options.sync = NOTIFICATIONS;
    options.bench = FUNCTIONAL_XFAIL;

    MPI_CHECK(MPI_Init(&argc, &argv));
    MPI_CHECK(MPI_Comm_size(MPI_COMM_WORLD, &nprocs));
    MPI_CHECK(MPI_Comm_rank(MPI_COMM_WORLD, &rank));

    print_header(rank, options.win, options.sync, options.bench);

    run_test(rank, options.win);

    MPI_CHECK(MPI_Finalize());

    return EXIT_SUCCESS;
}

