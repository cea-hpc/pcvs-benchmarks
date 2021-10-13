/*
 *  (C) 2020 by Bull S. A. S.  All rights reserved.
 *
 *  This test checks if the MPI implementation handles all notification ids
 *  with Put Notify and returns without error.
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

#define LARGE_PRIME 9999999967 /* must be bigger than max notification index */

/* Send and receive buffers associated to MPI windows */
int *sbuf=NULL, *rbuf=NULL;

static void run_test(int rank, enum WINDOW type)
{
    int errs = 0, id=0, count=0;
    int max_notif_idx = get_max_notification_id();
    int peer = (rank + 1) % 2;

    win_and_memory_allocate(rank, (void **)&sbuf, (void **)&rbuf,
                BUFSIZE * sizeof(int), type, &win);


    MPI_CHECK(MPI_Win_lock_all(0, win));

    int flush_freq=10000000;

    do {
        if (0 == rank) {
            DBG("Send notif %d (%d)\n", id, count);
            MPI_CHECK(MPIX_Put_notify(sbuf,0 , MPI_INT, peer, 0, 0, MPI_INT, win, id));
            if (0 == ((count+1) % flush_freq)){
                    MPI_CHECK(MPI_Win_flush(peer,win));
                    DBG("Flush\n");
            }
        } else if (1 == rank) {
            DBG("Wait notif %d (%d)\n", id, count);
            MPI_CHECK(MPIX_Win_wait_notify(win, id));
        }

        id = (id + LARGE_PRIME) % max_notif_idx;
        count ++;
    } while(id != 0 );

    MPI_CHECK(MPI_Win_unlock_all(win));
    win_and_memory_free(sbuf, rbuf, type, win);

    if (count != max_notif_idx){
        errs ++;
    }

    print_result(rank, errs, 0);
}

int main (int argc, char *argv[])
{
    int rank, nprocs;

    set_header(HEADER);

    options.win = WIN_ALLOCATE_NOTIFY;
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
