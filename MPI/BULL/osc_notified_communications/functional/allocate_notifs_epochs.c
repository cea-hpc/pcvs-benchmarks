/*
 *  (C) 2001 by Argonne National Laboratory.
 *  (C) 2019 by Bull S. A. S.  All rights reserved.
 *
 * This test checks if MPI notification routines can be used safely with all the
 * existing synchronization schemes available in MPI-3 standard, i.e. Fence/PSCW
 * for Active Target mode, and Lock/Unlock(_all) for Passive Target mode.
 * WARNING: With notifications on top of MPI, this test should fail, as the
 * Put_notify call requires a call to MPI_Win_flush, which can happen only
 * inside a Passive epoch.
 */

#define BENCHMARK   "MPI Notifications Functionnal Test - RMA Notified " \
                    "Communications with Active and Passive Epochs"
#define BUFSIZE     1000
/* Size and displacement of vector datatype used for this test */
#define VSIZE       BUFSIZE / 4
#define VDISP       2

#include <stdio.h>
#include <stdlib.h>
#include <mpi.h>
#include <mpi-ext.h>
#include "util/common.h"
#include "util/common_output.h"

/* Type of notification synchronization used */
enum NOTIF_SYNC {
    WAIT=0,
    TEST
};

MPI_Win win;
MPI_Datatype vectype;

/* Send and receive buffers associated to MPI windows */
int *sbuf=NULL, *rbuf=NULL;

/* Check if the communication really happened */
static int check_data(enum SYNC synctype)
{
    int i, j = 0, errs = 0;
    int recorded_prints = 0;

    for (i = 0; i < VSIZE; i++) {
        if (rbuf[j] != sbuf[j]) {
            errs++;
            if (MAX_LOG > recorded_prints) {
                fprintf(stderr, "VecPut_notify w/ sync type %s: rbuf[%d] = %d, "
                            "should = %d\n", sync_info[synctype], j, rbuf[j],
                            sbuf[j]);
                recorded_prints++;
            }
        }
        j += VDISP;
    }

    return errs;
}

/* Check communication arrival with notifications */
static void notif_sync(enum SYNC synctype, enum NOTIF_SYNC type)
{
    int flag = 0;
#if (DEBUG == 1)
    /* This comment is useful to detect the currently running test if the
     * notification check livelocks. */
    fprintf(stderr, "Sync test sync_type %d notif_type %d\n", synctype, type);
#else
    (void) synctype;
#endif // DEBUG

    if (WAIT == type) {
        MPI_CHECK(MPIX_Win_wait_notify(win, NOTIF_ID));
    } else {    /* type == TEST */
        while (0 == flag) {
            MPI_CHECK(MPIX_Win_test_notify(win, NOTIF_ID, &flag));
        }
    }
}

static int do_test_lock(int peer, enum NOTIF_SYNC type)
{
    /* Initialize receive buffer */
    memset(rbuf, 0, BUFSIZE * sizeof(int));

    MPI_CHECK(MPI_Win_lock(MPI_LOCK_EXCLUSIVE, peer, 0, win));
    MPI_CHECK(MPIX_Put_notify(sbuf, 1, vectype, peer, 0, 1, vectype, win,
                    NOTIF_ID));
    notif_sync(LOCK, type);
    MPI_CHECK(MPI_Win_unlock(peer, win));

    return check_data(LOCK);
}

static int do_test_fence(int peer, enum NOTIF_SYNC type)
{
    /* Initialize receive buffer */
    memset(rbuf, 0, BUFSIZE * sizeof(int));

    MPI_CHECK(MPI_Win_fence(0, win));
    MPI_CHECK(MPIX_Put_notify(sbuf, 1, vectype, peer, 0, 1, vectype, win,
                    NOTIF_ID));
    notif_sync(FENCE, type);
    MPI_CHECK(MPI_Win_fence(0, win));

    return check_data(FENCE);
}

static int do_test_pscw(int peer, enum NOTIF_SYNC type)
{
    MPI_Group all_group;

    /* Initialize receive buffer */
    memset(rbuf, 0, BUFSIZE * sizeof(int));

    MPI_CHECK(MPI_Comm_group(MPI_COMM_WORLD, &all_group));
    MPI_CHECK(MPI_Win_post(all_group, 0, win));
    MPI_CHECK(MPI_Win_start(all_group, 0, win));

    MPI_CHECK(MPIX_Put_notify(sbuf, 1, vectype, peer, 0, 1, vectype, win,
                    NOTIF_ID));
    notif_sync(PSCW, type);

    MPI_CHECK(MPI_Win_complete(win));
    MPI_CHECK(MPI_Win_wait(win));

    return check_data(PSCW);
}

#if MPI_3
/* This routine perform the actual communication */
static int do_test_lockall(int peer, enum NOTIF_SYNC type)
{

    /* Initialize receive buffer */
    memset(rbuf, 0, BUFSIZE * sizeof(int));

    MPI_CHECK(MPI_Win_lock_all(0, win));
    MPI_CHECK(MPIX_Put_notify(sbuf, 1, vectype, peer, 0, 1, vectype, win,
                    NOTIF_ID));
    notif_sync(LOCK_ALL, type);
    MPI_CHECK(MPI_Win_unlock_all(win));

    return check_data(LOCK_ALL);
}
#endif // MPI_3

static void run_test(int rank, enum WINDOW type)
{
    int errs = 0, i;
    int peer = (rank % 2) ? rank -1 : rank +1;
    enum NOTIF_SYNC notif_type;

    MPI_CHECK(MPI_Type_vector(VSIZE, 1, VDISP, MPI_INT, &vectype));
    MPI_CHECK(MPI_Type_commit(&vectype));

    win_and_memory_allocate(rank, (void **)&sbuf, (void **)&rbuf,
                BUFSIZE * sizeof(int), type, &win);

    /* Initialize send buffer */
    for (i = 0; i < BUFSIZE; i++) {
        sbuf[i] = i;
    }

    /* Test for all synchronization types for notifications */
    for (notif_type = WAIT; notif_type <= TEST; notif_type++) {
        errs += do_test_lock(peer, notif_type);
        errs += do_test_pscw(peer, notif_type);
        errs += do_test_fence(peer, notif_type);
#if MPI_3
        errs += do_test_lockall(peer, notif_type);
#endif // MPI_3
    }

    win_and_memory_free(sbuf, rbuf, type, win);

    print_result(rank, errs, EXPECTED_ERRORS);
}

int main (int argc, char *argv[])
{
    int rank, nprocs;

    set_header(HEADER);

    options.win = WIN_ALLOCATE_NOTIFY;
    options.bench = FUNCTIONAL_OK;
    options.sync = MULTIPLE;

    MPI_CHECK(MPI_Init(&argc, &argv));
    MPI_CHECK(MPI_Comm_size(MPI_COMM_WORLD, &nprocs));
    MPI_CHECK(MPI_Comm_rank(MPI_COMM_WORLD, &rank));

    if(nprocs % 2) {
        if(0 == rank) {
            fprintf(stderr, "This test requires an even processes number\n");
        }
        MPI_CHECK(MPI_Finalize());
        return EXIT_FAILURE;
    }

    print_header(rank, options.win, options.sync, options.bench);

    run_test(rank, options.win);

    MPI_CHECK(MPI_Finalize());

    return EXIT_SUCCESS;
}
