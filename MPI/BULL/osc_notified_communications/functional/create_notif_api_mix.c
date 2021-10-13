/*
 *  (C) 2001 by Argonne National Laboratory.
 *  (C) 2019 by Bull S. A. S.  All rights reserved.
 *
 * This test checks if RMA MPI routines can be used safely when the MPI
 * window was created with MPIX_Win_create_notify.
 */

#define BENCHMARK   "MPI Notifications Functionnal Test - Mix RMA and " \
                    "Notifications API"
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

MPI_Win win;

/* Send and receive buffers associated to MPI windows */
int *sbuf=NULL, *rbuf=NULL;

static void run_test(int rank, enum WINDOW type)
{
    int errs = 0, flag = -1, i, j;
    int peer = (rank % 2) ? rank -1 : rank +1;
    MPI_Datatype vectype;

    MPI_CHECK(MPI_Type_vector(VSIZE, 1, VDISP, MPI_INT, &vectype));
    MPI_CHECK(MPI_Type_commit(&vectype));

    win_and_memory_allocate(rank, (void **)&sbuf, (void **)&rbuf,
                BUFSIZE * sizeof(int), type, &win);

    /* Initialize send buffer */
    for (i = 0; i < BUFSIZE; i++) {
        sbuf[i] = i;
    }

    /* Initialize receive buffer */
    memset(rbuf, 0, BUFSIZE * sizeof(int));

    MPI_CHECK(MPI_Win_lock_all(0, win));
    /* Test a regular Put on a notifed window */
    MPI_CHECK(MPI_Put(sbuf, 1, vectype, peer, 0, 1, vectype, win));
    MPI_CHECK(MPI_Win_unlock_all(win));
    MPI_CHECK(MPI_Win_fence(0, win));

    /* Check if the communication really happened */
    j = 0;
    for (i = 0; i < VSIZE; i++) {
        if (rbuf[j] != sbuf[j]) {
            errs++;
#if (DEBUG == 1)
            fprintf(stderr, "VecPut_notify: rbuf[%d] = %d, should = %d\n", j,
                        rbuf[j], sbuf[j]);
#endif
        }
        j += VDISP;
    }

    MPI_CHECK(MPI_Win_free(&win));

    free(sbuf);
    free(rbuf);

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
