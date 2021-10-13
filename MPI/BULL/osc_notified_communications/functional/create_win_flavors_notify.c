/*
 *  (C) 2001 by Argonne National Laboratory.
 *  (C) 2019 by Bull S. A. S.  All rights reserved.
 *
 *  This test checks if the MPI implementation properly allocates MPI
 *  windows that supports notified communications.
 */

#define BENCHMARK   "MPI Notifications Functionnal Test - Window allocation " \
                    "with valid metadata"
#define BUFSIZE     256

#include <stdio.h>
#include <stdlib.h>
#include <mpi.h>
#include <mpi-ext.h>
#include "util/common.h"
#include "util/common_output.h"

int main (int argc, char *argv[])
{
    int rank, nprocs;
    int errs = 0;
    int *flavor, *model, flag;
    MPI_Win win;
    /* Buffer associated to the MPI win */
    int *rbuf=NULL;

    set_header(HEADER);

    options.win = WIN_CREATE_NOTIFY;
    options.sync = NOTIFICATIONS;
    options.bench = FUNCTIONAL_OK;

    MPI_CHECK(MPI_Init(&argc, &argv));
    MPI_CHECK(MPI_Comm_size(MPI_COMM_WORLD, &nprocs));
    MPI_CHECK(MPI_Comm_rank(MPI_COMM_WORLD, &rank));

    print_header(rank, options.win, options.sync, options.bench);

    MPI_CHECK(MPIX_Win_allocate_notify(BUFSIZE * sizeof(int), 1, MPI_INFO_NULL,
                    MPI_COMM_WORLD, &rbuf, &win));

    if (NULL == rbuf) {
        errs++;
        fprintf(stderr, "%d: MPIX_Win_allocate_notify - Error, bad base pointer\n", rank);
    }

    MPI_CHECK(MPI_Win_get_attr(win, MPI_WIN_CREATE_FLAVOR, &flavor, &flag));

    if (0 == flag) {
        errs++;
        fprintf(stderr, "%d: MPIX_Win_allocate_notify - Error, no flavor\n", rank);
    }

    if (MPI_WIN_FLAVOR_ALLOCATE != *flavor) {
        errs++;
        fprintf(stderr, "%d: MPIX_Win_allocate_notify - Error, bad flavor (%d)\n", rank, *flavor);
    }

    MPI_CHECK(MPI_Win_get_attr(win, MPI_WIN_MODEL, &model, &flag));

    if (0 == flag) {
        errs++;
        fprintf(stderr, "%d: MPIX_Win_allocate_notify - Error, no model\n", rank);
    }

    if (MPI_WIN_SEPARATE != *model && MPI_WIN_UNIFIED != *model) {
        errs++;
        fprintf(stderr, "%d: MPIX_Win_allocate_notify - Error, bad model (%d)\n", rank, *model);
    }

    MPI_CHECK(MPI_Win_free(&win));

    print_result(rank, errs, EXPECTED_ERRORS);

    MPI_CHECK(MPI_Finalize());

    return EXIT_SUCCESS;
}
