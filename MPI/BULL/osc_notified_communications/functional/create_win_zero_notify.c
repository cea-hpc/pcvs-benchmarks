/*
 *  (C) 2001 by Argonne National Laboratory.
 *  (C) 2019 by Bull S. A. S.  All rights reserved.
 *
 *  This test checks if the MPI implementation can allocate MPI windows when all
 *  or some of them have a size of zero.
 */

#define BENCHMARK   "MPI Notifications Functionnal Test - Window allocation " \
                    "with zero size"

#include <mpi.h>
#include <mpi-ext.h>
#include "util/common.h"
#include "util/common_output.h"

int main (int argc, char *argv[])
{
    int rank, size;
    MPI_Win win;
    /* Buffer associated to the MPI win */
    int *rbuf=NULL;

    set_header(HEADER);

    options.win = WIN_CREATE_NOTIFY;
    options.sync = NOTIFICATIONS;
    options.bench = FUNCTIONAL_OK;

    MPI_CHECK(MPI_Init(&argc, &argv));
    MPI_CHECK(MPI_Comm_rank(MPI_COMM_WORLD, &rank));

    print_header(rank, options.win, options.sync, options.bench);

    /* All ranks allocate zero bytes */
    MPI_CHECK(MPIX_Win_allocate_notify(0, 1, MPI_INFO_NULL, MPI_COMM_WORLD,
                    &rbuf, &win));
    MPI_CHECK(MPI_Win_free(&win));

    /* Some ranks allocate zero bytes with the same window object */
    size = rank % 2;
    MPI_CHECK(MPIX_Win_allocate_notify(size, 1, MPI_INFO_NULL, MPI_COMM_WORLD,
                    &rbuf, &win));
    MPI_CHECK(MPI_Win_free(&win));

    print_result(rank, 0, EXPECTED_ERRORS);

    MPI_CHECK(MPI_Finalize());

    return EXIT_SUCCESS;
}
