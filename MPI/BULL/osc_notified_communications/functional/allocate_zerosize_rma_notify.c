/*
 *  (C) 2001 by Argonne National Laboratory.
 *  (C) 2019 by Bull S. A. S.  All rights reserved.
 *
 *  This test checks several combinations of zero-size RMA calls
 *  and/or RMA calls with a NULL send buffer. All these calls must
 *  not raise an error by the MPI implementation.
 */

#define BENCHMARK   "MPI Notifications Functionnal Test - Zero-size and " \
                    "NULL RMA calls"
#define BUFSIZE     100

#include <mpi.h>
#include <mpi-ext.h>
#include <stdio.h>
#include "util/common.h"
#include "util/common_output.h"

MPI_Win win;

/* Send and receive buffers associated to MPI windows */
void *sbuf=NULL, *rbuf=NULL;

/* Datatypes used in this test */
enum types {
    ZERO_INT = 0,
    ONE_INT,
    INT,
    DOUBLE,
    N_TYPES
};

static void do_test(int origin_count, MPI_Datatype origin_type, int
            target_count, MPI_Datatype target_type)
{
    int origin_type_size;

    MPI_CHECK(MPIX_Put_notify(sbuf, origin_count, origin_type, 1, 0,
                    target_count, target_type, win, NOTIF_ID));

    MPI_CHECK(MPI_Type_size(origin_type, &origin_type_size));

    if (0 == origin_count || 0 == origin_type_size) {
        MPI_CHECK(MPIX_Put_notify(NULL, origin_count, origin_type, 1, 0,
                        target_count, target_type, win, NOTIF_ID));
    }
}

static void run_test(int rank, enum WINDOW type)
{
    int i, j;
    MPI_Datatype types[N_TYPES];

    /* types[ZERO_INT] is of zero size.  Everything else is non-zero size */
    MPI_CHECK(MPI_Type_contiguous(0, MPI_INT, &types[ZERO_INT]));
    MPI_CHECK(MPI_Type_commit(&types[ZERO_INT]));

    MPI_CHECK(MPI_Type_contiguous(1, MPI_INT, &types[ONE_INT]));
    MPI_CHECK(MPI_Type_commit(&types[ONE_INT]));

    types[INT] = MPI_INT;
    types[DOUBLE] = MPI_DOUBLE;

    win_and_memory_allocate(rank, &sbuf, &rbuf, BUFSIZE * sizeof(int), type,
                &win);

    MPI_CHECK(MPI_Win_lock_all(0, win));

    if (0 == rank) {
        /* zero-count */
        for (i = ZERO_INT; i < N_TYPES; i++) {
            for (j = ZERO_INT; j < N_TYPES; j++) {
                do_test(0, types[i], 0, types[j]);
            }
        }

        /* single zero-size datatype, but non-zero count */
        for (i = ONE_INT; i < N_TYPES; i++) {
            do_test(1, types[ZERO_INT], 0, types[i]);
            do_test(0, types[i], 1, types[ZERO_INT]);
        }

        /* two zero-size datatypes, but non-zero count */
        do_test(1, types[ZERO_INT], 1, types[ZERO_INT]);
        do_test(1, types[ZERO_INT], 2, types[ZERO_INT]);
        do_test(2, types[ZERO_INT], 1, types[ZERO_INT]);
        do_test(2, types[ZERO_INT], 2, types[ZERO_INT]);
    }

    MPI_CHECK(MPI_Win_unlock_all(win));

    win_and_memory_free(sbuf, rbuf, type, win);

    MPI_CHECK(MPI_Type_free(&types[ZERO_INT]));
    MPI_CHECK(MPI_Type_free(&types[ONE_INT]));

    print_result(rank, 0, EXPECTED_ERRORS);
}

int main(int argc, char *argv[])
{
    int rank, nprocs;

    set_header(HEADER);

    options.win = WIN_ALLOCATE_NOTIFY;
    options.sync = NOTIFICATIONS;
    options.bench = FUNCTIONAL_OK;

    MPI_CHECK(MPI_Init(&argc, &argv));
    MPI_CHECK(MPI_Comm_size(MPI_COMM_WORLD, &nprocs));
    MPI_CHECK(MPI_Comm_rank(MPI_COMM_WORLD, &rank));

    if(2 > nprocs) {
        if(0 == rank) {
            fprintf(stderr, "This test requires two or more processes\n");
        }
        MPI_CHECK(MPI_Finalize());
        return EXIT_FAILURE;
    }

    print_header(rank, options.win, options.sync, options.bench);

    run_test(rank, options.win);

    MPI_CHECK(MPI_Finalize());

    return EXIT_SUCCESS;
}
