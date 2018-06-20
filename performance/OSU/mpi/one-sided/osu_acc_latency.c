#define BENCHMARK "OSU MPI_Accumulate%s latency Test"
/*
 * Copyright (C) 2003-2018 the Network-Based Computing Laboratory
 * (NBCL), The Ohio State University.
 *
 * Contact: Dr. D. K. Panda (panda@cse.ohio-state.edu)
 *
 * For detailed copyright and licensing information, please refer to the
 * copyright file COPYRIGHT in the top level OMB directory.
 */

#include <osu_util.h>

double  t_start = 0.0, t_end = 0.0;
char    sbuf_original[ONESBUFSIZE];
char    rbuf_original[ONESBUFSIZE];
char    *sbuf=NULL, *rbuf=NULL;

void print_latency (int, int);
void run_acc_with_lock (int, enum WINDOW);
void run_acc_with_fence (int, enum WINDOW);
void run_acc_with_lock_all (int, enum WINDOW);
void run_acc_with_flush (int, enum WINDOW);
void run_acc_with_flush_local (int, enum WINDOW);
void run_acc_with_pscw (int, enum WINDOW);

int main (int argc, char *argv[])
{
    int         po_ret = PO_OKAY;
#if MPI_VERSION >= 3
    options.win = WIN_ALLOCATE;
    options.sync= FLUSH;
#else
    options.win = WIN_CREATE;
    options.sync = LOCK;
#endif
    int         rank,nprocs;

    options.bench = ONE_SIDED;
    options.subtype = LAT;
    options.synctype = ALL_SYNC;

    set_header(HEADER);
    set_benchmark_name("osu_acc_latency");

    po_ret = process_options(argc, argv);

    if (PO_OKAY == po_ret && NONE != options.accel) {
        if (init_accel()) {
            fprintf(stderr, "Error initializing device\n");
            exit(EXIT_FAILURE);
        }
    }

    MPI_CHECK(MPI_Init(&argc, &argv));
    MPI_CHECK(MPI_Comm_size(MPI_COMM_WORLD, &nprocs));
    MPI_CHECK(MPI_Comm_rank(MPI_COMM_WORLD, &rank));

    if (0 == rank) {
        switch (po_ret) {
            case PO_CUDA_NOT_AVAIL:
                fprintf(stderr, "CUDA support not enabled.  Please recompile "
                        "benchmark with CUDA support.\n");
                break;
            case PO_OPENACC_NOT_AVAIL:
                fprintf(stderr, "OPENACC support not enabled.  Please "
                        "recompile benchmark with OPENACC support.\n");
                break;
            case PO_BAD_USAGE:
                print_bad_usage_message(rank);
                break;
            case PO_HELP_MESSAGE:
                usage_one_sided("osu_acc_latency");
                break;
            case PO_VERSION_MESSAGE:
                print_version_message(rank);
                MPI_CHECK(MPI_Finalize());
                exit(EXIT_SUCCESS);
            case PO_OKAY:
                break;
        }
    }

    switch (po_ret) {
        case PO_CUDA_NOT_AVAIL:
        case PO_OPENACC_NOT_AVAIL:
        case PO_BAD_USAGE:
            MPI_CHECK(MPI_Finalize());
            exit(EXIT_FAILURE);
        case PO_HELP_MESSAGE:
        case PO_VERSION_MESSAGE:
            MPI_CHECK(MPI_Finalize());
            exit(EXIT_SUCCESS);
        case PO_OKAY:
            break;
    }

    if(nprocs != 2) {
        if(rank == 0) {
            fprintf(stderr, "This test requires exactly two processes\n");
        }

        MPI_CHECK(MPI_Finalize());

        return EXIT_FAILURE;
    }

    print_header_one_sided(rank, options.win, options.sync);

    switch (options.sync){
        case LOCK:
            run_acc_with_lock(rank, options.win);
            break;
        case PSCW:
            run_acc_with_pscw(rank, options.win);
            break;
        case FENCE:
            run_acc_with_fence(rank, options.win);
            break;
#if MPI_VERSION >= 3
        case LOCK_ALL:
            run_acc_with_lock_all(rank, options.win);
            break;
        case FLUSH_LOCAL:
            run_acc_with_flush_local(rank, options.win);
            break;
        default:
            run_acc_with_flush(rank, options.win);
            break;
#endif
    }

    MPI_CHECK(MPI_Finalize());

    if (NONE != options.accel) {
        if (cleanup_accel()) {
            fprintf(stderr, "Error cleaning up device\n");
            exit(EXIT_FAILURE);
        }
    }

    return EXIT_SUCCESS;
}

void print_latency(int rank, int size)
{
    if (rank == 0) {
        fprintf(stdout, "%-*d%*.*f\n", 10, size, FIELD_WIDTH,
                FLOAT_PRECISION, (t_end - t_start) * 1.0e6 / options.iterations);
        fflush(stdout);
    }
}

#if MPI_VERSION >= 3
/*Run ACC with flush */
void run_acc_with_flush (int rank, enum WINDOW type)
{
    int size, i;
    MPI_Aint disp = 0;
    MPI_Win     win;

    for (size = options.min_message_size; size <= options.max_message_size; size = (size ? size * 2 : 1)) {
        allocate_memory_one_sided(rank, sbuf_original, rbuf_original, &sbuf, &rbuf, &sbuf, size, type, &win);

        if (type == WIN_DYNAMIC) {
            disp = disp_remote;
        }
        if(size > LARGE_MESSAGE_SIZE) {
            options.iterations = options.iterations_large;
            options.skip = options.skip_large;
        }

        if(rank == 0) {
            MPI_CHECK(MPI_Win_lock(MPI_LOCK_SHARED, 1, 0, win));
            for (i = 0; i < options.skip + options.iterations; i++) {
                if (i == options.skip) {
                    t_start = MPI_Wtime ();
                }
                MPI_CHECK(MPI_Accumulate(sbuf, size, MPI_CHAR, 1, disp, size, MPI_CHAR, MPI_SUM, win));
                MPI_CHECK(MPI_Win_flush(1, win));
            }
            t_end = MPI_Wtime ();
            MPI_CHECK(MPI_Win_unlock(1, win));
        }

        MPI_CHECK(MPI_Barrier(MPI_COMM_WORLD));

        print_latency(rank, size);

        free_memory_one_sided (sbuf, rbuf, win, rank);
    }
}

/*Run ACC with flush local*/
void run_acc_with_flush_local (int rank, enum WINDOW type)
{
    int size, i;
    MPI_Aint disp = 0;
    MPI_Win     win;

    for (size = options.min_message_size; size <= options.max_message_size; size = (size ? size * 2 : 1)) {
        allocate_memory_one_sided(rank, sbuf_original, rbuf_original, &sbuf, &rbuf, &sbuf, size, type, &win);

        if (type == WIN_DYNAMIC) {
            disp = disp_remote;
        }
        if(size > LARGE_MESSAGE_SIZE) {
            options.iterations = options.iterations_large;
            options.skip = options.skip_large;
        }

        if(rank == 0) {
            MPI_CHECK(MPI_Win_lock(MPI_LOCK_SHARED, 1, 0, win));
            for (i = 0; i < options.skip + options.iterations; i++) {
                if (i == options.skip) {
                    t_start = MPI_Wtime ();
                }
                MPI_CHECK(MPI_Accumulate(sbuf, size, MPI_CHAR, 1, disp, size, MPI_CHAR, MPI_SUM, win));
                MPI_CHECK(MPI_Win_flush_local(1, win));
            }
            t_end = MPI_Wtime ();
            MPI_CHECK(MPI_Win_unlock(1, win));
        }

        MPI_CHECK(MPI_Barrier(MPI_COMM_WORLD));

        print_latency(rank, size);

        free_memory_one_sided (sbuf, rbuf, win, rank);
    }
}

/*Run ACC with Lock_all/unlock_all */
void run_acc_with_lock_all (int rank, enum WINDOW type)
{
    int size, i;
    MPI_Aint disp = 0;
    MPI_Win     win;

    for (size = options.min_message_size; size <= options.max_message_size; size = (size ? size * 2 : 1)) {
        allocate_memory_one_sided(rank, sbuf_original, rbuf_original, &sbuf, &rbuf, &sbuf, size, type, &win);

        if (type == WIN_DYNAMIC) {
            disp = disp_remote;
        }

        if(size > LARGE_MESSAGE_SIZE) {
            options.iterations = options.iterations_large;
            options.skip = options.skip_large;
        }

        if(rank == 0) {
            for (i = 0; i < options.skip + options.iterations; i++) {
                if (i == options.skip) {
                    t_start = MPI_Wtime ();
                }
                MPI_CHECK(MPI_Win_lock_all(0, win));
                MPI_CHECK(MPI_Accumulate(sbuf, size, MPI_CHAR, 1, disp, size, MPI_CHAR, MPI_SUM, win));
                MPI_CHECK(MPI_Win_unlock_all(win));
            }
            t_end = MPI_Wtime ();
        }

        MPI_CHECK(MPI_Barrier(MPI_COMM_WORLD));

        print_latency(rank, size);

        free_memory_one_sided (sbuf, rbuf, win, rank);
    }
}
#endif

/*Run ACC with Lock/unlock */
void run_acc_with_lock(int rank, enum WINDOW type)
{
    int size, i;
    MPI_Aint disp = 0;
    MPI_Win     win;

    for (size = options.min_message_size; size <= options.max_message_size; size = (size ? size * 2 : 1)) {
        allocate_memory_one_sided(rank, sbuf_original, rbuf_original, &sbuf, &rbuf, &sbuf, size, type, &win);

#if MPI_VERSION >= 3
        if (type == WIN_DYNAMIC) {
            disp = disp_remote;
        }
#endif
        if(size > LARGE_MESSAGE_SIZE) {
            options.iterations = options.iterations_large;
            options.skip = options.iterations_large;
        }

        if(rank == 0) {
            for (i = 0; i < options.skip + options.iterations; i++) {
                if (i == options.skip) {
                    t_start = MPI_Wtime ();
                }
                MPI_CHECK(MPI_Win_lock(MPI_LOCK_SHARED, 1, 0, win));
                MPI_CHECK(MPI_Accumulate(sbuf, size, MPI_CHAR, 1, disp, size, MPI_CHAR, MPI_SUM, win));
                MPI_CHECK(MPI_Win_unlock(1, win));
            }
            t_end = MPI_Wtime ();
        }

        MPI_CHECK(MPI_Barrier(MPI_COMM_WORLD));

        print_latency(rank, size);

        free_memory_one_sided (sbuf, rbuf, win, rank);
    }
}

/*Run ACC with Fence */
void run_acc_with_fence(int rank, enum WINDOW type)
{
    int size, i;
    MPI_Aint disp = 0;
    MPI_Win     win;


    for (size = options.min_message_size; size <= options.max_message_size; size = (size ? size * 2 : 1)) {
        allocate_memory_one_sided(rank, sbuf_original, rbuf_original, &sbuf, &rbuf, &sbuf, size, type, &win);

#if MPI_VERSION >= 3
        if (type == WIN_DYNAMIC) {
            disp = disp_remote;
        }
#endif

        if(size > LARGE_MESSAGE_SIZE) {
            options.iterations = options.iterations_large;
            options.skip = options.skip_large;
        }

        MPI_CHECK(MPI_Barrier(MPI_COMM_WORLD));

        if(rank == 0) {
            for (i = 0; i < options.skip + options.iterations; i++) {
                if (i == options.skip) {
                    t_start = MPI_Wtime ();
                }
                MPI_CHECK(MPI_Win_fence(0, win));
                MPI_CHECK(MPI_Accumulate(sbuf, size, MPI_CHAR, 1, disp, size, MPI_CHAR, MPI_SUM, win));
                MPI_CHECK(MPI_Win_fence(0, win));
                MPI_CHECK(MPI_Win_fence(0, win));
            }
            t_end = MPI_Wtime ();
        } else {
            for (i = 0; i < options.skip + options.iterations; i++) {
                MPI_CHECK(MPI_Win_fence(0, win));
                MPI_CHECK(MPI_Win_fence(0, win));
                MPI_CHECK(MPI_Accumulate(sbuf, size, MPI_CHAR, 0, disp, size, MPI_CHAR, MPI_SUM, win));
                MPI_CHECK(MPI_Win_fence(0, win));
            }
        }

        MPI_CHECK(MPI_Barrier(MPI_COMM_WORLD));

        if (rank == 0) {
            fprintf(stdout, "%-*d%*.*f\n", 10, size, FIELD_WIDTH,
                    FLOAT_PRECISION, (t_end - t_start) * 1.0e6 / options.iterations / 2);
            fflush(stdout);
        }

        free_memory_one_sided (sbuf, rbuf, win, rank);
    }
}

/*Run ACC with Post/Start/Complete/Wait */
void run_acc_with_pscw(int rank, enum WINDOW type)
{
    int destrank, size, i;
    MPI_Aint disp = 0;
    MPI_Win     win;
    MPI_Group   comm_group, group;

    MPI_CHECK(MPI_Comm_group(MPI_COMM_WORLD, &comm_group));

    for (size = options.min_message_size; size <= options.max_message_size; size = (size ? size * 2 : 1)) {
        allocate_memory_one_sided(rank, sbuf_original, rbuf_original, &sbuf, &rbuf, &sbuf, size, type, &win);

#if MPI_VERSION >= 3
        if (type == WIN_DYNAMIC) {
            disp = disp_remote;
        }
#endif

        if (size > LARGE_MESSAGE_SIZE) {
            options.iterations = options.iterations_large;
            options.skip = options.skip_large;
        }

        if (rank == 0) {
            destrank = 1;

            MPI_CHECK(MPI_Group_incl(comm_group, 1, &destrank, &group));
            MPI_CHECK(MPI_Barrier(MPI_COMM_WORLD));

            for (i = 0; i < options.skip + options.iterations; i++) {
                MPI_CHECK(MPI_Win_start (group, 0, win));
                if (i == options.skip) {
                    t_start = MPI_Wtime ();
                }
                MPI_CHECK(MPI_Accumulate(sbuf, size, MPI_CHAR, 1, disp, size, MPI_CHAR, MPI_SUM, win));
                MPI_CHECK(MPI_Win_complete(win));
                MPI_CHECK(MPI_Win_post(group, 0, win));
                MPI_CHECK(MPI_Win_wait(win));
            }

            t_end = MPI_Wtime ();
        } else {
            /* rank=1 */
            destrank = 0;

            MPI_CHECK(MPI_Group_incl(comm_group, 1, &destrank, &group));
            MPI_CHECK(MPI_Barrier(MPI_COMM_WORLD));

            for (i = 0; i < options.skip + options.iterations; i++) {
                MPI_CHECK(MPI_Win_post(group, 0, win));
                MPI_CHECK(MPI_Win_wait(win));
                MPI_CHECK(MPI_Win_start(group, 0, win));
                MPI_CHECK(MPI_Accumulate(sbuf, size, MPI_CHAR, 0, disp, size, MPI_CHAR, MPI_SUM, win));
                MPI_CHECK(MPI_Win_complete(win));
            }
        }

        MPI_CHECK(MPI_Barrier(MPI_COMM_WORLD));

        if (rank == 0) {
            fprintf(stdout, "%-*d%*.*f\n", 10, size, FIELD_WIDTH,
                    FLOAT_PRECISION, (t_end - t_start) * 1.0e6 / options.iterations / 2);
            fflush(stdout);
        }

        MPI_CHECK(MPI_Group_free(&group));

        free_memory_one_sided (sbuf, rbuf, win, rank);
    }
    MPI_CHECK(MPI_Group_free(&comm_group));
}
/* vi: set sw=4 sts=4 tw=80: */
