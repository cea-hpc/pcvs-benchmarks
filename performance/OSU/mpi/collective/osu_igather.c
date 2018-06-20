#define BENCHMARK "OSU MPI%s Non-blocking Gather Latency Test"
/*
 * Copyright (C) 2002-2018 the Network-Based Computing Laboratory
 * (NBCL), The Ohio State University.
 *
 * Contact: Dr. D. K. Panda (panda@cse.ohio-state.edu)
 *
 * For detailed copyright and licensing information, please refer to the
 * copyright file COPYRIGHT in the top level OMB directory.
 */

#include <osu_util.h>

int main(int argc, char *argv[])
{
    int i = 0, rank, size;
    int numprocs;
    double latency = 0.0, t_start = 0.0, t_stop = 0.0;
    double tcomp = 0.0, tcomp_total = 0.0, latency_in_secs = 0.0;
    double timer = 0.0;
    double test_time = 0.0, test_total = 0.0;
    double wait_time = 0.0, init_time = 0.0;
    double init_total = 0.0, wait_total = 0.0;
    char *sendbuf = NULL;
    char *recvbuf = NULL;
    int po_ret;
    size_t bufsize;

    set_header(HEADER);
    set_benchmark_name("osu_igather");

    options.bench = COLLECTIVE;
    options.subtype = LAT;

    po_ret = process_options(argc, argv);

    if (PO_OKAY == po_ret && NONE != options.accel) {
        if (init_accel()) {
            fprintf(stderr, "Error initializing device\n");
            exit(EXIT_FAILURE);
        }
    }

    MPI_CHECK(MPI_Init(&argc, &argv));
    MPI_CHECK(MPI_Comm_rank(MPI_COMM_WORLD, &rank));
    MPI_CHECK(MPI_Comm_size(MPI_COMM_WORLD, &numprocs));
    MPI_Request request;
    MPI_Status status;

    switch (po_ret) {
        case PO_BAD_USAGE:
            print_bad_usage_message(rank);
            MPI_CHECK(MPI_Finalize());
            exit(EXIT_FAILURE);
        case PO_HELP_MESSAGE:
            print_help_message(rank);
            MPI_CHECK(MPI_Finalize());
            exit(EXIT_SUCCESS);
        case PO_VERSION_MESSAGE:
            print_version_message(rank);
            MPI_CHECK(MPI_Finalize());
            exit(EXIT_SUCCESS);
        case PO_OKAY:
            break;
    }

    if(numprocs < 2) {
        if (rank == 0) {
            fprintf(stderr, "This test requires at least two processes\n");
        }

        MPI_CHECK(MPI_Finalize());
        exit(EXIT_FAILURE);
    }

    if (options.max_message_size > options.max_mem_limit) {
        if (rank == 0) {
            fprintf(stderr, "Warning! Increase the Max Memory Limit to be able to run up to %ld bytes.\n"
                            "Continuing with max message size of %ld bytes\n", 
                            options.max_message_size, options.max_mem_limit);
        }
        options.max_message_size = options.max_mem_limit;
    }

    if (0 == rank) {
        bufsize = options.max_message_size * numprocs;
        if (allocate_memory_coll((void**)&recvbuf, bufsize, options.accel)) {
            fprintf(stderr, "Could Not Allocate Memory [rank %d]\n", rank);
            MPI_CHECK(MPI_Abort(MPI_COMM_WORLD, EXIT_FAILURE));
        }
        set_buffer(recvbuf, options.accel, 1, bufsize);
    }

    if (allocate_memory_coll((void**)&sendbuf, options.max_message_size,
                options.accel)) {
        fprintf(stderr, "Could Not Allocate Memory [rank %d]\n", rank);
        MPI_CHECK(MPI_Abort(MPI_COMM_WORLD, EXIT_FAILURE));
    }
    set_buffer(sendbuf, options.accel, 0, options.max_message_size);

    print_preamble_nbc(rank);

    for(size=options.min_message_size; size <= options.max_message_size; size *= 2) {
        if(size > LARGE_MESSAGE_SIZE) {
            options.skip = options.skip_large;
            options.iterations = options.iterations_large;
        }

        timer = 0.0;

        for(i=0; i < options.iterations + options.skip ; i++) {
            t_start = MPI_Wtime();
            MPI_CHECK(MPI_Igather(sendbuf, size, MPI_CHAR,
                        recvbuf, size, MPI_CHAR,
                        0, MPI_COMM_WORLD, &request));
            MPI_CHECK(MPI_Wait(&request,&status));

            t_stop = MPI_Wtime();

            if(i>=options.skip){
                timer += t_stop-t_start;
            }
            MPI_CHECK(MPI_Barrier(MPI_COMM_WORLD));
        }

        MPI_CHECK(MPI_Barrier(MPI_COMM_WORLD));

        latency = (timer * 1e6) / options.iterations;

        latency_in_secs = timer/options.iterations;

        init_arrays(latency_in_secs);

        MPI_CHECK(MPI_Barrier(MPI_COMM_WORLD));

        timer = 0.0; tcomp_total = 0; tcomp = 0;
        init_total = 0.0; wait_total = 0.0;
	    test_time = 0.0, test_total = 0.0;

	    /* for loop with dummy_compute */
        for(i=0; i < options.iterations + options.skip ; i++) {
            t_start = MPI_Wtime();

            init_time = MPI_Wtime();
            MPI_CHECK(MPI_Igather(sendbuf, size, MPI_CHAR,
                        recvbuf, size, MPI_CHAR,
                        0, MPI_COMM_WORLD, &request));
            init_time = MPI_Wtime() - init_time;

            tcomp = MPI_Wtime();
            test_time = dummy_compute(latency_in_secs, &request);
            tcomp = MPI_Wtime() - tcomp;

            wait_time = MPI_Wtime();
            MPI_CHECK(MPI_Wait(&request,&status));
            wait_time = MPI_Wtime() - wait_time;

            t_stop = MPI_Wtime();

            if(i>=options.skip){
                timer += t_stop-t_start;
                tcomp_total += tcomp;
                test_total += test_time;
		        init_total += init_time;
                wait_total += wait_time;
            }
            MPI_CHECK(MPI_Barrier(MPI_COMM_WORLD));
        }

        MPI_CHECK(MPI_Barrier(MPI_COMM_WORLD));

        calculate_and_print_stats(rank, size, numprocs,
                                  timer, latency,
                                  test_total, tcomp_total,
                                  wait_total, init_total);
    }

    if (0 == rank) {
        free_buffer(sendbuf, options.accel);
    }
    free_buffer(recvbuf, options.accel);
    MPI_CHECK(MPI_Finalize());

    if (NONE != options.accel) {
        if (cleanup_accel()) {
            fprintf(stderr, "Error cleaning up device\n");
            exit(EXIT_FAILURE);
        }
    }

    return EXIT_SUCCESS;
}

/* vi: set sw=4 sts=4 tw=80: */
