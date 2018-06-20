#define BENCHMARK "OSU MPI%s Gatherv Latency Test"
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
    int i, numprocs, rank, size, disp;
    double latency = 0.0, t_start = 0.0, t_stop = 0.0;
    double timer=0.0;
    double avg_time = 0.0, max_time = 0.0, min_time = 0.0;
    char *sendbuf, *recvbuf;
    int *rdispls, *recvcounts;
    int po_ret;
    size_t bufsize;
    options.bench = COLLECTIVE;
    options.subtype = LAT;

    set_header(HEADER);
    set_benchmark_name("osu_gatherv");
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

    if ((options.max_message_size * numprocs) > options.max_mem_limit) {
        options.max_message_size = options.max_mem_limit / numprocs;
    }

    if (0 == rank) {
        if (allocate_memory_coll((void**)&recvcounts, numprocs*sizeof(int), NONE)) {
            fprintf(stderr, "Could Not Allocate Memory [rank %d]\n", rank);
            MPI_CHECK(MPI_Abort(MPI_COMM_WORLD, EXIT_FAILURE));
        }
        if (allocate_memory_coll((void**)&rdispls, numprocs*sizeof(int), NONE)) {
            fprintf(stderr, "Could Not Allocate Memory [rank %d]\n", rank);
            MPI_CHECK(MPI_Abort(MPI_COMM_WORLD, EXIT_FAILURE));
        }

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

    print_preamble(rank);

    for(size=options.min_message_size; size <= options.max_message_size; size *= 2) {

        if(size > LARGE_MESSAGE_SIZE) {
            options.skip = options.skip_large;
            options.iterations = options.iterations_large;
        }

        MPI_CHECK(MPI_Barrier(MPI_COMM_WORLD));

        if (0 == rank) {
            disp =0;
            for ( i = 0; i < numprocs; i++) {
                recvcounts[i] = size;
                rdispls[i] = disp;
                disp += size;
            }
        }

        MPI_CHECK(MPI_Barrier(MPI_COMM_WORLD));
        timer=0.0;
        for(i=0; i < options.iterations + options.skip ; i++) {

            t_start = MPI_Wtime();

            MPI_CHECK(MPI_Gatherv(sendbuf, size, MPI_CHAR, recvbuf, recvcounts, rdispls, MPI_CHAR, 0, MPI_COMM_WORLD));

            t_stop = MPI_Wtime();

            if(i >= options.skip) {
                timer+= t_stop-t_start;
            }
            MPI_CHECK(MPI_Barrier(MPI_COMM_WORLD));

        }

        MPI_CHECK(MPI_Barrier(MPI_COMM_WORLD));

        latency = (double)(timer * 1e6) / options.iterations;

        MPI_CHECK(MPI_Reduce(&latency, &min_time, 1, MPI_DOUBLE, MPI_MIN, 0,
                MPI_COMM_WORLD));
        MPI_CHECK(MPI_Reduce(&latency, &max_time, 1, MPI_DOUBLE, MPI_MAX, 0,
                MPI_COMM_WORLD));
        MPI_CHECK(MPI_Reduce(&latency, &avg_time, 1, MPI_DOUBLE, MPI_SUM, 0,
                MPI_COMM_WORLD));
        avg_time = avg_time/numprocs;

        print_stats(rank, size, avg_time, min_time, max_time);
        MPI_CHECK(MPI_Barrier(MPI_COMM_WORLD));
    }

    if (0 == rank) {
        free_buffer(rdispls, NONE);
        free_buffer(recvcounts, NONE);
        free_buffer(recvbuf, options.accel);
    }
    free_buffer(sendbuf, options.accel);

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
