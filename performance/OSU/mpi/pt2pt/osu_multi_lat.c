#define BENCHMARK "OSU MPI Multi Latency Test"
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

char *s_buf, *r_buf;

static void multi_latency(int rank, int pairs);

int main(int argc, char* argv[])
{
    unsigned long align_size = sysconf(_SC_PAGESIZE);
    int rank, nprocs; 
    int pairs;
    int po_ret = 0;
    options.bench = PT2PT;
    options.subtype = LAT;

    set_header(HEADER);
    set_benchmark_name("osu_multi_lat");

    po_ret = process_options(argc, argv);

    if (PO_OKAY == po_ret && NONE != options.accel) {
        if (init_accel()) {
            fprintf(stderr, "Error initializing device\n");
            exit(EXIT_FAILURE);
        }
    }
    MPI_CHECK(MPI_Init(&argc, &argv));

    MPI_CHECK(MPI_Comm_rank(MPI_COMM_WORLD, &rank));
    MPI_CHECK(MPI_Comm_size(MPI_COMM_WORLD, &nprocs));

    pairs = nprocs/2;

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
                print_help_message(rank);
                break;
            case PO_VERSION_MESSAGE:
                print_version_message(rank);
                MPI_CHECK(MPI_Finalize());
                break;
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

    if (posix_memalign((void**)&s_buf, align_size, options.max_message_size)) {
        fprintf(stderr, "Error allocating host memory\n");
        return EXIT_FAILURE;
    }

    if (posix_memalign((void**)&r_buf, align_size, options.max_message_size)) {
        fprintf(stderr, "Error allocating host memory\n");
        return EXIT_FAILURE;
    }

    memset(s_buf, 0, options.max_message_size);
    memset(r_buf, 0, options.max_message_size);

    if(rank == 0) {
        fprintf(stdout, HEADER);
        fprintf(stdout, "%-*s%*s\n", 10, "# Size", FIELD_WIDTH, "Latency (us)");
        fflush(stdout);
    }

    MPI_CHECK(MPI_Barrier(MPI_COMM_WORLD));

    multi_latency(rank, pairs);
    
    MPI_CHECK(MPI_Barrier(MPI_COMM_WORLD));

    MPI_CHECK(MPI_Finalize());

    free(r_buf);
    free(s_buf);

    return EXIT_SUCCESS;
}

static void multi_latency(int rank, int pairs)
{
    int size, partner;
    int i;
    double t_start = 0.0, t_end = 0.0,
           latency = 0.0, total_lat = 0.0,
           avg_lat = 0.0;

    MPI_Status reqstat;


    for(size = options.min_message_size; size <= options.max_message_size; size  = (size ? size * 2 : 1)) {

        MPI_CHECK(MPI_Barrier(MPI_COMM_WORLD));

        if(size > LARGE_MESSAGE_SIZE) {
            options.iterations = options.iterations_large;
            options.skip = options.skip_large;
        } else {
            options.iterations = options.iterations;
            options.skip = options.skip;
        }

        if (rank < pairs) {
            partner = rank + pairs;

            for (i = 0; i < options.iterations + options.skip; i++) {

                if (i == options.skip) {
                    t_start = MPI_Wtime();
                    MPI_CHECK(MPI_Barrier(MPI_COMM_WORLD));
                }

                MPI_CHECK(MPI_Send(s_buf, size, MPI_CHAR, partner, 1, MPI_COMM_WORLD));
                MPI_CHECK(MPI_Recv(r_buf, size, MPI_CHAR, partner, 1, MPI_COMM_WORLD,
                         &reqstat));
            }

            t_end = MPI_Wtime();

        } else {
            partner = rank - pairs;

            for (i = 0; i < options.iterations + options.skip; i++) {

                if (i == options.skip) {
                    t_start = MPI_Wtime();
                    MPI_CHECK(MPI_Barrier(MPI_COMM_WORLD));
                }

                MPI_CHECK(MPI_Recv(r_buf, size, MPI_CHAR, partner, 1, MPI_COMM_WORLD,
                         &reqstat));
                MPI_CHECK(MPI_Send(s_buf, size, MPI_CHAR, partner, 1, MPI_COMM_WORLD));
            }

            t_end = MPI_Wtime();
        }

        latency = (t_end - t_start) * 1.0e6 / (2.0 * options.iterations);

        MPI_CHECK(MPI_Reduce(&latency, &total_lat, 1, MPI_DOUBLE, MPI_SUM, 0,
                   MPI_COMM_WORLD));

        avg_lat = total_lat/(double) (pairs * 2);

        if(0 == rank) {
            fprintf(stdout, "%-*d%*.*f\n", 10, size, FIELD_WIDTH,
                    FLOAT_PRECISION, avg_lat);
            fflush(stdout);
        }
    }
}

/* vi: set sw=4 sts=4 tw=80: */
