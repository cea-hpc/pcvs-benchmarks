#define BENCHMARK "OSU MPI Multiple Bandwidth / Message Rate Test"
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

#ifdef PACKAGE_VERSION
#   define HEADER "# " BENCHMARK " v" PACKAGE_VERSION "\n"
#else
#   define HEADER "# " BENCHMARK "\n"
#endif

MPI_Request * mbw_request;
MPI_Status * mbw_reqstat;

double calc_bw(int rank, int size, int num_pairs, int window_size, char *s_buf, char *r_buf);

static int loop_override;
static int skip_override;

int main(int argc, char *argv[])
{
    char *s_buf, *r_buf;
    unsigned long align_size = sysconf(_SC_PAGESIZE);
    int numprocs, rank;
    int c, curr_size;

    loop_override = 0;
    skip_override = 0;

    options.bench = MBW_MR;
    options.subtype = BW;
    
    MPI_CHECK(MPI_Init(&argc, &argv));

    MPI_CHECK(MPI_Comm_size(MPI_COMM_WORLD, &numprocs));
    MPI_CHECK(MPI_Comm_rank(MPI_COMM_WORLD, &rank));

    options.pairs            = numprocs / 2;

    int po_ret = process_options(argc, argv);

    if (PO_OKAY == po_ret && NONE != options.accel) {
        if (init_accel()) {
            fprintf(stderr, "Error initializing device\n");
            exit(EXIT_FAILURE);
        }
    }

    if(options.pairs > (numprocs / 2)) {
        po_ret = PO_BAD_USAGE;
    }

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
                usage_mbw_mr();
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

    if(numprocs < 2) {
        if(rank == 0) {
            fprintf(stderr, "This test requires at least two processes\n");
        }

        MPI_CHECK(MPI_Finalize());

        return EXIT_FAILURE;
    }

    if(rank == 0) {
        fprintf(stdout, HEADER);

        if(options.window_varied) {
            fprintf(stdout, "# [ pairs: %d ] [ window size: varied ]\n", options.pairs);
            fprintf(stdout, "\n# Uni-directional Bandwidth (MB/sec)\n");
        }

        else {
            fprintf(stdout, "# [ pairs: %d ] [ window size: %d ]\n", options.pairs,
                    options.window_size);

            if(options.print_rate) {
                fprintf(stdout, "%-*s%*s%*s\n", 10, "# Size", FIELD_WIDTH,
                        "MB/s", FIELD_WIDTH, "Messages/s");
            }

            else {
                fprintf(stdout, "%-*s%*s\n", 10, "# Size", FIELD_WIDTH, "MB/s");
            }
        }

        fflush(stdout);
    }

   /* More than one window size */

   if(options.window_varied) {
       int window_array[] = WINDOW_SIZES;
       double ** bandwidth_results;
       int log_val = 1, tmp_message_size = options.max_message_size;
       int i, j;

       for(i = 0; i < WINDOW_SIZES_COUNT; i++) {
           if(window_array[i] > options.window_size) {
               options.window_size = window_array[i];
           }
       }

       mbw_request = (MPI_Request *) malloc(sizeof(MPI_Request) * options.window_size);
       mbw_reqstat = (MPI_Status *) malloc(sizeof(MPI_Status) * options.window_size);

       while(tmp_message_size >>= 1) {
           log_val++;
       }

       bandwidth_results = (double **) malloc(sizeof(double *) * log_val);

       for(i = 0; i < log_val; i++) {
           bandwidth_results[i] = (double *)malloc(sizeof(double) *
                   WINDOW_SIZES_COUNT);
       }

       if(rank == 0) {
           fprintf(stdout, "#      ");

           for(i = 0; i < WINDOW_SIZES_COUNT; i++) {
               fprintf(stdout, "  %10d", window_array[i]);
           }

           fprintf(stdout, "\n");
           fflush(stdout);
       }
    
       for(j = 0, curr_size = options.min_message_size; curr_size <= options.max_message_size; curr_size *= 2, j++) {
           if(rank == 0) {
               fprintf(stdout, "%-7d", curr_size);
           }

           for(i = 0; i < WINDOW_SIZES_COUNT; i++) {
               bandwidth_results[j][i] = calc_bw(rank, curr_size, options.pairs,
                       window_array[i], s_buf, r_buf);

               if(rank == 0) {
                   fprintf(stdout, "  %10.*f", FLOAT_PRECISION,
                           bandwidth_results[j][i]);
               }
           }

           if(rank == 0) {
               fprintf(stdout, "\n");
               fflush(stdout);
           }
       }

       if(rank == 0 && options.print_rate) {
            fprintf(stdout, "\n# Message Rate Profile\n");
            fprintf(stdout, "#      ");

            for(i = 0; i < WINDOW_SIZES_COUNT; i++) {
                fprintf(stdout, "  %10d", window_array[i]);
            }       

            fprintf(stdout, "\n");
            fflush(stdout);

            for(c = 0, curr_size = options.min_message_size; curr_size <= options.max_message_size; curr_size *= 2) {
                fprintf(stdout, "%-7d", curr_size); 

                for(i = 0; i < WINDOW_SIZES_COUNT; i++) {
                    double rate = 1e6 * bandwidth_results[c][i] / curr_size;

                    fprintf(stdout, "  %10.2f", rate);
                }       

                fprintf(stdout, "\n");
                fflush(stdout);
                c++;    
            }
       }
   }

   else {
       /* Just one window size */
       mbw_request = (MPI_Request *)malloc(sizeof(MPI_Request) * options.window_size);
       mbw_reqstat = (MPI_Status *)malloc(sizeof(MPI_Status) * options.window_size);

       for(curr_size = options.min_message_size; curr_size <= options.max_message_size; curr_size *= 2) {
           double bw, rate;

           bw = calc_bw(rank, curr_size, options.pairs, options.window_size, s_buf, r_buf);

           if(rank == 0) {
               rate = 1e6 * bw / curr_size;

               if(options.print_rate) {
                   fprintf(stdout, "%-*d%*.*f%*.*f\n", 10, curr_size,
                           FIELD_WIDTH, FLOAT_PRECISION, bw, FIELD_WIDTH,
                           FLOAT_PRECISION, rate);
               }

               else {
                   fprintf(stdout, "%-*d%*.*f\n", 10, curr_size, FIELD_WIDTH,
                           FLOAT_PRECISION, bw);
               }
           } 
       }
   }

   free(r_buf);
   free(s_buf);

   MPI_CHECK(MPI_Finalize());

   return EXIT_SUCCESS;
}

double calc_bw(int rank, int size, int num_pairs, int window_size, char *s_buf,
        char *r_buf)
{
    double t_start = 0, t_end = 0, t = 0, sum_time = 0, bw = 0;
    int i, j, target;

    for(i = 0; i < size; i++) {
        s_buf[i] = 'a';
        r_buf[i] = 'b';
    }

    MPI_CHECK(MPI_Barrier(MPI_COMM_WORLD));

    if(rank < num_pairs) {
        target = rank + num_pairs;

        for(i = 0; i <  options.iterations +  options.skip; i++) {
            if(i ==  options.skip) {
                MPI_CHECK(MPI_Barrier(MPI_COMM_WORLD));
                t_start = MPI_Wtime();
            }

            for(j = 0; j < window_size; j++) {
                MPI_CHECK(MPI_Isend(s_buf, size, MPI_CHAR, target, 100, MPI_COMM_WORLD,
                        mbw_request + j));
            }
            MPI_CHECK(MPI_Waitall(window_size, mbw_request, mbw_reqstat));
            MPI_CHECK(MPI_Recv(r_buf, 4, MPI_CHAR, target, 101, MPI_COMM_WORLD,
                    &mbw_reqstat[0]));
        }

        t_end = MPI_Wtime();
        t = t_end - t_start;
    }

    else if(rank < num_pairs * 2) {
        target = rank - num_pairs;

        for(i = 0; i <  options.iterations +  options.skip; i++) {
            if(i ==  options.skip) {
                MPI_CHECK(MPI_Barrier(MPI_COMM_WORLD));
            }

            for(j = 0; j < window_size; j++) {
                MPI_CHECK(MPI_Irecv(r_buf, size, MPI_CHAR, target, 100, MPI_COMM_WORLD,
                        mbw_request + j));
            }

            MPI_CHECK(MPI_Waitall(window_size, mbw_request, mbw_reqstat));
            MPI_CHECK(MPI_Send(s_buf, 4, MPI_CHAR, target, 101, MPI_COMM_WORLD));
        }
    }

    else {
        MPI_CHECK(MPI_Barrier(MPI_COMM_WORLD));
    }

    MPI_CHECK(MPI_Reduce(&t, &sum_time, 1, MPI_DOUBLE, MPI_SUM, 0, MPI_COMM_WORLD));

    if(rank == 0) {
        double tmp = size / 1e6 * num_pairs ;
        
        sum_time /= num_pairs;
        tmp = tmp *  options.iterations * window_size;
        bw = tmp / sum_time;

        return bw;
    }

    return 0;
}

/* vi: set sw=4 sts=4 tw=80: */
