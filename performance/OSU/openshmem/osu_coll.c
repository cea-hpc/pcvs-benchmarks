/*
 * Copyright (C) 2002-2016 the Network-Based Computing Laboratory
 * (NBCL), The Ohio State University. 
 *
 * Contact: Dr. D. K. Panda (panda@cse.ohio-state.edu)
 *
 * For detailed copyright and licensing information, please refer to the
 * copyright file COPYRIGHT in the top level directory.
 */

/*
 * HEADER FILES
 */
#include "osu_coll.h"


void 
print_header (const char *header, int rank, int full)
{
    if(rank == 0) {
        fprintf(stdout, header, "");

        if (print_size) {
            fprintf(stdout, "%-*s", 10, "# Size");
            fprintf(stdout, "%*s", FIELD_WIDTH, "Avg Latency(us)");
        }

        else {
            fprintf(stdout, "# Avg Latency(us)");
        }

        if (full) {
            fprintf(stdout, "%*s", FIELD_WIDTH, "Min Latency(us)");
            fprintf(stdout, "%*s", FIELD_WIDTH, "Max Latency(us)");
            fprintf(stdout, "%*s\n", 12, "Iterations");
        }

        else {
            fprintf(stdout, "\n");
        }

        fflush(stdout);
    }
}

void
print_data (int rank, int full, int size, double avg_time, double
        min_time, double max_time, int iterations)
{
    if(rank == 0) {
        if (print_size) {
            fprintf(stdout, "%-*d", 10, size);
            fprintf(stdout, "%*.*f", FIELD_WIDTH, FLOAT_PRECISION, avg_time);
        }

        else {
            fprintf(stdout, "%*.*f", 17, FLOAT_PRECISION, avg_time);
        }

        if (full) {
            fprintf(stdout, "%*.*f%*.*f%*d\n", 
                    FIELD_WIDTH, FLOAT_PRECISION, min_time,
                    FIELD_WIDTH, FLOAT_PRECISION, max_time,
                    12, iterations);
        }

        else {
            fprintf(stdout, "\n");
        }

        fflush(stdout);
    }
}
                                                           
                                                                                    
