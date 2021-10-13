/*
 * Copyright (C) 2002-2018 the Network-Based Computing Laboratory
 * (NBCL), The Ohio State University.
 *
 * Copyright (C) 2019 Bull S. A. S. All rights reserved.
 * Bull, Rue Jean Jaures, B.P.68, 78340, Les Clayes-sous-Bois, France
 * This is not Free or Open Source software.
 * Please contact Bull S. A. S. for details about its license.
 */

#include "common_output.h"

extern char const * win_info[N_WIN];
extern char const * sync_info[N_SYNC];
extern char const * bench_info[N_BENCH];
extern char const * benchmark_header;

/*
 * Benchmarks printings
 */

void print_header(int rank, enum WINDOW win, enum SYNC sync,
            enum benchmark_type bench)
{
    if(rank == 0) {
        printf(benchmark_header);
        fprintf(stdout, "# Window creation: %s\n", win_info[win]);
        fprintf(stdout, "# Synchronization: %s\n", sync_info[sync]);
        fprintf(stdout, "# Expected result: %s\n", bench_info[bench]);
        fflush(stdout);
    }
}

void print_result(int rank, int errs, int expected_errs)
{
    if (0 == rank) {
        if (0 == expected_errs && 0 == errs) {
            fprintf(stdout,"OK\n");
        } else if (0 < expected_errs && errs == expected_errs) {
            fprintf(stdout,"XFAIL\n");
        } else {
            fprintf(stdout,"Error: %d / %d found\n", errs, expected_errs);
            fprintf(stdout,"FAIL\n");
            fflush(stdout);
            exit(1);
        }
        fflush(stdout);
    }
}

