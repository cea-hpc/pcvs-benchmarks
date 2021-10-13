/*
 * Copyright (C) 2002-2018 the Network-Based Computing Laboratory
 * (NBCL), The Ohio State University.
 *
 * Copyright (C) 2019 Bull S. A. S. All rights reserved.
 * Bull, Rue Jean Jaures, B.P.68, 78340, Les Clayes-sous-Bois, France
 * This is not Free or Open Source software.
 * Please contact Bull S. A. S. for details about its license.
 */

#ifndef __COMMON_OUTPUT_H__
#define __COMMON_OUTPUT_H__

#define HEADER "# " BENCHMARK "\n"
#define FIELD_WIDTH     20
#define FLOAT_PRECISION 2
#define MAX_LOG         10

#if DEBUG
#define DBG(...) do{fprintf(stderr, "DEBUG: " __VA_ARGS__);fflush(stderr);} while(0)
#else
#define DBG(...)
#endif

#include "common.h"
#include <assert.h>

/*
 * Error check macros
 */
#define MEM_CHECK(stmt)                                                     \
    do {                                                                    \
        int errno = (stmt);                                                 \
        if (0 != errno) {                                                   \
            fprintf(stderr, "[%s:%d] function call failed with %d \n",      \
                        __FILE__, __LINE__, errno);                         \
            exit(EXIT_FAILURE);                                             \
        }                                                                   \
        assert(0 == errno);                                                 \
    } while (0)

#define MPI_CHECK(stmt)                                                     \
    do {                                                                    \
        int mpi_errno = (stmt);                                             \
        if (MPI_SUCCESS != mpi_errno) {                                     \
            fprintf(stderr, "[%s:%d] MPI call failed with %d \n",           \
                        __FILE__, __LINE__,mpi_errno);                      \
            exit(EXIT_FAILURE);                                             \
        }                                                                   \
        assert(MPI_SUCCESS == mpi_errno);                                   \
    } while (0)

#define MPI_COUNT_ERROR(stmt, error_count)                                  \
    do {                                                                    \
        int mpi_errno = (stmt);                                             \
        if (MPI_SUCCESS != mpi_errno) {                                     \
            (*((int *)error_count))++;                                      \
        }                                                                   \
    } while (0)

/*
 * One-sided benchmarks printings
 */
void print_header (int rank, enum WINDOW win, enum SYNC
            sync, enum benchmark_type bench);
void print_result (int rank, int errs, int expected_errs);

#endif // __COMMON_OUTPUT_H__
