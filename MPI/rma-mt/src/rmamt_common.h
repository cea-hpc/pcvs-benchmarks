/* -*- Mode: C; c-basic-offset:4 ; -*- */
/*
 *   Copyright (C) 2007      University of Chicago
 *   Copyright (c) 2016-2018 Los Alamos National Security, LLC. All rights
 *                           reserved.
 * ****** SANDIA ADD YOUR COPYRIGHTS BEFORE RELEASE ******
 *   See COPYRIGHT notice in top-level directory.
 */

#if !defined(RMAMT_COMMON_H)
#define RMAMT_COMMON_H

#include "config.h"

#include <stdint.h>
#include <string.h>
#include <strings.h>
#include <inttypes.h>
#include <time.h>
#include <mpi.h>
#include <assert.h>

#include "rmamt_options.h"

#if RMAMT_ATOMICS_C11

#include <stdatomic.h>

#elif RMAMT_ATOMICS_SYNC

typedef volatile long atomic_long;

#define atomic_load(x) __atomic_load_n(x, __ATOMIC_RELAXED)
#define atomic_init(x, y) *(x) = (y)
#define atomic_fetch_add(x, y) __atomic_fetch_add (x, y, __ATOMIC_RELAXED)
#define ATOMIC_VAR_INIT(x) (x)

#elif RMAMT_ATOMICS_BUILTIN

typedef volatile long atomic_long;

#define atomic_load(x) __sync_fetch_and_add(x, 0)
#define atomic_init(x, y) *(x) = (y)
#define atomic_fetch_add(x, y) __sync_fetch_and_add(x, y)
#define ATOMIC_VAR_INIT(x) (x)

#endif

#define RMAMT_WARMUP_ITERATIONS 32

typedef struct arg_struct {
    MPI_Win win;
    MPI_Comm comm;
    int tid;
    size_t max_size;
    size_t min_size;
    MPI_Group group;
    int target;
    bool do_sync;
    bool do_init;
    bool all_sync;
} ArgStruct;

typedef void *(*rmamt_fn_t) (ArgStruct *);

static atomic_long current_value;
static long barrier_value;
static volatile _Atomic long current_cycle = 0;

static void thread_barrier_init (int value)
{
    barrier_value = value + 1;
    atomic_init (&current_value, barrier_value);
}

static void thread_barrier (int cycle)
{
    long tmp;

    /* wait for the expected cycle */
    while (cycle > current_cycle);

    /* decrement the counter */
    tmp = atomic_fetch_add (&current_value, -1);
    if (2 == tmp) {
        /* this thread was the last one. reset the counter and increment the cycle */
        atomic_init (&current_value, barrier_value);

        /* only one thread will modify the cycle at any time so atomics are not needed*/
        ++current_cycle;
    } else {
        /* wait for cycle increment */
        while (cycle == current_cycle);
    }
}

#define max(a,b) (((a) > (b)) ? (a) : (b))

#include <time.h>

static inline uint64_t time_getns (void)
{
  struct timespec ts;

  clock_gettime (CLOCK_MONOTONIC, &ts);

  return ts.tv_nsec + ts.tv_sec * 1000000000;
}

#define MPI_CHECK(stmt)                                          \
do {                                                             \
   int mpi_errno = (stmt);                                       \
   if (MPI_SUCCESS != mpi_errno) {                               \
       fprintf(stderr, "[%s:%d] MPI call failed with %d \n",     \
        __FILE__, __LINE__,mpi_errno);                           \
       exit(EXIT_FAILURE);                                       \
   }                                                             \
   assert(MPI_SUCCESS == mpi_errno);                             \
} while (0)

int rmamt_bind_init (void);
void rmamt_bind (int thread_id);
void rmamt_bind_finalize (void);

void *rmamt_malloc (size_t size);
void rmamt_free (void *ptr, size_t size);

#endif /* RMAMT_COMMON_H */
