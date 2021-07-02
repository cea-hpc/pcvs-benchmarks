/* -*- Mode: C; c-basic-offset:4 ; -*- */
/*
 *   Copyright (C) 2007      University of Chicago
 *   Copyright (c) 2016-2018 Los Alamos National Security, LLC. All rights
 *                           reserved.
 * ****** SANDIA ADD YOUR COPYRIGHTS BEFORE RELEASE ******
 *   See COPYRIGHT notice in top-level directory.
 */

#include "rmamt_common.h"

#include <stdio.h>
#include <pthread.h>
#include <stdlib.h>
#include <strings.h>
#include <stdint.h>
#include <inttypes.h>
#include <unistd.h>
#include <sys/mman.h>
#include <fcntl.h>
#include <sys/syscall.h>
#include <sys/types.h>

uint64_t find_max();

/* origin-side window per thread functions */
static void *bibw_put_lock_all_new (ArgStruct* a);
static void *bibw_get_lock_all_new (ArgStruct* a);
static void *bibw_put_fence_new (ArgStruct* a);
static void *bibw_get_fence_new (ArgStruct* a);
static void *bibw_put_lock_per_rank_new (ArgStruct* a);
static void *bibw_get_lock_per_rank_new (ArgStruct* a);
static void *bibw_put_flush_new (ArgStruct* a);
static void *bibw_get_flush_new (ArgStruct* a);
static void *bibw_put_pscw_new (ArgStruct* a);
static void *bibw_get_pscw_new (ArgStruct* a);
static void *bibw_put_flush_thread_new (ArgStruct* a);
static void *bibw_get_flush_thread_new (ArgStruct* a);
static void *bibw_put_flush_all_new (ArgStruct* a);
static void *bibw_get_flush_all_new (ArgStruct* a);
static void *bibw_put_flush_local_new (ArgStruct* a);
static void *bibw_get_flush_local_new (ArgStruct* a);
static void *bibw_put_flush_local_all_new (ArgStruct* a);
static void *bibw_get_flush_local_all_new (ArgStruct* a);

static rmamt_fn_t rmamt_new_fns[RMAMT_OPERATIONS_MAX][RMAMT_SYNC_MAX] = {
    [RMAMT_PUT] = {
        [RMAMT_LOCK_ALL] = bibw_put_lock_all_new,
        [RMAMT_FENCE] = bibw_put_fence_new,
        [RMAMT_LOCK] = bibw_put_lock_per_rank_new,
        [RMAMT_FLUSH] = bibw_put_flush_new,
        [RMAMT_PSCW] = bibw_put_pscw_new,
        [RMAMT_ALL_FLUSH] = bibw_put_flush_new,
        [RMAMT_FLUSH_ALL] = bibw_put_flush_all_new,
        [RMAMT_FLUSH_LOCAL] = bibw_put_flush_local_new,
        [RMAMT_FLUSH_LOCAL_ALL] = bibw_put_flush_local_all_new,
    },
    [RMAMT_GET] = {
        [RMAMT_LOCK_ALL] = bibw_get_lock_all_new,
        [RMAMT_FENCE] = bibw_get_fence_new,
        [RMAMT_LOCK] = bibw_get_lock_per_rank_new,
        [RMAMT_FLUSH] = bibw_get_flush_new,
        [RMAMT_PSCW] = bibw_get_pscw_new,
        [RMAMT_ALL_FLUSH] = bibw_get_flush_new,
        [RMAMT_FLUSH_ALL] = bibw_get_flush_all_new,
        [RMAMT_FLUSH_LOCAL] = bibw_get_flush_local_new,
        [RMAMT_FLUSH_LOCAL_ALL] = bibw_get_flush_local_all_new,
    },
};

static ArgStruct args[MAX_THREADS];
static uint64_t thread_etimes[MAX_THREADS];
static char* tbufs[MAX_THREADS];
static char* obuf;
static MPI_Win win[MAX_THREADS];
static int64_t times[MAX_THREADS][256];
static uint64_t barrier_time;

int main(int argc,char *argv[])
{
    MPI_Comm test_comm, leader_comm;
    MPI_Group group = MPI_GROUP_NULL, comm_group;
    int nprocs, provided, rank, rc;
    pthread_t id[MAX_THREADS];
    MPI_Request req;
    int64_t win_size;
    int win_count, pairs;
    size_t max_size, min_size;
    FILE *output_file = NULL;

    MPI_Init_thread(&argc, &argv, MPI_THREAD_MULTIPLE, &provided);
    if (provided != MPI_THREAD_MULTIPLE) {
        printf("MPI Thread multiple support required to run this benchmark\n");
        MPI_Abort(MPI_COMM_WORLD, 1);
    }

    MPI_T_init_thread (MPI_THREAD_MULTIPLE, &provided);

    MPI_Comm_size(MPI_COMM_WORLD, &nprocs);
    if (nprocs & 1) {
        printf("Run with a multiple of two MPI processes\n");
        MPI_Abort(MPI_COMM_WORLD, 1);
    }

    MPI_Comm_rank(MPI_COMM_WORLD, &rank);

    pairs = nprocs >> 1;

    /* create test communicator */
    MPI_Comm_split (MPI_COMM_WORLD, rank & ~1, 0, &test_comm);
    MPI_Comm_split (MPI_COMM_WORLD, (rank & 1) ? MPI_UNDEFINED : 0, 0, &leader_comm);

    MPI_Comm_group (test_comm, &comm_group);

    rmamt_parse_options ("rmamt_bibw", argc, argv);

    if (rmamt_output_file && 0 == rank) {
        output_file = fopen (rmamt_output_file, "w");
        if (NULL == output_file) {
            fprintf (stderr, "Could not open %s for writing\n", rmamt_output_file);
            MPI_Abort (MPI_COMM_WORLD, 1);
        }
        free (rmamt_output_file);
    }

    if (rmamt_bind_threads) {
        rc = rmamt_bind_init ();
        if (-1 == rc) {
            printf ("***** WARNING: Thread binding requested but not available *****\n");
        }
    }

    min_size = rmamt_min_size / rmamt_threads;
    if (min_size < 1) {
        min_size = 1;
    }

    max_size = rmamt_max_size / (rmamt_threads * pairs);
    win_count = rmamt_win_per_thread ? rmamt_threads : 1;
    win_size = rmamt_max_size / win_count;

    obuf = rmamt_malloc (rmamt_max_size);
    if (!obuf) {
        printf("Cannot allocate buffer\n");
        MPI_Abort(MPI_COMM_WORLD, 1);
    }

    for (int k = 0 ; k < rmamt_threads ; ++k) {
        memset(obuf + max_size * k, (char)k%9+'0', max_size);
    }

    /* create windows */
    for (int i = 0 ; i < win_count ; ++i) {
        MPI_CHECK(MPI_Win_allocate (win_size, 1, MPI_INFO_NULL, test_comm,
                                    tbufs + i, win + i));
        if (win_size) {
            memset (tbufs[i], '-', win_size);
        }
    }

    if (RMAMT_PSCW == rmamt_sync) {
        MPI_Group_incl (comm_group, 1, &(int){!(rank & 1)}, &group);
    }

    if (!rank) {
        printf ("##########################################\n");
        printf ("# RMA-MT Bi-directional Bandwidth\n");
        printf ("#\n");
        printf ("# Operation: %s\n", rmamt_operation_strings[rmamt_operation]);
        printf ("# Sync: %s\n", rmamt_sync_strings[rmamt_sync]);
        printf ("# Thread count: %u\n", (unsigned) rmamt_threads);
        printf ("# Number of MPI process pairs: %d\n", pairs);
        printf ("# Iterations: %u\n", (unsigned) rmamt_iterations);
        printf ("# Bind worker threads: %s\n", rmamt_bind_threads ? "yes" : "no");
        printf ("# Number of windows: %u\n", rmamt_win_per_thread ? rmamt_threads : 1);
        printf ("##########################################\n");
        printf ("  BpT(%i)\t  BxT(%i)\tBandwidth(MiB/s)\tMessage_Rate(M/s)\n", rmamt_threads, rmamt_threads);
        if (output_file) {
            fprintf (output_file, "##########################################\n");
            fprintf (output_file, "# RMA-MT Bi-directional Bandwidth\n");
            fprintf (output_file, "#\n");
            fprintf (output_file, "# Operation: %s\n", rmamt_operation_strings[rmamt_operation]);
            fprintf (output_file, "# Sync: %s\n", rmamt_sync_strings[rmamt_sync]);
            fprintf (output_file, "# Thread count: %u\n", (unsigned) rmamt_threads);
            fprintf (output_file, "# Number of MPI process pairs: %d\n", pairs);
            fprintf (output_file, "# Iterations: %u\n", (unsigned) rmamt_iterations);
            fprintf (output_file, "# Bind worker threads: %s\n", rmamt_bind_threads ? "yes" : "no");
            fprintf (output_file, "# Number of windows: %u\n", rmamt_win_per_thread ? rmamt_threads : 1);
            fprintf (output_file, "##########################################\n");
            fprintf (output_file, "BpT(%i),BxT(%i),Bandwidth(MiB/s),Message_Rate(M/s)\n", rmamt_threads, rmamt_threads);
        }
    }

    thread_barrier_init (rmamt_threads);

    /* attempt to measure barrier overhead to improve the quality of the benchmark as the number
     * of MPI process pairs increases */
    MPI_Barrier (MPI_COMM_WORLD);
    uint64_t start = time_getns ();
    for (int i = 0 ; i < 100 ; ++i) {
        MPI_Barrier (MPI_COMM_WORLD);
    }

    barrier_time = (time_getns () - start) / 100;

    for (int i = 0 ; i < rmamt_threads ; ++i) {
        args[i].tid = i;
        args[i].max_size = max_size;
        args[i].min_size = min_size;
        args[i].win = rmamt_win_per_thread ? win[i] : win[0];
        args[i].group = group;
        args[i].all_sync = (RMAMT_ALL_FLUSH == rmamt_sync);
        args[i].target = !(rank & 1);

        args[i].do_init = (rmamt_win_per_thread || 0 == i);
        args[i].do_sync = (args[i].all_sync || 0 == i);

        pthread_create(id+i, NULL, (void *(*)(void *)) rmamt_new_fns[rmamt_operation][rmamt_sync], args+i);
    }

    for (int i = 0 ; i < rmamt_threads ; ++i) {
        pthread_join(id[i], NULL);
    }

    for (uint64_t j = min_size, step = 0 ; j <= max_size ; j <<= 1, ++step) {
        double speed = 0.0, msg_rate = 0.0;

        for (int i = 0 ; i < rmamt_threads ; ++i) {
            speed += ((double) (j * rmamt_iterations) * 953.67431640625) / (double) times[i][step];
            msg_rate += ((double) (rmamt_iterations) * 1000000000.0) / (double) times[i][step];
        }

        MPI_Allreduce (MPI_IN_PLACE, &speed, 1, MPI_DOUBLE, MPI_SUM, MPI_COMM_WORLD);
        MPI_Allreduce (MPI_IN_PLACE, &msg_rate, 1, MPI_DOUBLE, MPI_SUM, MPI_COMM_WORLD);

        if (0 == rank) {
            if (output_file) {
                fprintf (output_file, "%lu,%lu,%lf,%lf\n", (unsigned long) j, (unsigned long) j * rmamt_threads,
                         speed, msg_rate);
            }

            printf ("%9lu\t%9lu\t%13.3lf\t\t%13.3lf\n", (unsigned long) j, (unsigned long) j * rmamt_threads,
                    speed, msg_rate);
        }
    }

    if (MPI_GROUP_NULL != group) {
        MPI_Group_free (&group);
    }
    MPI_Group_free (&comm_group);

    MPI_Barrier (MPI_COMM_WORLD);

    if (output_file) {
        fclose (output_file);
    }

    for (int i = 0 ; i < win_count ; ++i) {
        MPI_CHECK(MPI_Win_free(win + i));
    }

    if (rmamt_bind_threads) {
        rmamt_bind_finalize ();
    }

    rmamt_free (obuf, rmamt_max_size);

    MPI_Comm_free (&test_comm);
    MPI_T_finalize ();
    MPI_Finalize();
    return 0;
}

uint64_t find_max(){
  uint64_t max = 0;
  int tmp;
  int sz = sizeof(thread_etimes)/sizeof(thread_etimes[0]);
  for (tmp = 0; tmp < sz; tmp++)
    if(max < thread_etimes[tmp]) max=thread_etimes[tmp];
  return (double) max;
}

#define DEFINE_ORIGIN_THREAD_FN(sync, type, fn, init_fn, start_sync, end_sync, fini_fn, expose, release) \
    static void *bibw_ ## type ## _ ## sync ## _new (ArgStruct* a) {      \
        const int tid = (int) a->tid;                                   \
        uint64_t start, stop;                                           \
        size_t max_size = a->max_size;                                  \
        size_t min_size = a->min_size;                                  \
        int barrier_cycle = 0;                                          \
                                                                        \
        if (rmamt_bind_threads) {                                       \
            rmamt_bind (tid);                                           \
        }                                                               \
                                                                        \
        thread_etimes[tid] = time_getns ();                             \
                                                                        \
        if (a->do_init) {                                               \
            init_fn;                                                    \
        }                                                               \
                                                                        \
        for (uint32_t j = min_size, cycle = 0 ; j <= max_size ; j <<= 1, ++cycle) { \
            if (0 == tid) {                                             \
                expose;                                                 \
            }                                                           \
                                                                        \
            if (a->do_sync) {                                           \
                start_sync;                                             \
            }                                                           \
                                                                        \
            thread_barrier (barrier_cycle++);                           \
                                                                        \
            for (int l = 0 ; l < RMAMT_WARMUP_ITERATIONS ; l++) {       \
                fn (obuf + tid * j, j, MPI_BYTE, a->target, 0, j,       \
                    MPI_BYTE, a->win);                                  \
            }                                                           \
                                                                        \
            if (!a->all_sync) {                                         \
                thread_barrier (barrier_cycle++);                       \
            }                                                           \
                                                                        \
            if (a->do_sync) {                                           \
                end_sync;                                               \
            }                                                           \
                                                                        \
            if (0 == tid) {                                             \
                release;                                                \
                MPI_Barrier (MPI_COMM_WORLD);                           \
            }                                                           \
                                                                        \
            thread_barrier (barrier_cycle++);                           \
                                                                        \
            start = time_getns ();                                      \
                                                                        \
            if (0 == tid) {                                             \
                expose;                                                 \
            }                                                           \
                                                                        \
            if (a->do_sync) {                                           \
                start_sync;                                             \
            }                                                           \
                                                                        \
            thread_barrier (barrier_cycle++);                           \
                                                                        \
            for (int l = 0 ; l < rmamt_iterations ; l++) {              \
                fn (obuf + tid * j, j, MPI_BYTE, a->target, 0, j,       \
                    MPI_BYTE, a->win);                                  \
            }                                                           \
                                                                        \
            thread_barrier (barrier_cycle++);                           \
                                                                        \
            if (a->do_sync) {                                           \
                end_sync;                                               \
            }                                                           \
                                                                        \
            if (0 == tid) {                                             \
                release;                                                \
                MPI_Barrier (MPI_COMM_WORLD);                           \
            }                                                           \
                                                                        \
            thread_barrier (barrier_cycle++);                           \
                                                                        \
            stop = time_getns ();                                       \
                                                                        \
            times[tid][cycle] = stop - start - barrier_time;            \
                                                                        \
            thread_barrier (barrier_cycle++);                           \
        }                                                               \
                                                                        \
        if (a->do_init) {                                               \
            fini_fn;                                                    \
        }                                                               \
                                                                        \
        return 0;                                                       \
    }

DEFINE_ORIGIN_THREAD_FN(lock_all, put, MPI_Put, (void) 0, MPI_Win_lock_all (0, a->win), MPI_Win_unlock_all (a->win), (void) 0, (void) 0, (void) 0)
DEFINE_ORIGIN_THREAD_FN(lock_all, get, MPI_Get, (void) 0, MPI_Win_lock_all (0, a->win), MPI_Win_unlock_all (a->win), (void) 0, (void) 0, (void) 0)
DEFINE_ORIGIN_THREAD_FN(lock_per_rank, put, MPI_Put, (void) 0, MPI_Win_lock (MPI_LOCK_SHARED, a->target, 0, a->win), MPI_Win_unlock (a->target, a->win),
                        (void) 0, (void) 0, (void) 0)
DEFINE_ORIGIN_THREAD_FN(lock_per_rank, get, MPI_Get, (void) 0, MPI_Win_lock (MPI_LOCK_SHARED, a->target, 0, a->win), MPI_Win_unlock (a->target, a->win),
                        (void) 0, (void) 0, (void) 0)
DEFINE_ORIGIN_THREAD_FN(flush, put, MPI_Put, MPI_Win_lock (MPI_LOCK_SHARED, a->target, 0, a->win), (void) 0, MPI_Win_flush (a->target, a->win),
                        MPI_Win_unlock (a->target, a->win), (void) 0, (void) 0)
DEFINE_ORIGIN_THREAD_FN(flush, get, MPI_Get, MPI_Win_lock (MPI_LOCK_SHARED, a->target, 0, a->win), (void) 0, MPI_Win_flush (a->target, a->win),
                        MPI_Win_unlock (a->target, a->win), (void) 0, (void) 0)
DEFINE_ORIGIN_THREAD_FN(pscw, put, MPI_Put, (void) 0, MPI_Win_start (a->group, 0, a->win), MPI_Win_complete (a->win), (void) 0,
                        MPI_Win_post (a->group, 0, a->win), MPI_Win_wait (a->win))
DEFINE_ORIGIN_THREAD_FN(pscw, get, MPI_Get, (void) 0, MPI_Win_start (a->group, 0, a->win), MPI_Win_complete (a->win), (void) 0,
                        MPI_Win_post (a->group, 0, a->win), MPI_Win_wait (a->win))
DEFINE_ORIGIN_THREAD_FN(fence, put, MPI_Put, MPI_Win_fence (MPI_MODE_NOPRECEDE, a->win), (void) 0, MPI_Win_fence (0, a->win), (void) 0,
                        MPI_Win_fence (0, a->win), (void) 0)
DEFINE_ORIGIN_THREAD_FN(fence, get, MPI_Get, MPI_Win_fence (MPI_MODE_NOPRECEDE, a->win), (void) 0, MPI_Win_fence (0, a->win), (void) 0,
                        MPI_Win_fence (0, a->win), (void) 0)
DEFINE_ORIGIN_THREAD_FN(flush_all, put, MPI_Put, MPI_Win_lock (MPI_LOCK_SHARED, a->target, 0, a->win), (void) 0, MPI_Win_flush_all (a->win),
                        MPI_Win_unlock (a->target, a->win), (void) 0, (void) 0)
DEFINE_ORIGIN_THREAD_FN(flush_all, get, MPI_Get, MPI_Win_lock (MPI_LOCK_SHARED, a->target, 0, a->win), (void) 0, MPI_Win_flush_all (a->win),
                        MPI_Win_unlock (a->target, a->win), (void) 0, (void) 0)
DEFINE_ORIGIN_THREAD_FN(flush_local, put, MPI_Put, MPI_Win_lock (MPI_LOCK_SHARED, a->target, 0, a->win), (void) 0, MPI_Win_flush_local (a->target, a->win),
                        MPI_Win_unlock (a->target, a->win), (void) 0, (void) 0)
DEFINE_ORIGIN_THREAD_FN(flush_local, get, MPI_Get, MPI_Win_lock (MPI_LOCK_SHARED, a->target, 0, a->win), (void) 0, MPI_Win_flush_local (a->target, a->win),
                        MPI_Win_unlock (a->target, a->win), (void) 0, (void) 0)
DEFINE_ORIGIN_THREAD_FN(flush_local_all, put, MPI_Put, MPI_Win_lock (MPI_LOCK_SHARED, a->target, 0, a->win), (void) 0, MPI_Win_flush_local_all (a->win),
                        MPI_Win_unlock (a->target, a->win), (void) 0, (void) 0)
DEFINE_ORIGIN_THREAD_FN(flush_local_all, get, MPI_Get, MPI_Win_lock (MPI_LOCK_SHARED, a->target, 0, a->win), (void) 0, MPI_Win_flush_local_all (a->win),
                        MPI_Win_unlock (a->target, a->win), (void) 0, (void) 0)
