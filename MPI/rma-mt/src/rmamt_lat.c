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
#include <inttypes.h>
#include <unistd.h>

uint64_t find_max();

/* target-side functions for single window */
static void *runfunc_put (ArgStruct* a);
static void *runfunc_get (ArgStruct* a);
static void *runfunc_pscw (ArgStruct* a);
static void *runfunc_fence (ArgStruct* a);

/* origin-side window per thread functions */
static void *lat_put_lock_all_winperthread (ArgStruct* a);
static void *lat_get_lock_all_winperthread (ArgStruct* a);
static void *lat_put_fence_winperthread (ArgStruct* a);
static void *lat_get_fence_winperthread (ArgStruct* a);
static void *lat_put_lock_per_rank_winperthread (ArgStruct* a);
static void *lat_get_lock_per_rank_winperthread (ArgStruct* a);
static void *lat_put_flush_winperthread (ArgStruct* a);
static void *lat_get_flush_winperthread (ArgStruct* a);
static void *lat_put_pscw_winperthread (ArgStruct* a);
static void *lat_get_pscw_winperthread (ArgStruct* a);

static rmamt_fn_t rmamt_winperthread_fns[RMAMT_OPERATIONS_MAX][RMAMT_SYNC_MAX] = {
    [RMAMT_PUT] = {
        [RMAMT_LOCK_ALL] = lat_put_lock_all_winperthread,
        [RMAMT_FENCE] = lat_put_fence_winperthread,
        [RMAMT_LOCK] = lat_put_lock_per_rank_winperthread,
        [RMAMT_FLUSH] = lat_put_flush_winperthread,
        [RMAMT_PSCW] = lat_put_pscw_winperthread,
    },
    [RMAMT_GET] = {
        [RMAMT_LOCK_ALL] = lat_get_lock_all_winperthread,
        [RMAMT_FENCE] = lat_get_fence_winperthread,
        [RMAMT_LOCK] = lat_get_lock_per_rank_winperthread,
        [RMAMT_FLUSH] = lat_get_flush_winperthread,
        [RMAMT_PSCW] = lat_get_pscw_winperthread,
    },
};

/* origin-side functions */
static void *lat_orig_lock_all (ArgStruct *a);
static void *lat_orig_lock (ArgStruct *a);
static void *lat_orig_flush (ArgStruct *a);
static void *lat_orig_fence (ArgStruct *a);
static void *lat_orig_pscw (ArgStruct *a);

static rmamt_fn_t rmamt_origin_fns[RMAMT_SYNC_MAX] = {
    [RMAMT_LOCK_ALL] = lat_orig_lock_all,
    [RMAMT_FENCE] = lat_orig_fence,
    [RMAMT_LOCK] = lat_orig_lock,
    [RMAMT_FLUSH] = lat_orig_flush,
    [RMAMT_PSCW] = lat_orig_pscw,
};

static ArgStruct args[MAX_THREADS];
static uint64_t thread_etimes[MAX_THREADS];
static char* tbufs[MAX_THREADS];
static char* obuf;
static MPI_Win win[MAX_THREADS];
static int64_t times[MAX_THREADS][256];

int main(int argc,char *argv[])
{
    MPI_Group group = MPI_GROUP_NULL, comm_group;
    int nprocs, provided, rank, rc;
    pthread_t id[MAX_THREADS];
    MPI_Request req;
    int64_t win_size;
    int win_count;
    size_t max_size, min_size;
    uint64_t stt, ttt = 0;
    FILE *output_file = NULL;

    MPI_Init_thread(&argc, &argv, MPI_THREAD_MULTIPLE, &provided);
    if (provided != MPI_THREAD_MULTIPLE) {
        printf("Thread multiple needed\n");
        MPI_Abort(MPI_COMM_WORLD, 1);
    }

    MPI_Comm_size(MPI_COMM_WORLD,&nprocs);
    if (nprocs != 2) {
        printf("Run with 2 processes\n");
        MPI_Abort(MPI_COMM_WORLD, 1);
    }

    MPI_Comm_rank(MPI_COMM_WORLD,&rank);
    MPI_Comm_group (MPI_COMM_WORLD, &comm_group);

    rmamt_parse_options ("rmamt_bw", argc, argv);

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

    max_size = rmamt_max_size / rmamt_threads;
    win_count = rmamt_win_per_thread ? rmamt_threads : 1;
    win_size = (0 != rank) ? rmamt_max_size / win_count : 0;

    obuf = (char *) rmamt_malloc (rmamt_max_size);
    if (!obuf) {
        printf("Cannot allocate buffer\n");
        MPI_Abort(MPI_COMM_WORLD, 1);
    }

    for (int k = 0 ; k < rmamt_threads ; ++k) {
        memset(obuf + max_size * k, (char)k%9+'0', max_size);
    }

    /* create windows */
    for (int i = 0 ; i < win_count ; ++i) {
        MPI_CHECK(MPI_Win_allocate (win_size, 1, MPI_INFO_NULL, MPI_COMM_WORLD,
                                    tbufs + i, win + i));
        if (win_size) {
            memset (tbufs[i], '-', win_size);
        }
    }

    if (RMAMT_PSCW == rmamt_sync) {
        MPI_Group_incl (comm_group, 1, &(int){!rank}, &group);
    }


    if (!rank) {
        printf ("##########################################\n");
        printf ("# RMA-MT Latency\n");
        printf ("#\n");
        printf ("# Operation: %s\n", rmamt_operation_strings[rmamt_operation]);
        printf ("# Sync: %s\n", rmamt_sync_strings[rmamt_sync]);
        printf ("# Thread count: %u\n", (unsigned) rmamt_threads);
        printf ("# Iterations: %u\n", (unsigned) rmamt_iterations);
        printf ("# Ibarrier: %s, sleep interval: %uns\n", rmamt_use_ibarrier ? "yes" : "no",
                rmamt_sleep_interval);
	printf ("# Bind worker threads: %s\n", rmamt_bind_threads ? "yes" : "no");
	printf ("# Number of windows: %u\n", rmamt_win_per_thread ? rmamt_threads : 1);
        printf ("##########################################\n");
        printf ("BpT(%i)\tBxT(%i)\tLatency (us)\n", rmamt_threads, rmamt_threads);

	if (output_file) {
	    fprintf (output_file, "##########################################\n");
	    fprintf (output_file, "# RMA-MT Latency\n");
	    fprintf (output_file, "#\n");
	    fprintf (output_file, "# Operation: %s\n", rmamt_operation_strings[rmamt_operation]);
	    fprintf (output_file, "# Sync: %s\n", rmamt_sync_strings[rmamt_sync]);
	    fprintf (output_file, "# Thread count: %u\n", (unsigned) rmamt_threads);
	    fprintf (output_file, "# Iterations: %u\n", (unsigned) rmamt_iterations);
	    fprintf (output_file, "# Ibarrier: %s, sleep interval: %uns\n", rmamt_use_ibarrier ? "yes" : "no",
		     rmamt_sleep_interval);
	    fprintf (output_file, "# Bind worker threads: %s\n", rmamt_bind_threads ? "yes" : "no");
	    fprintf (output_file, "# Number of windows: %u\n", rmamt_win_per_thread ? rmamt_threads : 1);
	    fprintf (output_file, "##########################################\n");
	    fprintf (output_file, "BpT(%i),BxT(%i),Latency(us)\n", rmamt_threads, rmamt_threads);
	}
    }

    if (0 == rank) {
        thread_barrier_init (rmamt_win_per_thread ? rmamt_threads : rmamt_threads + 1);

        stt = time_getns ();

        for (int i = 0 ; i < rmamt_threads ; ++i) {
            args[i].tid = i;
            args[i].max_size = max_size;
	    args[i].min_size = min_size;
            args[i].win = rmamt_win_per_thread ? win[i] : win[0];
            args[i].group = group;

            //printf("args[%u].tid = %u\n", i, arggs[i].tid);
            if (!rmamt_win_per_thread) {
                pthread_create(id+i, NULL, (void *(*)(void *)) (RMAMT_GET == rmamt_operation ? runfunc_get : runfunc_put), args+i);
            } else {
                pthread_create(id+i, NULL, (void *(*)(void *)) rmamt_winperthread_fns[rmamt_operation][rmamt_sync], args+i);
            }
        }


        /* wait for threads to be ready */
        thread_barrier (0);

        if (ttt < find_max()-stt) ttt = find_max()-stt;

        if (!rmamt_win_per_thread) {
            rmamt_origin_fns[rmamt_sync] (&(ArgStruct){.min_size = min_size, .max_size = max_size, .group = group, .win = win[0]});
        }

        for (int i = 0 ; i < rmamt_threads ; ++i) {
            pthread_join(id[i], NULL);
        }

        for (uint32_t j = min_size, step = 0 ; j <= max_size ; j <<= 1, ++step) {
            float latency = 0.0;

            for (int i = 0 ; i < win_count ; ++i) {
                latency += (float) times[i][step] / 1000.0;
            }

            latency /= (float) (win_count * rmamt_iterations);

	    if (output_file) {
		fprintf (output_file, "%lu,%lu,%f\n", (unsigned long) j, (unsigned long) j * rmamt_threads, latency);
	    }

            printf ("%lu\t%lu\t%f\n", (unsigned long) j, (unsigned long) j * rmamt_threads, latency);
        }

        if (rmamt_use_ibarrier) {
            MPI_Ibarrier (MPI_COMM_WORLD, &req);
            MPI_Wait (&req, MPI_STATUS_IGNORE);
        } else {
            MPI_Barrier (MPI_COMM_WORLD);
        }
    } else {
        if (RMAMT_PSCW == rmamt_sync || RMAMT_FENCE == rmamt_sync) {
            args[0].tid = 0;
            args[0].max_size = max_size;
	    args[0].min_size = min_size;
            args[0].group = group;
            args[0].win = win[0];
            args[0].group = group;

            if (rmamt_win_per_thread) {
                for (int i = 1 ; i < rmamt_threads ; ++i) {
                    args[i].tid = i;
                    args[i].max_size = max_size;
		    args[i].min_size = min_size;
                    args[i].group = group;
                    args[i].win = win[i];
                    args[i].group = group;

                    //printf("args[%u].tid = %u\n", i, arggs[i].tid);
                    if (RMAMT_PSCW == rmamt_sync) {
                        pthread_create(id+i, NULL, (void *(*)(void *)) runfunc_pscw, args + i);
                    } else {
                        pthread_create(id+i, NULL, (void *(*)(void *)) runfunc_fence, args + i);
                    }
                }
            }

            if (RMAMT_PSCW == rmamt_sync) {
                runfunc_pscw (args);
            } else {
                runfunc_fence (args);
            }

            if (rmamt_win_per_thread) {
                for (int i = 1 ; i < rmamt_threads ; ++i) {
                    pthread_join(id[i], NULL);
                }
            }
        }

        if (rmamt_use_ibarrier) {
            const struct timespec interval = {.tv_sec = rmamt_sleep_interval / 1000000000,
                                              .tv_nsec = rmamt_sleep_interval % 1000000000};
            int flag = 0;

            MPI_Ibarrier (MPI_COMM_WORLD, &req);
            do {
                /* sleep for 10us to simulate a non-mpi workload */
                nanosleep (&interval, NULL);
                /* enter MPI. this will progress pt2pt type implementations */
                MPI_Test (&req, &flag, MPI_STATUS_IGNORE);
            } while (!flag);
        } else {
            MPI_Barrier (MPI_COMM_WORLD);
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

    if (rank == 0){
        printf("Max time for thread create: %lu ns\n", (unsigned long) ttt);
    }

    for (int i = 0 ; i < win_count ; ++i) {
        MPI_CHECK(MPI_Win_free(win + i));
    }

    if (rmamt_bind_threads) {
	rmamt_bind_finalize ();
    }

    rmamt_free (obuf, rmamt_max_size);

    MPI_Finalize();
    return 0;
}

static void *runfunc_pscw (ArgStruct* a) {
    ArgStruct* s = (ArgStruct*) a;
    MPI_Win twin = s->win;
    size_t max_size = a->max_size;
    size_t min_size = a->min_size;
    MPI_Group group = a->group;

    for (uint64_t j = min_size ; j <= max_size ; j <<= 1) {
        for (int i = 0 ; i < rmamt_iterations + RMAMT_WARMUP_ITERATIONS ; ++i) {
            MPI_Win_post (group, 0, twin);
            MPI_Win_wait (twin);
        }
    }

    return NULL;
}

static void *runfunc_fence (ArgStruct* a) {
    ArgStruct* s = (ArgStruct*) a;
    MPI_Win twin = s->win;
    size_t max_size = a->max_size;
    size_t min_size = a->min_size;

    MPI_Win_fence (MPI_MODE_NOPRECEDE, twin);

    for (uint64_t j = min_size ; j <= max_size ; j <<= 1) {
        for (int i = 0 ; i < rmamt_iterations + RMAMT_WARMUP_ITERATIONS ; ++i) {        
            MPI_Win_fence (0, twin);
        }
    }

    return NULL;
}

uint64_t find_max(){
  uint64_t max = 0;
  int tmp;
  int sz = sizeof(thread_etimes)/sizeof(thread_etimes[0]);
  for (tmp = 0; tmp < sz; tmp++)
    if(max < thread_etimes[tmp]) max=thread_etimes[tmp];
  return (double) max;
}

#define DEFINE_ORIGIN_THREAD_FN(sync, type, fn, init_fn, start_sync, end_sync, fini_fn) \
    static void *lat_ ## type ## _ ## sync ## _winperthread (ArgStruct* a) { \
        const int tid = (int) a->tid;                                   \
        uint64_t start, stop;                                           \
        size_t max_size = a->max_size;                                  \
        size_t min_size = a->min_size;                                  \
                                                                        \
	if (rmamt_bind_threads) {					\
	    rmamt_bind (tid);						\
	}								\
									\
        thread_etimes[tid] = time_getns ();                             \
                                                                        \
        init_fn;                                                        \
        /* signal the main thread that we are ready */                  \
        thread_barrier (0);                                             \
                                                                        \
        for (uint32_t j = min_size, cycle = 0, barrier_cycle = 1 ; j <= max_size ; j <<= 1) { \
	  for (int l = 0 ; l < rmamt_iterations + RMAMT_WARMUP_ITERATIONS ; l++) { \
	    thread_barrier (barrier_cycle++);				\
	    start = time_getns ();					\
	    start_sync;							\
	    								\
	    fn (obuf + tid * j, j, MPI_BYTE, 1, 0, j, MPI_BYTE, a->win); \
            								\
	    end_sync;							\
	    stop = time_getns ();					\
            								\
	    thread_barrier (barrier_cycle++);				\
            								\
	    if (l >= RMAMT_WARMUP_ITERATIONS) {				\
	      times[tid][cycle] += stop - start;			\
	    }								\
	  }								\
	  ++cycle;							\
        }                                                               \
									\
        fini_fn;                                                        \
        								\
        return 0;                                                       \
    }

DEFINE_ORIGIN_THREAD_FN(lock_all, put, MPI_Put, (void) 0, MPI_Win_lock_all (0, a->win), MPI_Win_unlock_all (a->win), (void) 0)
DEFINE_ORIGIN_THREAD_FN(lock_all, get, MPI_Get, (void) 0, MPI_Win_lock_all (0, a->win), MPI_Win_unlock_all (a->win), (void) 0)
DEFINE_ORIGIN_THREAD_FN(lock_per_rank, put, MPI_Put, (void) 0, MPI_Win_lock (MPI_LOCK_SHARED, 1, 0, a->win), MPI_Win_unlock (1, a->win), (void) 0)
DEFINE_ORIGIN_THREAD_FN(lock_per_rank, get, MPI_Get, (void) 0, MPI_Win_lock (MPI_LOCK_SHARED, 1, 0, a->win), MPI_Win_unlock (1, a->win), (void) 0)
DEFINE_ORIGIN_THREAD_FN(flush, put, MPI_Put, MPI_Win_lock (MPI_LOCK_SHARED, 1, 0, a->win), (void) 0, MPI_Win_flush (1, a->win), MPI_Win_unlock (1, a->win))
DEFINE_ORIGIN_THREAD_FN(flush, get, MPI_Get, MPI_Win_lock (MPI_LOCK_SHARED, 1, 0, a->win), (void) 0, MPI_Win_flush (1, a->win), MPI_Win_unlock (1, a->win))
DEFINE_ORIGIN_THREAD_FN(pscw, put, MPI_Put, (void) 0, MPI_Win_start (a->group, 0, a->win), MPI_Win_complete (a->win), (void) 0)
DEFINE_ORIGIN_THREAD_FN(pscw, get, MPI_Get, (void) 0, MPI_Win_start (a->group, 0, a->win), MPI_Win_complete (a->win), (void) 0)
DEFINE_ORIGIN_THREAD_FN(fence, put, MPI_Put, MPI_Win_fence (MPI_MODE_NOPRECEDE, a->win), (void) 0, MPI_Win_fence (0, a->win), (void) 0)
DEFINE_ORIGIN_THREAD_FN(fence, get, MPI_Get, MPI_Win_fence (MPI_MODE_NOPRECEDE, a->win), (void) 0, MPI_Win_fence (0, a->win), (void) 0)

/* origin-side loops */
#define DEFINE_ORIGIN_FN(sync, init_fn, start_sync, end_sync, fini_fn)  \
    static void *lat_orig_ ## sync (ArgStruct *a) {			\
        size_t max_size = a->max_size;                                  \
        size_t min_size = a->min_size;                                  \
	uint64_t stime, etime;						\
									\
        init_fn;                                                        \
									\
        for (uint64_t j = min_size, cycle = 0, barrier_cycle = 1 ; j <= max_size ; j <<= 1, cycle++) { \
            uint64_t setime, btime;					\
                                                                        \
	    times[0][cycle] = 0;					\
									\
	    for (int l = 0 ; l < rmamt_iterations + RMAMT_WARMUP_ITERATIONS ; l++) { \
		stime = time_getns();					\
		MPI_CHECK( start_sync );				\
		setime = time_getns ();					\
									\
		thread_barrier (barrier_cycle++);			\
		thread_barrier (barrier_cycle++);			\
									\
		btime = time_getns();					\
		MPI_CHECK( end_sync );					\
		etime = time_getns ();					\
									\
		if (l >= RMAMT_WARMUP_ITERATIONS) {			\
		    uint64_t avg = 0;					\
		    for (int k = 0 ; k < rmamt_threads ; ++k) {		\
			avg += times[k + 1][cycle];			\
		    }							\
		    avg /= rmamt_threads;				\
		    times[0][cycle] += setime - stime + etime - btime + avg; \
		}							\
	    }								\
        }                                                               \
                                                                        \
        fini_fn;                                                        \
                                                                        \
        return NULL;                                                    \
    }

DEFINE_ORIGIN_FN(lock_all, (void) 0, MPI_Win_lock_all(0, a->win), MPI_Win_unlock_all (a->win), (void) 0)
DEFINE_ORIGIN_FN(lock, (void) 0, MPI_Win_lock(MPI_LOCK_SHARED, 1, 0, a->win), MPI_Win_unlock (1, a->win), (void) 0)
DEFINE_ORIGIN_FN(flush, MPI_Win_lock (MPI_LOCK_SHARED, 1, 0, a->win), MPI_SUCCESS, MPI_Win_flush (1, a->win), MPI_Win_unlock (1, a->win))
DEFINE_ORIGIN_FN(fence, MPI_Win_fence (MPI_MODE_NOPRECEDE, a->win), MPI_SUCCESS, MPI_Win_fence (0, a->win), (void) 0)
DEFINE_ORIGIN_FN(pscw, (void) 0, MPI_Win_start (a->group, 0, a->win), MPI_Win_complete (a->win), (void) 0)

#define DEFINE_ORIGIN_THREAD_RUNFN(fn, type)                            \
    static void *runfunc_ ## type (ArgStruct* a) {                      \
        int tid = (int) a->tid;                                         \
        size_t max_size = a->max_size;                                  \
        size_t min_size = a->min_size;                                  \
                                                                        \
	if (rmamt_bind_threads) {					\
	    rmamt_bind (tid);						\
	}								\
									\
        thread_etimes[tid] = time_getns ();                             \
                                                                        \
        /* signal the main thread that we are ready */                  \
        thread_barrier (0);                                             \
									\
        for (uint32_t j = min_size, step = 0, cycle = 1 ; j <= max_size ; j <<= 1, ++step) { \
	    for (int l = 0 ; l < rmamt_iterations + RMAMT_WARMUP_ITERATIONS ; l++) { \
		uint64_t stime;						\
		thread_barrier (cycle++);				\
		stime = time_getns ();					\
		fn (obuf + tid * j, j, MPI_BYTE, 1, tid *  j, j,	\
		    MPI_BYTE, a->win);					\
		times[tid + 1][step] = time_getns() - stime;		\
		thread_barrier (cycle++);				\
	    }								\
	}								\
                                                                        \
        return 0;                                                       \
    }

DEFINE_ORIGIN_THREAD_RUNFN(MPI_Get, get)
DEFINE_ORIGIN_THREAD_RUNFN(MPI_Put, put)
