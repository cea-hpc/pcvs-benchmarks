/*
 * Copyright (C) 2002-2018 the Network-Based Computing Laboratory
 * (NBCL), The Ohio State University.
 *
 * Copyright (C) 2019 Bull S. A. S. All rights reserved.
 * Bull, Rue Jean Jaures, B.P.68, 78340, Les Clayes-sous-Bois, France
 * This is not Free or Open Source software.
 * Please contact Bull S. A. S. for details about its license.
 */

#ifndef __COMMON_H__
#define __COMMON_H__

#include <mpi.h>
#include <mpi-ext.h>
#include <unistd.h>
#include <stdlib.h>
#include <stdio.h>
#include <string.h>

#define MAX_ALIGNMENT       65536
#define MPI_3               (MPI_VERSION >= 3)
#define NOTIF_ID            1
#define LARGE_PRIME 9999999967 /* must be bigger than max notification index */

#ifndef EXPECTED_ERRORS
#   define EXPECTED_ERRORS  0
#endif

#define GETENV(__str,__var,__default) \
    do {char *a=getenv(__str);__var=(a==NULL)?__default: atoi(a);}while(0)

int verbose;
int min_size;
int max_size;
int n_block;
int compute_time;
int n_iter;
int n_warmup;

static void parse_env(){
    GETENV("VERBOSE", verbose, 0);
    GETENV("NBLOCK",n_block,2);
    GETENV("MIN",min_size,1);
    GETENV("MAX",max_size,2*1024*1024);
    GETENV("COMPUTE",compute_time,0);
    GETENV("ITER",n_iter,1000);
    GETENV("WARM",n_warmup,1000);
}
#define COMPUTE compute(compute_time); /* Used for computation emulation */

enum benchmark_type {
    FUNCTIONAL_OK,
    FUNCTIONAL_XFAIL,
    N_BENCH
};

enum WINDOW {
    WIN_CREATE=0,
    WIN_CREATE_NOTIFY,
#if MPI_VERSION >= 3
    WIN_ALLOCATE,
    WIN_ALLOCATE_NOTIFY,
    WIN_DYNAMIC,
#endif
    N_WIN
};

/* Synchronization */
enum SYNC {
    MULTIPLE=0,
    LOCK,
    PSCW,
    FENCE,
#if MPI_VERSION >= 3
    LOCK_ALL,
    FLUSH,
    FLUSH_LOCAL,
    NOTIFICATIONS,
#endif
    N_SYNC
};

/* Benchmark options */
struct options_t {
    enum benchmark_type bench;
    enum WINDOW win;
    enum SYNC sync;
};

extern struct options_t options;
extern char const *sync_info[N_SYNC];
/*
 * Memory Management
 */
void win_and_memory_allocate(int rank, void **sbuf, void **win_base,
            size_t size, enum WINDOW type, MPI_Win *win);
void win_and_memory_free (void *sbuf, void *win_base, enum WINDOW type,
            MPI_Win win);

/*
 * Set Benchmark Properties
 */
void set_header (const char *header);

int get_max_notification_id(void);

/* Fake application compute phase */
void compute(int duration);
#endif // __COMMON_H__
