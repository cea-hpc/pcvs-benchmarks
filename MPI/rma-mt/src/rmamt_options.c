/* -*- Mode: C; c-basic-offset:4 ; -*- */
/*
 *   Copyright (c) 2016-2018 Los Alamos National Security, LLC. All rights
 *                           reserved.
 *   See COPYRIGHT notice in top-level directory.
 */

#include <stdlib.h>
#include <stdio.h>
#include <getopt.h>
#include <errno.h>
#include <mpi.h>
#include <string.h>
#include <strings.h>

#include "rmamt_options.h"

char *rmamt_operation_strings[] = {
    [RMAMT_PUT] = "put",
    [RMAMT_GET] = "get",
};

char *rmamt_sync_strings[] = {
    [RMAMT_LOCK_ALL] = "lock_all",
    [RMAMT_FENCE] = "fence",
    [RMAMT_LOCK] = "lock",
    [RMAMT_FLUSH] = "flush",
    [RMAMT_PSCW] = "pscw",
    [RMAMT_ALL_FLUSH] = "all_flush",
    [RMAMT_FLUSH_ALL] = "flush_all",
    [RMAMT_FLUSH_LOCAL] = "flush_local",
    [RMAMT_FLUSH_LOCAL_ALL] = "flush_local_all",
    [RMAMT_SYNC_MAX] = NULL,
};

bool rmamt_win_per_thread = false;
bool rmamt_use_ibarrier = false;
int rmamt_threads = 1;
int rmamt_iterations = 1000;
unsigned rmamt_sleep_interval = 10000;
int rmamt_sync;
int rmamt_operation;
unsigned long rmamt_max_size;
unsigned long rmamt_min_size;
bool rmamt_bind_threads;
char *rmamt_output_file;

static void print_usage (const char *name, bool failure)
{
    int rank;

    MPI_Comm_rank (MPI_COMM_WORLD, &rank);

    if (0 == rank) {
	printf ("RMA-MT Multi-threaded MPI-3 One-sided benchmarks\n\n"
		"Usage: %s [-wn] [-t <value>] [-s <value>] [-i <value>] <-o put|get> <-s lock_all|fence|lock|flush|pscw|all_flush\n\n"
		"Options:\n\n"
		" -o,--operation=value      Operation to benchmark: put, get\n"
		" -s,--sync=value           Synchronization function to use: lock_all, fence, lock, flush,\n"
		"                           pscw, all_flush, flush_all, flush_local, flush_local_all\n"
		" -m, --max-size=value      Maximum aggregate transfer size\n"
		" -l, --min-size=value      Minimum aggregate transfer size\n"
		" -w,--win-per-thread       Use a different MPI window in each thread\n"
		" -i,--iterations=value     Number of iterations to run for each per message size\n"
		" -t,--threads=value        Number of threads to use\n"
		" -n,--busy-loop            Run busy loop on receiver\n"
		" -x,--bind-threads         Bind threads to unique cores (requires hwloc)\n"
		" -z,--sleep-interval=value Sleep interval in ns to use on receiver if using busy loop\n"
		"                           loop (default: 10000)\n"
		" -r,--result=value         Write parsable output to the specified file\n"
		" -h,--help                 Print this help message\n", name);
    }

    exit (failure ? EXIT_FAILURE : EXIT_SUCCESS);
}

int rmamt_parse_options (const char *name, int argc, char *argv[])
{
    char c, *tmp;
    const struct option options[] = {
	{"win-per-thread", no_argument, NULL, 'w'},
	{"max-size", required_argument, NULL, 'm'},
	{"min-size", required_argument, NULL, 'l'},
	{"bind-threads", no_argument, NULL, 'x'},
	{"iterations", required_argument, NULL, 'i'},
	{"threads", required_argument, NULL, 't'},
	{"busy-loop", no_argument, NULL, 'b'},
	{"sleep-interval", required_argument, NULL, 'z'},
	{"operation", required_argument, NULL, 'o'},
	{"sync", required_argument, NULL, 's'},
	{"result", required_argument, NULL, 'r'},
	{"help", no_argument, NULL, 'h'},
	{NULL}
    };

    /* set default options */
    rmamt_win_per_thread = false;
    rmamt_threads = 1;
    rmamt_iterations = 1000;
    rmamt_use_ibarrier = false;
    /* default to 10us */
    rmamt_sleep_interval = 10000;
    rmamt_max_size = RMAMT_MAX_SIZE;
    rmamt_min_size = 1;
    rmamt_sync = -1;
    rmamt_operation = -1;
    rmamt_bind_threads = false;

    while (-1 != (c = getopt_long (argc, argv, "wi:t:bhz:o:s:m:l:xr:", options, NULL))) {
	switch (c) {
	case 'o':
	    if (0 == strcasecmp (optarg, "put")) {
		rmamt_operation = RMAMT_PUT;
	    } else if (0 == strcasecmp (optarg, "get")) {
		rmamt_operation = RMAMT_GET;
	    } else {
		print_usage (name, true);
	    }
	    break;
	case 's':
	    rmamt_sync = RMAMT_SYNC_MAX;

	    for (int i = 0 ; rmamt_sync_strings[i] ; ++i) {
		if (0 == strcasecmp (optarg, rmamt_sync_strings[i])) {
		    rmamt_sync = i;
		    break;
		}
	    }

	    if (RMAMT_SYNC_MAX == rmamt_sync) {
		print_usage (name, true);	
	    }

	    break;
	case 'w':
	    rmamt_win_per_thread = true;
	    break;
	case 'i':
	    errno = 0;
	    rmamt_iterations = strtoul (optarg, &tmp, 10);
	    if (0 != errno || tmp[0] != '\0') {
		print_usage (name, true);
	    }
	    break;
	case 'm':
	    errno = 0;
	    rmamt_max_size = strtoul (optarg, &tmp, 10);
	    if (0 != errno || tmp[0] != '\0') {
		bool ok = false;

		switch (tmp[0]) {
		case 'G':
		case 'g':
		    rmamt_max_size <<= 10;
		case 'M':
		case 'm':
		    rmamt_max_size <<= 10;
		case 'K':
		case 'k':
		    rmamt_max_size <<= 10;
		    ok = true;
		default:
		    break;
		}
		if (!ok) {
		    print_usage (name, true);
		}
	    }
	    break;
	case 'l':
	    errno = 0;
	    rmamt_min_size = strtoul (optarg, &tmp, 10);
	    if (0 != errno || tmp[0] != '\0') {
		bool ok = false;

		switch (tmp[0]) {
		case 'G':
		case 'g':
		    rmamt_min_size <<= 10;
		case 'M':
		case 'm':
		    rmamt_min_size <<= 10;
		case 'K':
		case 'k':
		    rmamt_min_size <<= 10;
		    ok = true;
		default:
		    break;
		}
		if (!ok) {
		    print_usage (name, true);
		}
	    }
	    break;
	case 't':
	    errno = 0;
	    rmamt_threads = strtoul (optarg, &tmp, 10);
	    if (0 != errno || tmp[0] != '\0') {
		print_usage (name, true);
	    }
	    if (rmamt_threads > MAX_THREADS) {
		printf ("Requested too many threads %d > %d\n", rmamt_threads, MAX_THREADS);
		exit (EXIT_FAILURE);
	    }
	    if (rmamt_threads & (rmamt_threads - 1)) {
		printf ("Threads requested must be a power of 2\n");
		exit (EXIT_FAILURE);
	    }

	    break;
	case 'z':
	    errno = 0;
	    rmamt_sleep_interval = (unsigned) strtoul (optarg, &tmp, 10);
	    if (0 != errno || tmp[0] != '\0') {
		print_usage (name, true);
	    }
	    break;
	case 'b':
	    rmamt_use_ibarrier = true;
	    break;
	case 'r':
	    rmamt_output_file = strdup (optarg);
	    break;
	case 'x':
#if defined(HAVE_LIBHWLOC)
	    printf ("hwloc support needs to be enable to bind threads\n");
	    exit (EXIT_FAILURE);
#endif
	    rmamt_bind_threads = true;
	    break;
	case 'h':
	    print_usage (name, false);
	    break;
	default:
	    print_usage (name, true);
	}
    }

    if (rmamt_sync < 0 || rmamt_operation < 0 || rmamt_min_size > rmamt_max_size) {
	print_usage (name, true);
    }

    return 0;
}
