/* (C) Northwestern University
 * See COPYING in the top-level directory . */

#ifndef CACHE_FLUSH_H
#define CACHE_FLUSH_H

#define _GNU_SOURCE
#include "mpi.h"
#include <mpi.h>
#include <getopt.h>
#include <string.h>
#include <stdio.h>
#include <unistd.h>
#include <stdio.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <sys/wait.h>
#include <fcntl.h>
#include <assert.h>
#include <errno.h>
#include <stdlib.h>
#include <sys/time.h>
#include <unistd.h>
#include <limits.h>
#include <math.h>
#include "mpi.h"

void cache_flush_all(int myid,
                     int numprocs,
                     int size,
                     char *filename);
void cache_flush_ind_all(int myid,
			 int numprocs,
			 int size,
			 char *filename);
void cache_flush_ind(int myid,
		     int numprocs,
		     int size,
		     char *filename);
void cache_flush_ind_all_remove_files(int myid,
				      int numprocs,
				      char *filename);
#endif

/*
 * Local variables:
 *  mode: c
 *  c-indent-level: 4
 *  c-basic-offset: 4
 * End:
 *
 * vim: ft=c ts=8 sts=4 sw=4 expandtab
 */
