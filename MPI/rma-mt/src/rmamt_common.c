/* -*- Mode: C; c-basic-offset:4 ; -*- */
/*
 *   Copyright (C) 2007      University of Chicago
 *   Copyright (c) 2016-2018 Los Alamos National Security, LLC. All rights
 *                           reserved.
 * ****** SANDIA ADD YOUR COPYRIGHTS BEFORE RELEASE ******
 *   See COPYRIGHT notice in top-level directory.
 */

#include "rmamt_common.h"

#include <stdlib.h>
#include <stdio.h>

#include <unistd.h>
#include <sys/mman.h>
#include <fcntl.h>

void *rmamt_malloc (size_t size) {
    char *tmp_file;
    int fd = -1, ret;
    void *ptr;

    size = (size + 4095) & ~4095;

    if (0 == access ("/var/lib/hugetlbfs/global/pagesize-2097152", R_OK)) {
	ret = asprintf (&tmp_file, "/var/lib/hugetlbfs/global/pagesize-2097152/rmamt.XXXXXX");
	if (0 > ret) {
	    return NULL;
	}

	mkstemp (tmp_file);

	truncate (tmp_file, size);
	fd = open (tmp_file, O_RDWR, 0600);
	if (-1 == fd) {
	    free (tmp_file);
	    return NULL;
	}
    }

    ptr = mmap (NULL, size, PROT_READ | PROT_WRITE, MAP_ANONYMOUS | MAP_PRIVATE, fd, 0);

    if (-1 != fd) {
	close (fd);
	unlink (tmp_file);
	free (tmp_file);
    }

    return MAP_FAILED == ptr ? NULL : ptr;
}

void rmamt_free (void *ptr, size_t size) {
  munmap (ptr, size);
}

#if defined(HAVE_LIBHWLOC)
#include <hwloc.h>

hwloc_topology_t topology;

int shared_comm_size, shared_comm_rank;

int rmamt_bind_init (void) {
    MPI_Comm shared_comm;

    hwloc_topology_init (&topology);
    hwloc_topology_load (topology);

    /* deterimine how many local ranks there are */
    MPI_Comm_split_type (MPI_COMM_WORLD, MPI_COMM_TYPE_SHARED, 0, MPI_INFO_NULL, &shared_comm);
    MPI_Comm_size (shared_comm, &shared_comm_size);
    MPI_Comm_rank (shared_comm, &shared_comm_rank);

    MPI_Comm_free (&shared_comm);

    return 0;
}

void rmamt_bind (int thread_id) {
    int ncores = hwloc_get_nbobjs_by_type (topology, HWLOC_OBJ_CORE);
    int cores_per_rank = ncores < shared_comm_size ? 1 : ncores / shared_comm_size;
    int core_base = (cores_per_rank * shared_comm_rank) % ncores;
    int obj_id = core_base + thread_id % cores_per_rank;
    hwloc_obj_t obj = hwloc_get_obj_by_type (topology, HWLOC_OBJ_CORE, obj_id);

    if (NULL != obj) {
	hwloc_set_cpubind (topology, obj->cpuset, HWLOC_CPUBIND_THREAD | HWLOC_CPUBIND_STRICT);
    } else {
	fprintf (stderr, "WARNING: could not bind thread\n");
    }
}

void rmamt_bind_finalize (void) {
    hwloc_topology_destroy (topology);
}

#else

int rmamt_bind_init (void) {
    return -1;
}

void rmamt_bind (int thread_id) {
}

void rmamt_bind_finalize (void) {
}

#endif

