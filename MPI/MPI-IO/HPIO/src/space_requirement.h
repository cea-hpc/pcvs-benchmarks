/* (C) Northwestern University
 * See COPYING in the top-level directory . */

#ifndef SPACE_REQUIREMENTS_H
#define SPACE_REQUIREMENTS_H

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

extern char *test_type_name[MAX_TEST];
extern const int params_count[MAX_TEST];
extern const int region_count_arr[MAX_COUNT_COUNT];
extern const int region_size_arr[MAX_SIZE_COUNT];
extern const int region_spacing_arr[MAX_SPACING_COUNT];
extern const int *params_arr_p[MAX_TEST];

int estimate_space(struct test_params_s *test_params_p);

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
