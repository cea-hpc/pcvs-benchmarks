/******************************************************************************
 *
 *   Copyright 2009 Google Inc.
 *   Copyright © International Business Machines  Corp., 2009
 *
 *   This program is free software;  you can redistribute it and/or modify
 *   it under the terms of the GNU General Public License as published by
 *   the Free Software Foundation; either version 2 of the License, or
 *   (at your option) any later version.
 *
 *   This program is distributed in the hope that it will be useful,
 *   but WITHOUT ANY WARRANTY;  without even the implied warranty of
 *   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See
 *   the GNU General Public License for more details.
 *
 *   You should have received a copy of the GNU General Public License
 *   along with this program;  if not, write to the Free Software
 *   Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307 USA
 *
 * NAME
 *      futex_wait.c
 *
 * DESCRIPTION
 *      Measure FUTEX_WAIT operations per second over a configurable number
 *      of iterations and treads.
 *
 * AUTHOR
 *      Michel Lespinasse <walken@google.com>
 *
 * HISTORY
 *      2009-Nov-16: Initial version by Michel Lespinasse <walken@google.com>
 *      2009-Nov-30: Optional thread and iteration parameters, general
 *                   integration into futextest by 
 *                   Darren Hart <dvhltc@us.ibm.com>
 *
 *****************************************************************************/

#include <getopt.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "futextest.h"
#include "logging.h"
#include "harness.h"


static int threads = 256;
static int iterations = 100000000;

void usage(char *prog)
{
	printf("Usage: %s\n", prog);
	printf("  -c	Use color\n");
	printf("  -h	Display this help message\n");
	printf("  -i I	Number of iterations (default: %d)\n", iterations);
	printf("  -n N	Number of threads (default: %d)\n", threads);
	printf("  -v L	Verbosity level: %d=QUIET %d=CRITICAL %d=INFO\n",
	       VQUIET, VCRITICAL, VINFO);
}

static inline void futex_wait_lock(futex_t *futex)
{
	int status = *futex;
	if (status == 0)
		status = futex_cmpxchg(futex, 0, 1);
	while (status != 0) {
		if (status == 1)
			status = futex_cmpxchg(futex, 1, 2);
		if (status != 0) {
			futex_wait(futex, 2, NULL, FUTEX_PRIVATE_FLAG);
			status = *futex;
		}
		if (status == 0)
			status = futex_cmpxchg(futex, 0, 2);
	}
}

static inline void futex_cmpxchg_unlock(futex_t *futex)
{
	int status = *futex;
	if (status == 1)
		status = futex_cmpxchg(futex, 1, 0);
	if (status == 2) {
		futex_cmpxchg(futex, 2, 0);
		futex_wake(futex, 1, FUTEX_PRIVATE_FLAG);
	}
}

static void futex_wait_test(futex_t *futex, int loops)
{
	while (loops--) {
		futex_wait_lock(futex);
		futex_cmpxchg_unlock(futex);
	}
}

int main(int argc, char *argv[])
{
	int ret, c;
	while ((c = getopt(argc, argv, "chi:n:v:")) != -1) {
		switch(c) {
		case 'c':
			log_color(1);
			break;
		case 'h':
			usage(basename(argv[0]));
			exit(0);
		case 'i':
			iterations = atoi(optarg);
			break;
		case 'n':
			threads = atoi(optarg);
			break;
		case 'v':
			log_verbosity(atoi(optarg));
			break;
		default:
			usage(basename(argv[0]));
			exit(1);
		}
	}

	printf("%s: Measure FUTEX_WAIT operations per second\n",
	       basename(argv[0]));
	printf("\tArguments: iterations=%d threads=%d\n", iterations, threads);

	/* run the test and display the results */
	ret = locktest(futex_wait_test, iterations, threads);

	return ret;
}
