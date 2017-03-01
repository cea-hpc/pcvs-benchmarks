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
 *      harness.h
 *
 * DESCRIPTION
 *      Common routines for futex_wait and futex_set_wait performance tests.
 *
 * AUTHOR
 *      Michel Lespinasse <walken@google.com>
 *
 * HISTORY
 *      2009-Nov-16: Initial version by Michel Lespinasse <walken@google.com>
 *      2009-Nov-30: Removal of hard-coded thread count array and general
 *                   integration into futextest by
 *                   Darren Hart <dvhltc@us.ibm.com>
 *
 *****************************************************************************/

#include <errno.h>
#include <limits.h>
#include <pthread.h>
#include <stdio.h>
#include <sys/times.h>
#include "logging.h"


struct thread_barrier {
	futex_t threads;
	futex_t unblock;
};

struct locktest_shared {
	struct thread_barrier barrier_before;
	struct thread_barrier barrier_after;
	void (* locktest_function)(futex_t *ptr, int loops);
	int loops;
	futex_t futex;
};

/* Called by main thread to initialize barrier */
static void barrier_init(struct thread_barrier *barrier, int threads)
{
	barrier->threads = threads;
	barrier->unblock = 0;
}

/* Called by worker threads to synchronize with main thread */
static int barrier_sync(struct thread_barrier *barrier)
{
	futex_dec(&barrier->threads);
	if (barrier->threads == 0)
		futex_wake(&barrier->threads, 1, FUTEX_PRIVATE_FLAG);
	while (barrier->unblock == 0)
		futex_wait(&barrier->unblock, 0, NULL, FUTEX_PRIVATE_FLAG);
	return barrier->unblock;
}

/* Called by main thread to wait for all workers to reach sync point */
static void barrier_wait(struct thread_barrier *barrier)
{
	int threads;
	while ((threads = barrier->threads) > 0)
		futex_wait(&barrier->threads, threads, NULL,
			   FUTEX_PRIVATE_FLAG);
}

/* Called by main thread to unblock worker threads from their sync point */
static void barrier_unblock(struct thread_barrier *barrier, int value)
{
	barrier->unblock = value;
	futex_wake(&barrier->unblock, INT_MAX, FUTEX_PRIVATE_FLAG);
}

static void * locktest_thread(void * dummy)
{
	struct locktest_shared * shared = dummy;
	if (barrier_sync(&shared->barrier_before) > 0) {
		shared->locktest_function(&shared->futex, shared->loops);
		barrier_sync(&shared->barrier_after);
	}
	return NULL;
}

static int locktest(void locktest_function(futex_t * ptr, int loops),
		     int iterations, int threads)
{
	struct locktest_shared shared;
	pthread_t thread[threads];
	int i;
	clock_t before, after;
	struct tms tms_before, tms_after;
	int wall, user, system;
	double tick;

	barrier_init(&shared.barrier_before, threads);
	barrier_init(&shared.barrier_after, threads);
	shared.locktest_function = locktest_function;
	shared.loops = iterations / threads;
	shared.futex = 0;

	for (i = 0; i < threads; i++)
		if (pthread_create(thread + i, NULL, locktest_thread,
					&shared)) {
			error("pthread_create\n", errno);
			/* Could not create thread; abort */
			barrier_unblock(&shared.barrier_before, -1);
			while (--i >= 0)
				pthread_join(thread[i], NULL);
			print_result(RET_ERROR);
			return RET_ERROR;
		}
	barrier_wait(&shared.barrier_before);
	before = times(&tms_before);
	barrier_unblock(&shared.barrier_before, 1);
	barrier_wait(&shared.barrier_after);
	after = times(&tms_after);
	wall = after - before;
	user = tms_after.tms_utime - tms_before.tms_utime;
	system = tms_after.tms_stime - tms_before.tms_stime;
	tick = 1.0 / sysconf(_SC_CLK_TCK);
	info("%.2fs user, %.2fs system, %.2fs wall, %.2f cores\n",
	     user * tick, system * tick, wall * tick,
	     wall ? (user + system) * 1. / wall : 1.);
	barrier_unblock(&shared.barrier_after, 1);
	for (i = 0; i < threads; i++)
		pthread_join(thread[i], NULL);

	printf("Result: %.0f Kiter/s\n",
	       (threads * shared.loops) / (wall * tick * 1000));

	return RET_PASS;
}
