/* ############################# MPC License ############################## */
/* # Wed Nov 19 15:19:19 CET 2008                                         # */
/* # Copyright or (C) or Copr. Commissariat a l'Energie Atomique          # */
/* #                                                                      # */
/* # IDDN.FR.001.230040.000.S.P.2007.000.10000                            # */
/* # This file is part of the MPC Runtime.                                # */
/* #                                                                      # */
/* # This software is governed by the CeCILL-C license under French law   # */
/* # and abiding by the rules of distribution of free software.  You can  # */
/* # use, modify and/ or redistribute the software under the terms of     # */
/* # the CeCILL-C license as circulated by CEA, CNRS and INRIA at the     # */
/* # following URL http://www.cecill.info.                                # */
/* #                                                                      # */
/* # The fact that you are presently reading this means that you have     # */
/* # had knowledge of the CeCILL-C license and that you accept its        # */
/* # terms.                                                               # */
/* #                                                                      # */
/* # Authors:                                                             # */
/* #   - PERACHE Marc marc.perache@cea.fr                                 # */
/* #   - CARRIBAULT Patrick patrick.carribault@cea.fr                     # */
/* #                                                                      # */
/* ######################################################################## */
#include <stdio.h>
#include <mpcomp_abi.h>
#include <assert.h>

int num_threads;

void *
run2 (void *arg)
{
  assert (omp_in_parallel ());
  fprintf (stderr, "\tI am %d on %d = %d\n",
	   omp_get_thread_num (), omp_get_num_threads (),
	   omp_get_num_procs ());
  return NULL;
}

void *
run (void *arg)
{
  assert (omp_in_parallel ());
  assert (omp_get_num_threads () == num_threads);
  fprintf (stderr, "I am %d on %d = %d\n", omp_get_thread_num (),
	   omp_get_num_threads (), omp_get_num_procs ());

  {
    int shared;
    __mpcomp_start_parallel_region(run2, (void *)&shared, 0);
  }
  return NULL;
}

int
main (int argc, char **argv)
{
  num_threads = 2;

  omp_set_num_threads (num_threads);

  assert (omp_get_thread_num () == 0);
  assert (omp_get_num_threads () == 1);
  assert (omp_in_parallel () == 0);

  fprintf (stderr, "(S) I am %d on %d max %d cpus %d\n",
	   omp_get_thread_num (),
	   omp_get_num_threads (),
	   omp_get_max_threads (), omp_get_num_procs ());

  {
    int shared;
    __mpcomp_start_parallel_region(run, (void *)&shared, 0);
  }
  return 0;
}
