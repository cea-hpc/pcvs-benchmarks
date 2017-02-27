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

void *
run (void *arg)
{
  assert (omp_in_parallel ());
  assert (omp_get_num_threads () <= 6);
  fprintf (stderr, "BARRIER 1\n");
  __mpcomp_barrier ();
  fprintf (stderr, "BARRIER 2\n");
  __mpcomp_barrier ();
  return NULL;
}

int
main (int argc, char **argv)
{

  assert (omp_get_thread_num () == 0);
  assert (omp_get_num_threads () == 1);
  assert (omp_in_parallel () == 0);

  __mpcomp_barrier ();

  fprintf (stderr, "I am %d on %d max %d cpus %d\n",
	   omp_get_thread_num (),
	   omp_get_num_threads (),
	   omp_get_max_threads (), omp_get_num_procs ());

  {
    int shared;
    __mpcomp_start_parallel_region(run, (void *)&shared, 6);
  }
  fprintf (stderr, "ALL DONE\n");
  return 0;
}
