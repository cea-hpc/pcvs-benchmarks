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
/* #                                                                      # */
/* ######################################################################## */
#include <stdio.h>
#include <mpcthread.h>

#define NTHREADS 4
void *thread_function ();
mpc_thread_mutex_t mutex1 = MPC_THREAD_MUTEX_INITIALIZER;
int counter = 0;

int
main (int argc, char **argv)
{
  mpc_thread_t thread_id[NTHREADS];
  int i, j;

  for (i = 0; i < NTHREADS; i++)
    {
      mpc_thread_create (&thread_id[i], NULL, &thread_function, NULL);
    }

  for (j = 0; j < NTHREADS; j++)
    {
      mpc_thread_join (thread_id[j], NULL);
    }

  /* Now that all threads are complete I can print the final result.     */
  /* Without the join I could be printing a value before all the threads */
  /* have been completed.                                                */

  printf ("Final counter value: %d\n", counter);
  return 0;
}

void *
thread_function ()
{
  printf ("Thread number %ld\n", mpc_thread_self ());
  mpc_thread_mutex_lock (&mutex1);
  counter++;
  mpc_thread_mutex_unlock (&mutex1);
  return NULL;
}
