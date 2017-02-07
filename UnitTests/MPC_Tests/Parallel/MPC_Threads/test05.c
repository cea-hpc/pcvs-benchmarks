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
#include <stdlib.h>
#include <mpcthread.h>

void *functionC ();
mpc_thread_mutex_t mutex1 = MPC_THREAD_MUTEX_INITIALIZER;
int counter = 0;

int
main (int argc, char **argv)
{
  int rc1, rc2;
  mpc_thread_t thread1, thread2;

  /* Create independent threads each of which will execute functionC */

  if ((rc1 = mpc_thread_create (&thread1, NULL, &functionC, NULL)))
    {
      printf ("Thread creation failed: %d\n", rc1);
    }

  if ((rc2 = mpc_thread_create (&thread2, NULL, &functionC, NULL)))
    {
      printf ("Thread creation failed: %d\n", rc2);
    }

  /* Wait till threads are complete before main continues. Unless we  */
  /* wait we run the risk of executing an exit which will terminate   */
  /* the process and all threads before the threads have completed.   */

  mpc_thread_join (thread1, NULL);
  mpc_thread_join (thread2, NULL);

  return 0;
}

void *
functionC ()
{
  mpc_thread_mutex_lock (&mutex1);
  counter++;
  printf ("Counter value: %d\n", counter);
  mpc_thread_mutex_unlock (&mutex1);
  return NULL;
}
