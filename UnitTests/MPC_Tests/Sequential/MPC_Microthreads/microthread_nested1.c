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
#include <mpc.h>

void *
run2 (void *arg)
{
  printf ("Hello from depth 2\n");
  return NULL;
}

void *
run (void *arg)
{

  sctk_microthread_t s;
  int i;

  printf ("Hello from depth 1\n");

  sctk_microthread_init (3, &s);

  for (i = 0; i < 3; i++)
    {
      long step;
      sctk_microthread_add_task (run2, NULL, i, &step, &s,
				 MPC_MICROTHREAD_NO_TASK_INFO);
    }

  sctk_microthread_parallel_exec (&s, MPC_MICROTHREAD_WAKE_VPS);

  printf ("Hello from depth 1 (after)\n");
  return NULL;
}

int
main (int argc, char **argv)
{

  sctk_microthread_t s;
  int i;

  sctk_microthread_init (6, &s);

  for (i = 0; i < 6; i++)
    {
      long step;
      sctk_microthread_add_task (run, NULL, i, &step, &s,
				 MPC_MICROTHREAD_NO_TASK_INFO);
    }

  sctk_microthread_parallel_exec (&s, MPC_MICROTHREAD_WAKE_VPS);

  return 0;
}
