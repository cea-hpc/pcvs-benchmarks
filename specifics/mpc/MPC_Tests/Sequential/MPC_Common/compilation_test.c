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
#include <mpc.h>
#include <assert.h>


int
main (int argc, char **argv)
{

  assert (MPC_check_compatibility () == 0);
  fprintf (stderr, "MODES:\n");
#if defined(MPC_Message_Passing) || defined(MPC_MODULE_MPC_Message_Passing)
  fprintf (stderr, "\t- MPC_Message_Passing\n");
#endif

#if defined(MPC_Threads) || defined(MPC_MODULE_MPC_Threads)
  fprintf (stderr, "\t- MPC_Threads\n");
#endif

#if defined(MPC_Allocator) || defined(MPC_MODULE_MPC_Allocator)
  fprintf (stderr, "\t- MPC_Allocator\n");
#endif


#if defined(MPC_Message_Passing) || defined(MPC_MODULE_MPC_Message_Passing)
  MPC_Init (&argc, &argv);
  MPC_Finalize ();
#endif

#if defined(MPC_Threads) || defined(MPC_MODULE_MPC_Threads)
#endif

#if defined(MPC_Allocator) || defined(MPC_MODULE_MPC_Allocator)
  free (malloc (4));
  free (malloc (1024 * 1024));
  free (malloc (4 * 1024 * 1024));
  malloc (64 * 1024 * 1024);
#endif
  return 0;
}
