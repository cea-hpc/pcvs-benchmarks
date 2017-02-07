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
#include "mpc.h"
#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <assert.h>

int is_printing = 1;

#define mprintf if(is_printing) fprintf

void
run (void *arg)
{
  MPC_Comm my_com;
  MPC_Request req, req2;
  int my_rank;
  int my_size;
  char msg[50];

  my_com = MPC_COMM_WORLD;
  MPC_Comm_rank (my_com, &my_rank);
  MPC_Comm_size (my_com, &my_size);



  sprintf (msg, "nothing");

  if (my_rank == 0)
    {
      sprintf (msg, "it works");
      mprintf (stderr, "from %d to %d\n", 0, (my_size - 1));
      mprintf (stderr, "init msg = %s\n", msg);
      MPC_Isend (msg, 40, MPC_CHAR, (my_size - 1), 0, my_com, &req);
      mprintf (stderr, "init msg = %s %p\n", msg, msg);
      MPC_Wait_pending (my_com);
      mprintf (stderr, "init msg wait done = %s done \n", msg);
    }
  else
    {
      sleep(5);
      if (my_rank == (my_size - 1))
	{
	  mprintf (stderr, "recv %d to %d\n", 0, (my_size - 1));
	  mprintf (stderr, "msg = %s\n", msg);
	  MPC_Irecv (msg, 40, MPC_CHAR, 0, 0, my_com, &req);
	  MPC_Wait_pending (my_com);
	  mprintf (stderr, "msg recv done = %s\n", msg);
	  assert (strcmp (msg, "it works") == 0);
	}
    }

  sprintf (msg, "nothing");

  if (my_rank == 0)
    {
      sprintf (msg, "it works2");
      mprintf (stderr, "from %d to %d 2\n", 0, (my_size - 1));
      mprintf (stderr, "init msg = %s\n", msg);
      MPC_Isend (msg, 40, MPC_CHAR, (my_size - 1), 1, my_com, &req2);
      mprintf (stderr, "init msg = %s\n", msg);
      MPC_Wait_pending (my_com);
      mprintf (stderr, "init msg wait done 2 = %s done \n", msg);
    }
  else
    {
      if (my_rank == (my_size - 1))
	{
	  mprintf (stderr, "recv %d to %d 2\n", 0, (my_size - 1));
	  mprintf (stderr, "msg = %s\n", msg);
	  MPC_Irecv (msg, 40, MPC_CHAR, 0, 1, my_com, &req2);
	  MPC_Wait_pending (my_com);
	  mprintf (stderr, "msg recv done = %s\n", msg);
	  assert (strcmp (msg, "it works2") == 0);
	}
    }
  mprintf (stderr, "ALL DONE RUN %d\n",my_rank);
}

int
main (int argc, char **argv)
{
  MPI_Init(&argc, &argv);
  char *printing;

  printing = getenv ("MPC_TEST_SILENCE");
  if (printing != NULL)
    is_printing = 0;

  run (NULL);
  mprintf (stderr, "ALL DONE\n");
  MPI_Finalize();
  return 0;
}
