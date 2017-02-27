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
  MPC_Comm my_com2;
  MPC_Request req,req2;
  int my_rank;
  int my_rank2;
  int dom_size;
  int *tab;
  char msg[50];
  char msg2[50];
  
  my_com = MPC_COMM_WORLD;
  MPC_Comm_rank (my_com, &my_rank);
  MPC_Comm_size (my_com, &dom_size);

  tab = (int *) malloc (dom_size * sizeof (int));

  for (my_rank = 0; my_rank < dom_size; my_rank++)
    {
      tab[my_rank] = my_rank;
    }

  sprintf (msg, "nothing");
  sprintf (msg2, "nothing");

  my_com = MPC_COMM_WORLD;
  MPC_Comm_rank (my_com, &my_rank);
  mprintf (stderr, "Avant creation %d\n", my_rank);
  MPC_Comm_create_list (my_com, tab, dom_size, &my_com2);
  MPC_Comm_rank (my_com2, &my_rank2);
  mprintf (stderr, "Apres creation %d %d\n", my_rank, my_rank2);

  if (my_rank == 0)
    {
      sprintf (msg, "it works 1");
      sprintf (msg2, "it works 2");
      MPC_Isend (msg, 40, MPC_CHAR, 1, 0, my_com, &req);
      MPC_Isend (msg2, 40, MPC_CHAR, 1, 0, my_com2, &req2);
      MPC_Wait_pending (my_com);
      MPC_Wait_pending (my_com2);
    }
  else
    {
      if (my_rank == 1)
	{
	  MPC_Irecv (msg, 40, MPC_CHAR, 0, 0, my_com, &req);
	  MPC_Irecv (msg2, 40, MPC_CHAR, 0, 0, my_com2, &req2);
	  MPC_Wait_pending (my_com);
	  MPC_Wait_pending (my_com2);
	  mprintf (stderr, "msg = %s\n", msg);
	  assert (strcmp (msg, "it works 1") == 0);
	  mprintf (stderr, "msg2 = %s\n", msg2);
	  assert (strcmp (msg2, "it works 2") == 0);
	}
    }


  mprintf (stderr, "Apres messages %d\n", my_rank);
  MPC_Comm_free (&my_com2);
  mprintf (stderr, "Apres destruction %d\n", my_rank);
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
  MPI_Finalize();
  return 0;
}
