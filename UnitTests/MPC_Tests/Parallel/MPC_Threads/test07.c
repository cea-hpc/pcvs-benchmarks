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
#include <string.h>
#include <mpcthread.h>

void *print_message_function (void *ptr);

int
main (int argc, char **argv)
{
  mpc_thread_t thread1, thread2;
  mpc_thread_attr_t attr;
  char *message1 = "Thread 1";
  char *message2 = "Thread 2";
  int iret1, iret2;
  void *ptr;
  int scope;

  /* Create independent threads each of which will execute function */

  mpc_thread_attr_init (&attr);
  mpc_thread_attr_setscope (&attr, MPC_THREAD_SCOPE_SYSTEM);

  fprintf (stderr, "Start thread creation \n");

  iret1 =
    mpc_thread_create (&thread1, &attr, print_message_function,
		       (void *) message1);
  mpc_thread_attr_getscope (&attr, &scope);
  fprintf (stderr, "Thread 1 created %d\n", scope);

  iret2 =
    mpc_thread_create (&thread2, &attr, print_message_function,
		       (void *) message2);
  mpc_thread_attr_getscope (&attr, &scope);
  fprintf (stderr, "Thread 2 created %d\n", scope);

  /* Wait till threads are complete before main continues. Unless we  */
  /* wait we run the risk of executing an exit which will terminate   */
  /* the process and all threads before the threads have completed.   */

  mpc_thread_join (thread1, &ptr);
  printf ("Thread 1 returns: %d %p=%p\n", iret1, ptr, message1);

  mpc_thread_join (thread2, &ptr);
  printf ("Thread 2 returns: %d %p=%p\n", iret2, ptr, message2);

  /* Create independent threads each of which will execute function */

  mpc_thread_attr_init (&attr);
  mpc_thread_attr_setscope (&attr, MPC_THREAD_SCOPE_SYSTEM);

  fprintf (stderr, "Start thread creation \n");

  iret1 =
    mpc_thread_create (&thread1, &attr, print_message_function,
		       (void *) message1);
  mpc_thread_attr_getscope (&attr, &scope);
  fprintf (stderr, "Thread 1 created %d\n", scope);

  iret2 =
    mpc_thread_create (&thread2, &attr, print_message_function,
		       (void *) message2);
  mpc_thread_attr_getscope (&attr, &scope);
  fprintf (stderr, "Thread 2 created %d\n", scope);

  /* Wait till threads are complete before main continues. Unless we  */
  /* wait we run the risk of executing an exit which will terminate   */
  /* the process and all threads before the threads have completed.   */

  mpc_thread_join (thread1, &ptr);
  printf ("Thread 1 returns: %d %p=%p\n", iret1, ptr, message1);

  mpc_thread_join (thread2, &ptr);
  printf ("Thread 2 returns: %d %p=%p\n", iret2, ptr, message2);
  return 0;
}

void *
print_message_function (void *ptr)
{
  char *message;
  int s;
  message = (char *) ptr;
  printf ("%s \n", message);
  s = strlen (message) + 1;
  message = malloc (s);
  memcpy (message, ptr, s);
  printf ("%s \n", message);
  free (message);
  return ptr;
}
