#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
#include <string.h>
#include <mpc.h>

int main(int argc, char ** argv) {
  int nb_tasks = -1, autokill = -1;
  int my_rank, my_size;
  int i = 0;

  MPC_Init(&argc, &argv);

  MPC_Comm_rank(MPC_COMM_WORLD, &my_rank);
  MPC_Comm_size(MPC_COMM_WORLD, &my_size);

  if (my_rank == 0) {
    for (i = 1; i < argc; i ++) {
      int idx = strstr(argv[i], "=") - argv[i];

      if (strcmp("--nb-tasks", strndup(argv[i], idx)) == 0) {
        nb_tasks = atoi(strdup(argv[i] + idx + 1));
      }
      else if (strcmp("--autokill", strndup(argv[i], idx)) == 0) {
        autokill = atoi(strdup(argv[i] + idx + 1));
      }
      else {
        fprintf(stderr, "Unknown argument %s.\n", argv[i]);
        MPC_Abort(MPC_COMM_WORLD, MPC_ERR_UNKNOWN);
      }
    }
  }

  MPC_Barrier(MPC_COMM_WORLD);

  if (my_rank == 0 && nb_tasks != -1 && nb_tasks != my_size) {
    fprintf(stderr, "Expected comm size is %d, and actual is %d.\n",
        nb_tasks, my_size);
    MPC_Abort(MPC_COMM_WORLD, MPC_ERR_UNKNOWN);
  }

  if (autokill != -1) {
    sleep(autokill * 2);
  }

  if (my_rank == 0 && autokill != -1) {
    fprintf(stderr, "Application should have be killed after %d seconds.\n", autokill);
    MPC_Abort(MPC_COMM_WORLD, MPC_ERR_UNKNOWN);
  }

  MPC_Finalize();

  return EXIT_SUCCESS;
}
