#include <assert.h>
#include <stdio.h>
#include <stdlib.h>

#include <mpi.h>

#include <unistd.h>

int main(int argc, char *argv[])
{
    int err;

    MPI_Init(&argc, &argv);

    int nb_procs;
    MPI_Comm_size(MPI_COMM_WORLD, &nb_procs);

    if(nb_procs != 4){
      MPI_Finalize();
      return 0;
    }



    int my_rank;
    MPI_Comm_rank(MPI_COMM_WORLD, &my_rank);

    int my_row = my_rank / 2;
    int my_col = my_rank % 2;

    MPI_Comm row_comm;
    MPI_Comm_split(MPI_COMM_WORLD, my_row, my_col, &row_comm);

    MPI_Comm col_comm;
    MPI_Comm_split(MPI_COMM_WORLD, my_col, my_row, &col_comm);

    {
        char c;

        if (my_rank == 1)
        {
            err = MPI_Bcast(&c, 1, MPI_CHAR, 1, col_comm);
            assert(err == MPI_SUCCESS);
        }
        else if (my_rank == 2)
        {
            sleep(1);

            err = MPI_Bcast(&c, 1, MPI_CHAR, 0, row_comm);
            assert(err == MPI_SUCCESS);
        }
        else if (my_rank == 3)
        {
            sleep(1);

            err = MPI_Bcast(&c, 1, MPI_CHAR, 0, row_comm);
            assert(err == MPI_SUCCESS);

            err = MPI_Bcast(&c, 1, MPI_CHAR, 1, col_comm);
            assert(err == MPI_SUCCESS);
        }
    }

    MPI_Comm_free(&col_comm);
    MPI_Comm_free(&row_comm);

    MPI_Finalize();

    return EXIT_SUCCESS;
}
