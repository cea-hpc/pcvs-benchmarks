#include <assert.h>
#include <stdio.h>
#include <stdlib.h>

#include <sys/time.h>

#include <mpi.h>

#define TIME_DIFF(b, e) (e.tv_sec - b.tv_sec + (e.tv_usec - b .tv_usec) / 1000000.f)

#define SIZE_MAX (1024 * 1024)

int main(int argc, char *argv[])
{
    int my_rank, tab_size, i, err, size;
    double *tab, *tab2, time;
    struct timeval begin, end;

    MPI_Init(&argc, &argv);
    MPI_Comm_rank(MPI_COMM_WORLD, &my_rank);

    tab = malloc(SIZE_MAX * sizeof(double));
    assert(tab != NULL);

    tab2 = malloc(SIZE_MAX * sizeof(double));
    assert(tab2 != NULL);

    for (tab_size = 1024; tab_size <= SIZE_MAX; tab_size *= 2)
    {
        gettimeofday(&begin, NULL);

        for (i = 0; i < 100; ++i)
        {
            err = MPI_Allreduce(tab, tab2, tab_size, MPI_DOUBLE, MPI_SUM, MPI_COMM_WORLD);
            assert(err == MPI_SUCCESS);
        }

        gettimeofday(&end, NULL);

        time = TIME_DIFF(begin, end) / 1000;
        size = tab_size * sizeof(double);

        if (my_rank == 0)
            printf("%d %f\n", size, size / time);
    }

    free(tab2);
    free(tab);

    MPI_Finalize();

    return EXIT_SUCCESS;
}
