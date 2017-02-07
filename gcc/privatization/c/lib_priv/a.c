#include <stdio.h>
#include <mpi.h>

extern int a;
int foo(int a);


int main(int argc, char ** argv )
{

	MPI_Init( &argc, &argv );
	int rank; 
	MPI_Comm_rank(MPI_COMM_WORLD,&rank);
	a = rank;
        printf("MAIN: &a = %p\n", &a);
        int ret = 0;
        if (foo(rank)) {
          printf("ERROR address mismatch with Library\n");
          ret = 1;
        } else {
          printf("CHECK OK\n");
        }

        MPI_Finalize();

        return ret;
}
