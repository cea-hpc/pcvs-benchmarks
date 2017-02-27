#include <mpi.h>
#include <cstdlib>
#include <iostream>

using namespace std;

double *table;

#pragma hls node(table)

int main( int argc, char **argv )
{
	int rank, size;
	MPI_Status status;
	MPI_Request request;
	int i = 0;
	double *addr;
	MPI_Init( &argc, &argv );
	MPI_Comm_rank( MPI_COMM_WORLD, &rank );
	MPI_Comm_size( MPI_COMM_WORLD, &size );

	//node
	if(rank == 0)
	{
		for(i=1 ; i<size ; i++)
		{
			MPI_Recv(&addr, 1, MPI_DOUBLE, i, 0, MPI_COMM_WORLD, &status);

			if(addr != (double *)&table)
			{
				cout << "HLS NODE FAILURE\n" << endl;
				abort();
			}
		}
		cout << "HLS NODE SUCCESS\n" << endl;
	}
	else
	{
		double *pointeur = (double *)&table;
		MPI_Send(&pointeur, 1, MPI_DOUBLE, 0, 0, MPI_COMM_WORLD);
	}
	
	MPI_Finalize();
	return EXIT_SUCCESS;
}
