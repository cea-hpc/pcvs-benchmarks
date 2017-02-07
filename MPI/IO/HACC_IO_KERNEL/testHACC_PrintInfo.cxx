#include <stdio.h>
#include <stdlib.h>
#include <errno.h>
#include <string.h>
#include <fcntl.h>
#include "mpi.h"
#include <iostream>

#include "RestartIO_GLEAN.h"

using namespace std;

int main (int argc, char * argv[]) 
{
	char* fname = 0;	
	char* buf = 0;
	int numtasks, myrank, status;
	MPI_File fh;

	status = MPI_Init(&argc, &argv);
    if ( MPI_SUCCESS != status)
    {
        printf(" Error Starting the MPI Program \n");
        MPI_Abort(MPI_COMM_WORLD, status);
    }

    MPI_Comm_size(MPI_COMM_WORLD, &numtasks);
    MPI_Comm_rank(MPI_COMM_WORLD, &myrank);

	RestartIO_GLEAN* rst = new RestartIO_GLEAN();
	
	rst->Initialize(MPI_COMM_WORLD);

	MPI_Barrier(MPI_COMM_WORLD);
    
	rst->PrintIOCoordInfo();

	rst->Finalize();

	delete rst;
	rst = 0;
    
	MPI_Finalize();

	return 0;

}



