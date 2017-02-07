#include "mpi.h"
#include <stdio.h>
 
int main(int argc, char *argv[])
{
    int myrank, rank, color;
    int errs = 0, err;
    MPI_Status status;
    MPI_Comm commsplit, intercomm;
    MPI_Init(&argc, &argv);
	
	MPI_Comm_rank( MPI_COMM_WORLD, &myrank );
	color = myrank % 2;
	MPI_Comm_split(MPI_COMM_WORLD, color, myrank, &commsplit);
	MPI_Intercomm_create( commsplit, 0, MPI_COMM_WORLD, 1-color, 52, &intercomm);
	
	if(myrank == 0)
	fprintf(stderr,"Lancement test Barrier ...\n");
	if (color == 0) 
	{
	    err = MPI_Barrier( intercomm );
	    if(err)
			errs++;
	}
	else 
	{
	    err = MPI_Barrier( intercomm );
	    if(err)
			errs++;
	}
	
	if(errs)
	{
		if(myrank == 0)
		fprintf(stderr, "erreurs detectées\n");
	}
	else
	{
		if(myrank == 0)
		fprintf(stderr,"pas d'erreur detectee\n");
	}
	
    MPI_Finalize();
    return 0;
}
