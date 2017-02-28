#include "mpi.h"
#include <stdio.h>
 
int main(int argc, char *argv[])
{
    int myrank, rank, color, i, count;
    int errs = 0, err;
    MPI_Status status;
    MPI_Comm commsplit, intercomm;
    MPI_Datatype datatype;
    int *buf;
    MPI_Init(&argc, &argv);
	
	MPI_Comm_rank( MPI_COMM_WORLD, &myrank );
	color = myrank % 2;
	MPI_Comm_split(MPI_COMM_WORLD, color, myrank, &commsplit);
	MPI_Intercomm_create( commsplit, 0, MPI_COMM_WORLD, 1-color, 52, &intercomm);
	MPI_Comm_rank( intercomm, &rank );
	datatype = MPI_INT;
	
	if(myrank == 0)
	fprintf(stderr,"Lancement test Bcast ...\n");
	for (count = 1; count < 65000; count = 2 * count) 
	{
	    buf = (int *)malloc( count * sizeof(int) );
	    if (color == 0) 
	    {
			if (rank == 0)
				for (i=0; i<count; i++) 
					buf[i] = i;
			else
				for (i=0; i<count; i++) 
					buf[i] = -1;
				
			err = MPI_Bcast( buf, count, datatype, (rank == 0) ? MPI_ROOT : MPI_PROC_NULL, intercomm );
			if (err)
				errs++;
				
			if (rank != 0) 
			{
				for (i=0; i<count; i++) 
				{
					if (buf[i] != -1)
						errs++;
				}
			}
	    }
	    else 
	    {
			for (i=0; i<count; i++) 
				buf[i] = -1;
			
			err = MPI_Bcast( buf, count, datatype, 0, intercomm );
			
			if (err)
				errs++;

			for (i=0; i<count; i++) 
			{
				if (buf[i] != i)
				errs++;
			}
	    }
		free( buf );
	}
	
	if(errs)
	{
		fprintf(stderr, "erreurs detectées\n");
	}
	else
	{
		fprintf(stderr,"pas d'erreur detectee\n");
	}
	
    MPI_Finalize();
    return 0;
}
