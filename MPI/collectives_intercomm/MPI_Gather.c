#include "mpi.h"
#include <stdio.h>
 
int main(int argc, char *argv[])
{
    int myrank, rank, color;
    MPI_Status status;
    MPI_Comm commsplit, intercomm;
    MPI_Datatype datatype;
    int errs = 0, err;
    int *buf = 0;
    int i, count, rsize;
    MPI_Init(&argc, &argv);
	
	MPI_Comm_rank( MPI_COMM_WORLD, &myrank );
	color = myrank % 2;
	MPI_Comm_split(MPI_COMM_WORLD, color, myrank, &commsplit);
	MPI_Intercomm_create( commsplit, 0, MPI_COMM_WORLD, 1-color, 52, &intercomm);
	
	MPI_Comm_rank( intercomm, &rank );
	MPI_Comm_remote_size( intercomm, &rsize );
	datatype = MPI_INT;
	
	if(myrank == 0)
	fprintf(stderr,"Lancement test Gather ...\n");
	for (count = 1; count < 65000; count = 2 * count) 
	{
	    if (color == 0) 
	    {
			buf = (int *)malloc( count * rsize * sizeof(int) );
			for (i=0; i<count*rsize; i++) 
				buf[i] = -1;

			err = MPI_Gather( NULL, 0, datatype, buf, count, datatype, (rank == 0) ? MPI_ROOT : MPI_PROC_NULL, intercomm );
			
			if (err)
				errs++;
				
			if (rank != 0) 
			{
				for (i=0; i<count; i++)
					if (buf[i] != -1)
						errs++;
			}
			else 
			{
				for (i=0; i<count*rsize; i++) 
					if (buf[i] != i) 
						errs++;
			}
	    }
	    else 
	    {
			buf = (int *)malloc( count * sizeof(int) );
			for (i=0; i<count; i++) 
				buf[i] = rank * count + i;
			
			err = MPI_Gather( buf, count, datatype, NULL, 0, datatype, 0, intercomm );
			
			if (err)
				errs++;
	    }
		free( buf );
	}
	if(errs)
	{
		if(myrank == 0)
		fprintf(stderr,"erreurs detectees\n");
		exit(1);
	}
	else
	{
		if(myrank == 0)
		fprintf(stderr,"aucune erreur detectee\n");
	}
	MPI_Finalize();
    return 0;
}
