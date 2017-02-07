#include "mpi.h"
#include <stdio.h>
#include <stdlib.h>
 
int main(int argc, char *argv[])
{
    int myrank, rank, color;
    MPI_Status status;
    MPI_Comm commsplit, intercomm;
    MPI_Datatype datatype;
    int *sendbuf = 0, *recvbuf = 0;
    int errs = 0, err;
    int i, count, rsize;
    MPI_Init(&argc, &argv);
	
	MPI_Comm_rank( MPI_COMM_WORLD, &myrank );
	color = myrank % 2;
	MPI_Comm_split(MPI_COMM_WORLD, color, myrank, &commsplit);
	MPI_Intercomm_create( commsplit, 0, MPI_COMM_WORLD, 1-color, 52, &intercomm);
	
	datatype = MPI_INT;
	MPI_Comm_rank( intercomm, &rank );
	MPI_Comm_remote_size( intercomm, &rsize );
	
	if(myrank == 0)
	fprintf(stderr,"Lancement test Reduce ...\n");
	for (count = 1; count < 65000; count = 2 * count) 
	{
	    sendbuf = (int *)malloc( count * sizeof(int) );
	    recvbuf = (int *)malloc( count * sizeof(int) );
	    
	    for (i=0; i<count; i++) 
	    {
			sendbuf[i] = -1;
			recvbuf[i] = -1;
	    }
	    if (color == 0) 
	    {
			err = MPI_Reduce( sendbuf, recvbuf, count, datatype, MPI_SUM, (rank == 0) ? MPI_ROOT : MPI_PROC_NULL, intercomm );
			if (err)
				errs++;

			if (rank == 0) 
			{
				for (i=0; i<count; i++)
					if (recvbuf[i] != i * rsize)
						errs++;
			}
			else 
			{
				for (i=0; i<count; i++) 
					if (recvbuf[i] != -1) 
						errs++;
			}
	    }
	    else 
	    {
			for (i=0; i<count; i++) 
				sendbuf[i] = i;
				
			err = MPI_Reduce( sendbuf, recvbuf, count, datatype, MPI_SUM, 0, intercomm );
			
			if (err)
				errs++;
			
			for (i=0; i<count; i++) 
				if (recvbuf[i] != -1) 
					errs++;
	    }
		free( sendbuf ); 
		free( recvbuf );
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
