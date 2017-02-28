#include "mpi.h"
#include <stdio.h>
 
int main(int argc, char *argv[])
{
    int myrank, rank, color;
    MPI_Status status;
    MPI_Comm commsplit, intercomm;
    MPI_Datatype datatype;
    int i, count, rsize, size;
    int errs = 0, err;
    int *buf = 0;
    int *sendcounts;
    int *senddispls;
    MPI_Init(&argc, &argv);
	
	MPI_Comm_rank( MPI_COMM_WORLD, &myrank );
	color = myrank % 2;
	MPI_Comm_split(MPI_COMM_WORLD, color, myrank, &commsplit);
	MPI_Intercomm_create( commsplit, 0, MPI_COMM_WORLD, 1-color, 52, &intercomm);
	
	MPI_Comm_rank( intercomm, &rank );
	MPI_Comm_size( intercomm, &size );
	MPI_Comm_remote_size( intercomm, &rsize );
	datatype = MPI_INT;
	
	if(myrank == 0)
	fprintf(stderr,"Lancement test Scatter ...\n");
	for (count = 1; count < 65000; count = 2 * count) 
	{
	    buf = 0;
	    sendcounts = (int *)malloc( rsize * sizeof(int) );
	    senddispls = (int *)malloc( rsize * sizeof(int) );
	    
	    for (i=0; i<rsize; i++) 
	    {
			sendcounts[i] = count;
			senddispls[i] = count * i;
	    }
	    if (color == 0) 
	    {
			buf = (int *)malloc( count * rsize * sizeof(int) );
			
			if (rank == 0) 
				for (i=0; i<count*rsize; i++) 
					buf[i] = i;
			else 
				for (i=0; i<count*rsize; i++) 
					buf[i] = -1;
			
			err = MPI_Scatterv( buf, sendcounts, senddispls, datatype, NULL, 0, datatype, (rank == 0) ? MPI_ROOT : MPI_PROC_NULL, intercomm );
			
			if (err)
				errs++;
				
			if (rank != 0) 
			{
				for (i=0; i<count*rsize; i++) 
				{
					if (buf[i] != -1) 
					{
						if (errs < 10) 
							fprintf( stderr, "Received data on root group!\n" );
						errs++;
					}
				}
			}
	    }
	    else 
	    {
			buf = (int *)malloc( count * sizeof(int) );

			for (i=0; i<count; i++) 
				buf[i] = -1;
				
			err = MPI_Scatterv( NULL, 0, 0, datatype, buf, count, datatype, 0, intercomm );
			
			if (err)
				errs++;

			for (i=0; i<count; i++) 
			{
				if (buf[i] != i + rank * count) 
				{
					if (errs < 10) 
						fprintf( stderr, "buf[%d] = %d on %d\n", i, buf[i], rank );
					errs++;
				}
			}
	    }
	    free( sendcounts );
	    free( senddispls );
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
