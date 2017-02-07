#include "mpi.h"
#include <stdio.h>
 
int main(int argc, char *argv[])
{
    int myrank, rank, color;
    MPI_Status status;
    MPI_Comm commsplit, intercomm;
    MPI_Datatype datatype;
    int errs = 0, err;
    int *rbuf = 0, *sbuf = 0;
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
	fprintf(stderr,"Lancement test Allgather ...\n");
	for (count = 1; count < 65000; count = 2 * count) 
	{
	    rbuf = (int *)malloc( count * rsize * sizeof(int) );
	    sbuf = (int *)malloc( count * sizeof(int) );
	    
	    for (i=0; i<count*rsize; i++) 
			rbuf[i] = -1;
			
	    if (color == 0) 
			for (i=0; i<count; i++)       
				sbuf[i] = i + rank*count;
	    else
			for (i=0; i<count; i++)       
				sbuf[i] = -(i + rank*count);
		
	    err = MPI_Allgather( sbuf, count, datatype, rbuf, count, datatype, intercomm );
	    
	    if (err)
			errs++;

	    if (color == 0) 
	    {
			for (i=0; i<count*rsize; i++) 
				if (rbuf[i] != -i) 
					errs++;
	    }
	    else 
	    {
			for (i=0; i<count*rsize; i++)
				if (rbuf[i] != i) 
					errs++;
	    }

	    for (i=0; i<count*rsize; i++) 
			rbuf[i] = -1;
			
	    if (color == 0) 
	    {
			err = MPI_Allgather( sbuf, 0, datatype, rbuf, count, datatype, intercomm );
			if (err) 
				errs++;
				
			for (i=0; i<count*rsize; i++) 
				if (rbuf[i] != -i) 
				errs++;
	    }
	    else 
	    {
			err = MPI_Allgather( sbuf, count, datatype, rbuf, 0, datatype, intercomm );
			if (err)
				errs++;
	
			for (i=0; i<count*rsize; i++) 
				if (rbuf[i] != -1) 
					errs++;
	    }
	    free( rbuf );
	    free( sbuf );
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
