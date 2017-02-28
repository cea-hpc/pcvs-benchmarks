#include "mpi.h"
#include <stdio.h>
#include <stdlib.h>

int main( int argc, char *argv[] )
{
    int errs = 0, tmp_errs = 0, err, myrank, color;
    int *sendbuf = 0, *recvbuf = 0;
    int i, j, idx, count, rrank, rsize;
    MPI_Comm commsplit, intercomm;
    MPI_Datatype datatype;

    MPI_Init(&argc, &argv);
	
	MPI_Comm_rank( MPI_COMM_WORLD, &myrank );
	color = myrank % 2;
	MPI_Comm_split(MPI_COMM_WORLD, color, myrank, &commsplit);
	MPI_Intercomm_create( commsplit, 0, MPI_COMM_WORLD, 1-color, 52, &intercomm);
	datatype = MPI_INT;
	    
	if(myrank == 0)
	fprintf(stderr,"Lancement test Alltoall ...\n");
	for (count = 1; count < 16; count = 2 * count) 
	{
		MPI_Comm_remote_size( intercomm, &rsize );
		MPI_Comm_rank( intercomm, &rrank );
		
	    sendbuf = (int *)malloc( rsize * count * sizeof(int) );
	    recvbuf = (int *)malloc( rsize * count * sizeof(int) );
	    
	    for (i=0; i<rsize*count; i++) 
			recvbuf[i] = -1;
			
	    if (color == 0) 
	    {
			idx = 0;
			for (j=0; j<rsize; j++)
				for (i=0; i<count; i++)
					sendbuf[idx++] = i + rrank;
		
			err = MPI_Alltoall( sendbuf, count, datatype, NULL, 0, datatype, intercomm );
			
			if (err)
				errs++;
	    }
	    else 
	    {
			int rank, size;

			MPI_Comm_rank( intercomm, &rank );
			MPI_Comm_size( intercomm, &size );

			err = MPI_Alltoall( NULL, 0, datatype, recvbuf, count, datatype, intercomm );
			
			if (err)
				errs++;
			
			idx = 0;
			for (j=0; j<rsize; j++) 
			{
				for (i=0; i<count; i++) 
				{
					if (recvbuf[idx++] != i + j) 
					{
						errs++;
						if (errs < 10) 
							fprintf( stderr, "warning : buf[%d] = %d on %d\n", i, recvbuf[i], rank);
					}
				}
			}
	    }
	    free( recvbuf );
	    free( sendbuf );
	}
	
	tmp_errs = errs;
	MPI_Allreduce(&tmp_errs, &errs, 1, MPI_INT, MPI_SUM, MPI_COMM_WORLD);
	
	if(myrank == 0)
	{
		if(errs)
		{
			fprintf(stderr,"erreurs detectees\n");
			exit(1);
		}
		else
		{
			fprintf(stderr,"aucune erreur detectee\n");
		}
	}
    MPI_Finalize();
    return 0;
}
