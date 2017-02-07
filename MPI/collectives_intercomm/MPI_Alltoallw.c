#include "mpi.h"
#include <stdio.h>
#include <stdlib.h>

int main( int argc, char *argv[] )
{
    int myrank, color;
    MPI_Comm commsplit, intercomm;
    MPI_Datatype *sendtypes, *recvtypes;
	int      *sbuf, *rbuf;
    int      rank, size, lsize, asize;
    int      *sendcounts, *recvcounts, *rdispls, *sdispls;
    int      i, j, *p, err, errs = 0;
    
    MPI_Init(&argc, &argv);
	
	MPI_Comm_rank( MPI_COMM_WORLD, &myrank );
	color = myrank % 2;
	MPI_Comm_split(MPI_COMM_WORLD, color, myrank, &commsplit);
	MPI_Intercomm_create( commsplit, 0, MPI_COMM_WORLD, 1-color, 52, &intercomm);
	    
	if(myrank == 0)
	fprintf(stderr,"Lancement test Alltoallw ...\n");
	
	MPI_Comm_size( intercomm, &lsize );
	MPI_Comm_remote_size( intercomm, &size );
	asize = (lsize > size) ? lsize : size;
	MPI_Comm_rank( intercomm, &rank );
	sbuf = (int *)malloc( size * size * sizeof(int) );
	rbuf = (int *)malloc( asize * asize * sizeof(int) );
	if (!sbuf || !rbuf) 
	{
		fprintf( stderr, "Could not allocated buffers!\n" );
		MPI_Abort( intercomm, 1 );
	}

	for (i=0; i<size*size; i++) 
	{
	sbuf[i] = i + 100*rank;
	rbuf[i] = -i;
	}

	sendcounts = (int *)malloc( size * sizeof(int) );
	recvcounts = (int *)malloc( size * sizeof(int) );
	rdispls    = (int *)malloc( size * sizeof(int) );
	sdispls    = (int *)malloc( size * sizeof(int) );
	
	sendtypes  = (MPI_Datatype *)malloc( size * sizeof(MPI_Datatype) );
	recvtypes  = (MPI_Datatype *)malloc( size * sizeof(MPI_Datatype) );
	
	if (!sendcounts || !recvcounts || !rdispls || !sdispls || !sendtypes || !recvtypes) 
	{
		fprintf( stderr, "Could not allocate arg items!\n" );
		MPI_Abort( intercomm, 1 );
	}
	
	for (i=0; i<size; i++) 
	{
		sendcounts[i] = i;
		sdispls[i]    = (((i+1) * (i))/2) * sizeof(int);
		sendtypes[i]  = MPI_INT;
		recvcounts[i] = rank;
		rdispls[i]    = i * rank * sizeof(int);
		recvtypes[i]  = MPI_INT;
	}
	MPI_Alltoallw( sbuf, sendcounts, sdispls, sendtypes, rbuf, recvcounts, rdispls, recvtypes, intercomm );

	for (i=0; i<size; i++) 
	{
		p = rbuf + rdispls[i]/sizeof(int);
		for (j=0; j<rank; j++) 
		{
			if (p[j] != i * 100 + (rank*(rank+1))/2 + j) 
			{
				fprintf( stderr, "[%d] got %d expected %d for %dth\n", rank, p[j],(i*(i+1))/2 + j, j );
				err++;
			}
		}
	}

	free(sendtypes);
	free(recvtypes);
	free( sdispls );
	free( rdispls );
	free( recvcounts );
	free( sendcounts );
	free( rbuf );
	free( sbuf );
	
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
