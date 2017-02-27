#include <mpi.h>
#include <stdio.h>

int main(int argc, char ** argv){
  int size; 
  int rank;
  int res = 4;
  int res2 = 4;
  int result = MPI_SUCCESS;
  int nb_error = 0;
  int nb_error2 = 0;
  int total_error = 0;
  int * rbuf;
  int root;
  int sendarray[100]; 
  int *displs,*rcounts; 
  int gsize,i,stride; 

  MPI_Init(&argc,&argv);

  MPI_Comm_rank(MPI_COMM_WORLD,&rank);
  MPI_Comm_size(MPI_COMM_WORLD,&size);

  rbuf = (int *)malloc(size*100*sizeof(int)); 

  stride = 1;
  gsize = size;
  displs = (int *)malloc(gsize*sizeof(int)); 
  rcounts = (int *)malloc(gsize*sizeof(int)); 
  for (i=0; i<gsize; ++i) { 
    displs[i] = i*stride; 
    rcounts[i] = 100; 
  } 

  /* MPI_Gather */
  for(root = 0; root < size; root++){
    if(root == rank){
      result = MPI_Gather(MPI_IN_PLACE , 100, MPI_INT, rbuf, 100, MPI_INT, root, MPI_COMM_WORLD); 
	  if (result != MPI_SUCCESS)
      {		  
		  total_error ++;
		  fprintf(stderr, "Rang %d: error on MPI_Gather, No %d\n", rank, result);
	  }
    } else {
      result = MPI_Gather(sendarray , 100, MPI_INT, rbuf, 100, MPI_INT, root, MPI_COMM_WORLD); 
	  if (result != MPI_SUCCESS)
      {		  
		  total_error ++;
		  fprintf(stderr, "Rang %d: error on MPI_Gather, No %d\n", rank, result);
	  }
    }
  }

  /* MPI_Scatter */
  for(root = 0; root < size; root++){
    if(root == rank){
      result = MPI_Scatter(rbuf , 100, MPI_INT, MPI_IN_PLACE, 100, MPI_INT, root, MPI_COMM_WORLD); 
	  if (result != MPI_SUCCESS)
      {		  
		  total_error ++;
		  fprintf(stderr, "Rang %d: error on MPI_Scatter, No %d\n", rank, result);
	  }
    } else {
      result = MPI_Scatter(rbuf  , 100, MPI_INT, sendarray, 100, MPI_INT, root, MPI_COMM_WORLD); 
	  if (result != MPI_SUCCESS)
      {		  
		  total_error ++;
		  fprintf(stderr, "Rang %d: error on MPI_Scatter, No %d\n", rank, result);
	  }
    }
  }

  /* MPI_Gatherv */
  for(root = 0; root < size; root++){
    if(root == rank){
      result = MPI_Gatherv(MPI_IN_PLACE , 100, MPI_INT, rbuf, rcounts, displs, MPI_INT, root, MPI_COMM_WORLD); 
	  if (result != MPI_SUCCESS)
      {		  
		  total_error ++;
		  fprintf(stderr, "Rang %d: error on MPI_Gatherv, No %d\n", rank, result);
	  }
    } else {
      result = MPI_Gatherv(sendarray , 100, MPI_INT, rbuf, rcounts, displs, MPI_INT, root, MPI_COMM_WORLD); 
	  if (result != MPI_SUCCESS)
      {		  
		  total_error ++;
		  fprintf(stderr, "Rang %d: error on MPI_Gatherv, No %d\n", rank, result);
	  }
    }
  }
  
  /* MPI_Scatterv */
  for(root = 0; root < size; root++){
    if(root == rank){
      result = MPI_Scatterv(rbuf , rcounts, displs, MPI_INT, MPI_IN_PLACE, 100, MPI_INT, root, MPI_COMM_WORLD); 
	  if (result != MPI_SUCCESS)
      {		  
		  total_error ++;
		  fprintf(stderr, "Rang %d: error on MPI_Scatterv, No %d\n", rank, result);
	  }
    } else {
      result = MPI_Scatterv(rbuf  , rcounts, displs, MPI_INT, sendarray, 100, MPI_INT, root, MPI_COMM_WORLD); 
	  if (result != MPI_SUCCESS)
      {		  
		  total_error ++;
		  fprintf(stderr, "Rang %d: error on MPI_Scatterv, No %d\n", rank, result);
	  }
    }
  }

      /* MPI_Allgather */
      result = MPI_Allgather(MPI_IN_PLACE , 100, MPI_INT, rbuf, 100, MPI_INT, MPI_COMM_WORLD); 
	  if (result != MPI_SUCCESS)
      {		  
		  total_error ++;
		  fprintf(stderr, "Rang %d: error on MPI_Allgather, No %d\n", rank, result);
	  }

	  /* MPI_Allgatherv */
      result = MPI_Allgatherv(MPI_IN_PLACE , 100, MPI_INT, rbuf, rcounts, displs, MPI_INT, MPI_COMM_WORLD); 
	  if (result != MPI_SUCCESS)
      {		  
		  total_error ++;
		  fprintf(stderr, "Rang %d: error on MPI_Allgatherv, No %d\n", rank, result);
	  }

	  /* MPI_Allreduce */
  	  result = MPI_Allreduce(MPI_IN_PLACE,&res,1,MPI_INT,MPI_SUM, MPI_COMM_WORLD);
	  if (result != MPI_SUCCESS)
      {		  
		  total_error ++;
		  fprintf(stderr, "Rang %d: error on MPI_Allreduce, No %d\n", rank, result);
	  }

	/* MPI_Reduce_scatter */
	int *sendbuf, recvbuf, *recvcounts;
	int sumval, idx, j, mycount;

	mycount = (1024 * 1024) / size;
    recvcounts = (int *)malloc( size * sizeof(int) );
	for (i=0; i<size; i++) 
		recvcounts[i] = mycount;
	
	sendbuf = (int *) malloc( mycount * size * sizeof(int) );
    if (!sendbuf) {
		fprintf( stderr, "Could not allocate %d ints for sendbuf\n", mycount * size );
		MPI_Abort( MPI_COMM_WORLD, 1 );
    }
    idx = 0;
    for (i=0; i<size; i++) {
		for (j=0; j<mycount; j++) {
	    	sendbuf[idx++] = rank + i;
		}
    }	

	MPI_Reduce_scatter( MPI_IN_PLACE, sendbuf, recvcounts, MPI_INT, MPI_SUM, MPI_COMM_WORLD);
	
	sumval = size * rank + ((size - 1) * size)/2;
	/* received values for my process should be size * (rank + i) */
	for (i=0; i<mycount; i++) {
		if (sendbuf[i] != sumval) {
			total_error++;
			fprintf( stdout, "Did not get expected value for reduce scatter (in place)\n" );
			fprintf( stdout, "[%d] Got buf[%d] = %d expected %d\n", rank, i, sendbuf[rank*mycount+i], sumval );
		}
	}
	free(sendbuf);

  MPI_Allreduce(&nb_error,&res,1,MPI_INT,MPI_SUM, MPI_COMM_WORLD);
  
  if(rank == 0){
    if (res == 0 && total_error == 0)
      fprintf(stderr,"Test Passed\n");
    else
      fprintf(stderr,"Test Failed (%d errors)\n",total_error);
  }
  MPI_Finalize();

  return res;
}
