#include <stdio.h>
#include <mpi.h>

int* tata []= {NULL};
int main( int argc, char ** argv ) {
 int titi = -2;	
  fprintf( stdout, "Hello\n" ) ; 

  MPI_Init( &argc, &argv ) ;
  fprintf( stdout, "Value of toto %d %d %p\n", titi,*tata,&tata[0]) ;

	MPI_Comm_rank(MPI_COMM_WORLD, &titi) ;
	MPI_Comm_rank(MPI_COMM_WORLD, tata) ;

  MPI_Barrier( MPI_COMM_WORLD ) ;
 fprintf( stdout, "Value of toto %d %d\n", titi,*tata) ;
  MPI_Finalize() ;

  return 0 ;

}

