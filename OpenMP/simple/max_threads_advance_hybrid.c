#include <stdio.h>
#include <omp.h>
#include <mpi.h>

int main( int argc, char** argv )
{
  int max_runtime_t ;
  int max_t ;
  int max_nest_t;
  int nest_t;
  int fixed_t ;
  int t_limit;
  int i;
  int root = 0;
  int my_rank;
  int comm_size;
  int total_count = 0, count = 0;
  int num_threads = getenv( "OMP_NUM_THREADS" ) ? atoi( getenv( "OMP_NUM_THREADS" )) : 0;

  t_limit = omp_get_thread_limit();

  MPI_Init( &argc, &argv );
  MPI_Comm_rank( MPI_COMM_WORLD, &my_rank );
  MPI_Comm_size( MPI_COMM_WORLD, &comm_size );

  printf("Task %d/%d starting...with t_limit=%d\n", my_rank, comm_size, t_limit );


  for ( i = 0 ; i < 256 ; i++ ) {

    max_runtime_t = omp_get_max_threads() ;

#pragma omp parallel
    {
#pragma omp master
      {
	    max_t = omp_get_num_threads() ;
      }

      max_nest_t = omp_get_max_threads() ;

#pragma omp parallel
      {
#pragma omp master
        {
          nest_t = omp_get_num_threads() ;
        }
      }
#pragma omp flush(nest_t)

      if ( nest_t != 1 ) {
        printf( "[%d] - 1.1 - Nested loop with max (expected 1) got %d != %d\n", 
                my_rank, max_nest_t, nest_t );
        count++;
      }
    }

    if( num_threads != 0 && num_threads < t_limit ){
      if( max_t != num_threads || max_t != max_runtime_t ){
        printf("1.2 - Iteration %d - num_threads=%d, max_runtime_t=%d, max_t=%d\n",
          i, num_threads, max_runtime_t, max_t );
        count++;
      }
    }
    else{
      if ( max_t > max_runtime_t ) {
        printf( "[%d] - 1.3 - Iteration %d - max_runtime_t=%d, max_t=%d\n", 
          my_rank, i, max_runtime_t, max_t );
        count++;
      }
    }
  }

  MPI_Reduce( &count, &total_count, 1, MPI_INT, MPI_SUM, root, MPI_COMM_WORLD );
  if( root == my_rank && total_count > 0 ){
    printf("At least one of the openmp task failed getting correct max threads value for the next parallel region in scenario 1\n");
    abort();
  }


  for ( i = 1 ; i < 256 ; i++ ) {

#pragma omp parallel num_threads(i)
    {
#pragma omp master
      {
	    fixed_t = omp_get_num_threads() ;
      }
    }

    max_runtime_t = omp_get_max_threads() ;

    if ( max_t > max_runtime_t ) {
      printf( "[%d] - 2.1 - Iteration %d - max_runtime_t=%d, max_t=%d\n", 
	  my_rank, i, max_runtime_t, max_t ) ;
      count++;
    }

    if( fixed_t < max_t && fixed_t != i ){
      printf( "[%d] - 2.2 - Iteration %d - max_t=%d, fixed_t=%d, i=%d\n",
          my_rank, i, max_t, fixed_t, i );
      count++;
    }

    if( fixed_t > t_limit ){
      printf( "[%d] - 2.3 - Iteration %d - fixed_t=%d, t_limit=%d\n",
          my_rank, i, fixed_t, t_limit );
      count++;
    }
#if 0
    if ( fixed_t > n_t ) {
      printf( "3 - Iteration %d - fixed_t=%d, n_t=%d\n", 
	  i, fixed_t, n_t ) ;
      abort() ;
    }
#endif

  }

  MPI_Reduce( &count, &total_count, 1, MPI_INT, MPI_SUM, root, MPI_COMM_WORLD );
  if( root == my_rank && total_count > 0 ){
    printf("At least one of the openmp task failed getting correct max threads value for the next parallel region in scenario 2\n");
    abort();
  }

  for ( i = 1 ; i < 256 ; i++ ) {

    omp_set_num_threads( i );

#pragma omp parallel
    {
#pragma omp master
      {
	    fixed_t = omp_get_num_threads() ;
      }
    }

    max_runtime_t = omp_get_max_threads() ;

    if ( max_runtime_t != i ) {
      printf( "[%d] - 3.1 - Iteration %d - max_t=%d, i=%d\n", 
	  my_rank, i, max_runtime_t, i ) ;
      count++;
    }

    if( fixed_t < t_limit && fixed_t != i ){
      printf( "[%d] - 3.2 - Iteration %d - t_limit=%d, fixed_t=%d, i=%d\n",
          my_rank, i, t_limit, fixed_t, i );
      count++;
    }

    if( fixed_t > t_limit ){
      printf( "[%d] - 3.3 - Iteration %d - fixed_t=%d, t_limit=%d\n",
          my_rank, i, fixed_t, t_limit );
      count++;
    }

  }

  MPI_Reduce( &count, &total_count, 1, MPI_INT, MPI_SUM, root, MPI_COMM_WORLD );
  if( root == my_rank && total_count > 0 ){
    printf("At least one of the openmp task failed getting correct max threads value for the next parallel region in scenario 3\n");
    abort();
  }

  MPI_Finalize();

  return 0 ;
}
