#include <pthread.h>
#include <mpi.h>
#include <stdio.h>
void
sctk_print_topology (FILE * fd);

void print_check(pthread_mutex_t * lock, char* name){

  pthread_mutex_lock(lock);
  fprintf(stderr,"LOCK: %s %p\n",name,lock);
  pthread_mutex_unlock(lock);
  MPI_Barrier(MPI_COMM_WORLD);
}

#define LOCK(a) print_check(&a,__STRING(a))

int main(int argc, char** argv){
  int rank;
  static pthread_mutex_t lock = PTHREAD_MUTEX_INITIALIZER;
#pragma hls node(lock)
  static pthread_mutex_t numa_lock = PTHREAD_MUTEX_INITIALIZER;
#pragma hls numa(numa_lock)
  static pthread_mutex_t core_lock = PTHREAD_MUTEX_INITIALIZER;
#pragma hls core(core_lock)

  MPI_Init(&argc,&argv);

  MPI_Comm_rank(MPI_COMM_WORLD,&rank);

/*   if(rank == 0){ */
/*     sctk_print_topology(stderr); */
/*   } */

  LOCK(lock);
  LOCK(numa_lock);
  LOCK(core_lock);


  MPI_Finalize();

  return 0;
}
