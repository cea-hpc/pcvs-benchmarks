#include <stdlib.h>
#include <stdio.h>
#ifdef MPC_MODULE_MPC_MPI
#include <mpi.h>
#else
#include <mpc.h>
#endif

int retrun_val= 255;

__thread int thread_local = -1;

int breakpoint_func_end(){

}


int breakpoint_func(){
  int cur_val = 255;

  breakpoint_func_end();
  fprintf(stderr,"RET VAL %d->%d\n",cur_val,retrun_val);
  retrun_val = cur_val;
}

int main(int argc,char * argv[])
{
  char* toto = NULL;
#ifdef MPC_MODULE_MPC_MPI

  int rank, size, loc_rank;
  MPI_Init(&argc, &argv);
  MPI_Comm_rank(MPI_COMM_WORLD, &rank);
  MPI_Comm_size(MPI_COMM_WORLD, &size);

  MPC_Local_task_rank(&loc_rank);

  MPI_Barrier(MPI_COMM_WORLD);
	
  thread_local = rank;
  fprintf(stderr,"Nb tasks: %d, Hello world from rank %d local %d!!! %s\n", size, rank,loc_rank,argv[1]);
#else
  printf("Hello world\n");
#endif

#ifdef MPC_MODULE_MPC_MPI

  MPI_Barrier(MPI_COMM_WORLD);
  if(loc_rank == 0){
    breakpoint_func();
  } 
  MPI_Bcast(&retrun_val,1,MPI_INT,0,MPI_COMM_WORLD);

#else
  breakpoint_func();
#endif

  {
    int val;
    val = retrun_val;
#ifdef MPC_MODULE_MPC_MPI
    MPI_Allreduce(&val,&retrun_val,1,MPI_INT,MPI_MAX,MPI_COMM_WORLD);
    MPI_Barrier(MPI_COMM_WORLD);
    MPI_Finalize();
#endif
    fprintf(stderr,"RET VAL %d to %d\n",val,retrun_val);
  }
  return retrun_val;
}
