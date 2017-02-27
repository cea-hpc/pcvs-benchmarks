#include <stdlib.h>
#include <stdio.h>
#include <omp.h>

int thr;
#pragma omp threadprivate (thr)

typedef struct info_s{
  int val;
  int* addr;
} info_t;

void reset_infos( info_t* t, int* e ){
  int i;

  for( i = 0; i < *e; i++ ){
    t[i].val = 0;
    t[i].addr = (int*) 0;
  }
}

int check_val( info_t* t, int* e ){
  int i;
  int ret = 0;

  for( i = 0; i < *e; i++ ){
    if( t[0].val != t[i].val ){
      ret = 1;
      break;
    }
  }

  return ret;
}

int check_addr( info_t* t, int* e ){
  int i,j;
  int ret = 0;

  for( i = 0; i < *e; i++ ){
    for( j = i + 1; j < *e; j++ ){
      if( t[i].addr == t[j].addr ){
        ret = 1;
      }
    }

    if( ret ) break;
  }

  return ret;
}

int main( int argc, char** argv ){
  int i;
  int task_n;
  int nb_th;
  int nb_it = 1000;

  #pragma omp parallel
  {
    nb_th = omp_get_num_threads();
  }

  info_t infos[nb_th];

  for( i=0; i<nb_it; i++ ) {
    thr = i + omp_get_num_threads();
    reset_infos( infos, &nb_th );

    #pragma omp parallel copyin( thr ) private( task_n )
    {
      task_n = omp_get_thread_num();

      infos[task_n].val = thr;
      infos[task_n].addr = &thr;
    }

    if( check_val( infos, &nb_th )){
      printf("At least one thread didn't get the rigth value of threadprivate variable thr\n");
      abort();
    }
    if( check_addr(infos, &nb_th )){
      printf("At least one thread failed to receive proper memory allocation for thread private variable thr");
      abort();
    }
  }

  return EXIT_SUCCESS;
}

