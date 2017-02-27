#include <pthread.h>
#include <mpc.h>
#include <stdio.h>

double sctk_atomics_get_timestamp_gettimeofday_test() {
  struct timeval tp;
  gettimeofday (&tp, NULL);
  return tp.tv_usec + tp.tv_sec * 1000000;
}


#define TH_MAX 256
#define ITERMAX 100000

double th_yield[TH_MAX];

void* run(void* arg){
  long i ;
  long id;
  double start;
  double end;

  id = (long)arg;
  sctk_thread_yield();

  start = sctk_atomics_get_timestamp_gettimeofday_test();
  for(i = 0; i < ITERMAX; i++){
    sctk_thread_yield();
  }
  end = sctk_atomics_get_timestamp_gettimeofday_test();
  th_yield[id] = (end - start);
}

int main(int argc, char** argv){
  long i;
  pthread_t TIDS[TH_MAX];
  double start;
  double end;
  double start_create;
  double end_create;
  double yield = 0;

#ifdef MPC_MODULE_MPC_Debugger
  fprintf(stderr,"Debug Enabled ");
#else
  fprintf(stderr,"Debug Disabled ");
#endif

  start = sctk_atomics_get_timestamp_gettimeofday_test();
  start_create = sctk_atomics_get_timestamp_gettimeofday_test();
  for (i = 0; i < TH_MAX; i++){
    pthread_create(&(TIDS[i]), NULL,run,(void*)i);
  }
  end_create = sctk_atomics_get_timestamp_gettimeofday_test();

  for (i = 0; i < TH_MAX; i++){
    pthread_join(TIDS[i],NULL);
  }
  end = sctk_atomics_get_timestamp_gettimeofday_test();


  for (i = 0; i < TH_MAX; i++){
    yield += th_yield[i];
  }

  yield = yield/TH_MAX;

  fprintf(stderr,"%d threads Elaspe %fs Create %f yield %fs\n",TH_MAX,(end - start)/1000000.0,(end_create - start_create)/1000000.0,yield/1000000.0);

  return 0;
}
