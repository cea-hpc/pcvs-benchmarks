#include <pthread.h>
#include <stdio.h>
#include <string.h>
#include <signal.h>
#include <stdlib.h>

#define STACK_SIZE (2*1024*1024)
#define GUARDSIZE (1*1024*1024)
#define NB_OPS 8

static void
sctk_require_page_handler (int signo, siginfo_t * info, void *context)
{
  fprintf(stderr,"SEGV on %p with error %d (SEGV_ACCERR = %d)\n",info->si_addr,info->si_code,SEGV_ACCERR);
  fprintf (stderr, "Test passed\n");
  exit (0);
}

void* foo(void* bar){
  int i; 
  char tab[GUARDSIZE/NB_OPS];

  fprintf(stderr,"MEMSET %p-%p\n",tab , tab+GUARDSIZE/NB_OPS);
  memset(tab, 0, GUARDSIZE/NB_OPS);

  return  foo(bar);
}

void* foo_start(void* bar){
  struct sigaction sigparam;
  struct sigaltstack ss;
  struct sigaltstack oss;
  void * stack;
  sigset_t sigs;

  stack = malloc(GUARDSIZE);

  sigemptyset (&sigs);
  sigaddset (&sigs, SIGSEGV);
  sigparam.sa_sigaction = sctk_require_page_handler;
  sigemptyset (&sigparam.sa_mask);
  sigparam.sa_flags = SA_SIGINFO|SA_ONSTACK;
  if(sigaction (SIGSEGV, &sigparam, NULL) != 0){
    abort();
  }

  ss.ss_sp = stack;
  ss.ss_size = GUARDSIZE;
  ss.ss_flags = 0;
  if (sigaltstack (&ss, &oss) < 0)
    abort ();

  return foo(bar);
}

int main(){
  pthread_t pid; 
  pthread_attr_t attr;

  pthread_attr_init(&attr);

  /* fprintf(stderr,"TRY thread creation\n"); */
  /* pthread_create(&pid, &attr,foo,NULL); */
  /* pthread_join(pid,NULL); */

  fprintf(stderr,"TRY thread creation with guardsize\n");
  pthread_attr_setguardsize(&attr, GUARDSIZE);
  pthread_attr_setstacksize(&attr,STACK_SIZE);
  pthread_create(&pid, &attr,foo_start,NULL);
  pthread_join(pid,NULL);


  fprintf(stderr,"Test failed\n");
  return 1;
}
