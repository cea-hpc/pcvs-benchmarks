#define BENCHMARK "OSU MPI Multi-threaded Latency Test"
/*
 * Copyright (C) 2002-2013 the Network-Based Computing Laboratory
 * (NBCL), The Ohio State University.
 *
 * Contact: Dr. D. K. Panda (panda@cse.ohio-state.edu)
 *
 * For detailed copyright and licensing information, please refer to the
 * copyright file COPYRIGHT in the top level OMB directory.
 */

#include <mpi.h>
#include <pthread.h>
#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>

#define MESSAGE_ALIGNMENT 64
#define MAX_MSG_SIZE (1 << 22)
#define MYBUFSIZE (MAX_MSG_SIZE + MESSAGE_ALIGNMENT)
#define THREADS 8
#define SKIP_LARGE 10
#define LOOP_LARGE 100
#define LARGE_MESSAGE_SIZE 8192

int skip = 1000;
int loop = 10000;
MPI_Comm comms[THREADS];

int finished_size;

MPI_Status reqstat[THREADS];

typedef struct thread_tag { int id; } thread_tag_t;

void *send_thread(void *arg);
void *recv_thread(void *arg);

#ifdef PACKAGE_VERSION
#define HEADER "# " BENCHMARK " v" PACKAGE_VERSION "\n"
#else
#define HEADER "# " BENCHMARK "\n"
#endif

#ifndef FIELD_WIDTH
#define FIELD_WIDTH 20
#endif

#ifndef FLOAT_PRECISION
#define FLOAT_PRECISION 2
#endif

pthread_barrier_t thread_barrier;

int main(int argc, char *argv[]) {
  int numprocs, provided, myid, err;
  int i = 0;
  pthread_t sr_threads[THREADS];
  thread_tag_t tags[THREADS];

  pthread_barrier_init(&thread_barrier, NULL, THREADS);

  err = MPI_Init_thread(&argc, &argv, MPI_THREAD_MULTIPLE, &provided);

  if (err != MPI_SUCCESS) {
    MPI_Abort(MPI_COMM_WORLD, 1);
  }

  MPI_Comm_size(MPI_COMM_WORLD, &numprocs);
  MPI_Comm_rank(MPI_COMM_WORLD, &myid);

  if (numprocs != 2) {
    if (myid == 0) {
      fprintf(stderr, "This test requires exactly two processes\n");
    }

    MPI_Finalize();

    return EXIT_FAILURE;
  }

  /* Check to make sure we actually have a thread-safe
   * implementation
   */

  finished_size = 1;

  if (provided != MPI_THREAD_MULTIPLE) {
    if (myid == 0) {
      fprintf(stderr, "MPI_Init_thread must return MPI_THREAD_MULTIPLE!\n");
    }

    MPI_Finalize();

    return EXIT_FAILURE;
  }

  if (myid == 0) {
    for (i = 0; i < THREADS; i++) {
      MPI_Comm_dup(MPI_COMM_WORLD, &(comms[i]));
      fprintf(stderr, "#Rank %d thread %d comm %d\n", myid, i, comms[i]);
    }
    fprintf(stdout, HEADER);
    fprintf(stdout, "%-*s%*s\n", 10, "# Size", FIELD_WIDTH, "Latency (us)");
    fflush(stdout);

    for (i = 0; i < THREADS; i++) {
      tags[i].id = i;
      pthread_create(&sr_threads[i], NULL, send_thread, &tags[i]);
    }

    for (i = 0; i < THREADS; i++) {
      pthread_join(sr_threads[i], NULL);
    }

  }

  else {
    for (i = 0; i < THREADS; i++) {
      MPI_Comm_dup(MPI_COMM_WORLD, &(comms[i]));
      // fprintf(stderr,"Rank %d thread %d\n",myid,i);
      fprintf(stderr, "#Rank %d thread %d comm %d\n", myid, i, comms[i]);
      tags[i].id = i;
      pthread_create(&sr_threads[i], NULL, recv_thread, &tags[i]);
    }

    for (i = 0; i < THREADS; i++) {
      pthread_join(sr_threads[i], NULL);
    }
  }

  fprintf(stderr, "All threads done %d\n", myid);

  MPI_Finalize();

  return EXIT_SUCCESS;
}

void synch_thread() { pthread_barrier_wait(&thread_barrier); }

void *recv_thread(void *arg) {
  int size, i, val, align_size;
  int iter;
  char *s_buf, *r_buf;
  thread_tag_t *thread_id;

  thread_id = (thread_tag_t *)arg;
  val = thread_id->id;

  align_size = MESSAGE_ALIGNMENT;

  s_buf = malloc(MAX_MSG_SIZE);
  r_buf = malloc(MAX_MSG_SIZE);

  for (size = 0, iter = 0; size <= MAX_MSG_SIZE; size = (size ? size * 2 : 1)) {
    MPI_Barrier(comms[val]);
    if (size > LARGE_MESSAGE_SIZE) {
      loop = LOOP_LARGE;
      skip = SKIP_LARGE;
    }

    /* touch the data */
    for (i = 0; i < size; i++) {
      s_buf[i] = 'a';
      r_buf[i] = 'b';
    }

    for (i = 0; i < loop + skip; i++) {
      MPI_Recv(r_buf, size, MPI_CHAR, 0, 1, comms[val], &reqstat[val]);
      MPI_Send(s_buf, size, MPI_CHAR, 0, 2, comms[val]);
    }
    synch_thread();

    iter++;
  }

  sleep(1);

  return 0;
}

void *send_thread(void *arg) {
  int size, i, val, align_size;
  int iter;
  char *s_buf, *r_buf;
  thread_tag_t *thread_id;
  double t_start = 0, t_end = 0, t = 0, latency;

  thread_id = (thread_tag_t *)arg;
  val = thread_id->id;

  align_size = MESSAGE_ALIGNMENT;

  s_buf = malloc(MAX_MSG_SIZE);
  r_buf = malloc(MAX_MSG_SIZE);

  for (size = 0, iter = 0; size <= MAX_MSG_SIZE; size = (size ? size * 2 : 1)) {
    MPI_Barrier(comms[val]);
    if (size > LARGE_MESSAGE_SIZE) {
      loop = LOOP_LARGE;
      skip = SKIP_LARGE;
    }

    /* touch the data */
    for (i = 0; i < size; i++) {
      s_buf[i] = 'a';
      r_buf[i] = 'b';
    }

    for (i = 0; i < loop + skip; i++) {
      if (i == skip) {
        t_start = MPI_Wtime();
      }
      MPI_Send(s_buf, size, MPI_CHAR, 1, 1, comms[val]);
      MPI_Recv(r_buf, size, MPI_CHAR, 1, 2, comms[val], &reqstat[val]);
    }

    t_end = MPI_Wtime();
    t = t_end - t_start;

    latency = (t)*1.0e6 / (2.0 * loop);
    fprintf(stdout, "%-*d%*.*f %4d\n", 10, size, FIELD_WIDTH, FLOAT_PRECISION,
            latency, val);
    fflush(stdout);

    synch_thread();

    iter++;
  }

  sleep(1);

  return 0;
}

#if 0
void * send_thread(void *arg) {
    int size, i, val, align_size, iter;
    char *s_buf, *r_buf;
    double t_start = 0, t_end = 0, t = 0, latency;
    thread_tag_t *thread_id = (thread_tag_t *)arg;

    val = thread_id->id;
    align_size = MESSAGE_ALIGNMENT;

    s_buf =
        (char *) (((unsigned long) s_buf1 + (align_size - 1)) /
                  align_size * align_size);
    r_buf =
        (char *) (((unsigned long) r_buf1 + (align_size - 1)) /
                  align_size * align_size);

    for(size = 0, iter = 0; size <= MAX_MSG_SIZE; size = (size ? size * 2 : 1)) {
        MPI_Barrier(comms[val]);

        if(size > LARGE_MESSAGE_SIZE) {
            loop = LOOP_LARGE;
            skip = SKIP_LARGE;
        }  

        /* touch the data */
        for(i = 0; i < size; i++) {
            s_buf[i] = 'a';
            r_buf[i] = 'b';
        }

        for(i = 0; i < loop + skip; i++) {
            if(i == skip) {
                t_start = MPI_Wtime();
            }

            MPI_Send(s_buf, size, MPI_CHAR, 1, 1, comms[val]);
            MPI_Recv(r_buf, size, MPI_CHAR, 1, 2, comms[val],
                    &reqstat[val]);
        }

        t_end = MPI_Wtime ();
        t = t_end - t_start;

        latency = (t) * 1.0e6 / (2.0 * loop);
        fprintf(stdout, "%-*d%*.*f\n", 10, size, FIELD_WIDTH, FLOAT_PRECISION,
                latency);
        fflush(stdout);
        iter++;
    }

    return 0;
}
#endif
/* vi: set sw=4 sts=4 tw=80: */
