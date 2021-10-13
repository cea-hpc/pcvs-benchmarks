/*
 *  (C) 2020 by Bull S. A. S.  All rights reserved.
 *
 *  This test measure performance for MPI notified RMA vs a Point-to-point pattern
 */

#include <stdio.h>
#include <stdlib.h>
#include <sys/time.h>
#include <mpi.h>
#include <mpi-ext.h>

#include "util/common.h"
#include "util/common_output.h"

enum comm_type {PUT, GET} mpi_comm;

MPI_Win win;
int n_notifs;
/* Send and receive buffers associated to MPI windows */
char *sbuf = NULL, *rbuf = NULL;

double perf_notified(int size, int rank, int peer, int max_notif_idx){
    struct timeval start, end;
    double duration;
    int id = 0;

    MPI_CHECK(MPI_Win_lock_all(0, win));

    switch(mpi_comm){
        case PUT:

            for(int i=0, id=0; i< n_warmup + n_notifs; i++){
                if( i == n_warmup){
                    gettimeofday(&start, NULL);
                }
                if (!rank){
                    DBG("Send notif %d (%d)\n", id, i);
                    MPI_CHECK(MPIX_Put_notify(sbuf, size, MPI_CHAR, peer,
                                              0, size, MPI_CHAR, win, id));
                    COMPUTE; /* Expecting comm-computation overlap */
                    MPI_CHECK(MPIX_Win_wait_notify(win, id));
                } else {
                    MPI_CHECK(MPIX_Win_wait_notify(win, id));
                    MPI_CHECK(MPIX_Put_notify(sbuf, size, MPI_CHAR, peer,
                                              0, size, MPI_CHAR, win, id));
                    COMPUTE; /* Expecting comm-computation overlap */
                }
                id = (id + LARGE_PRIME) % max_notif_idx;
            }

            break;

        case GET:
            for(int i=0, id=0; i< n_warmup + n_notifs; i++){
                if( i == n_warmup){
                    gettimeofday(&start, NULL);
                }
                if (!rank){
                    DBG("Send notif %d (%d)\n", id, i);
                    MPI_CHECK(MPIX_Get_notify(sbuf, size, MPI_CHAR, peer,
                                              0, size, MPI_CHAR, win, id));
                    COMPUTE; /* Expecting comm-computation overlap */
                    MPI_CHECK(MPIX_Win_wait_notify(win, id));
                } else {
                    MPI_CHECK(MPIX_Win_wait_notify(win, id));
                    MPI_CHECK(MPIX_Get_notify(sbuf, size, MPI_CHAR, peer,
                                              0, size, MPI_CHAR, win, id));
                    COMPUTE; /* Expecting comm-computation overlap */
                }
                id = (id + LARGE_PRIME) % max_notif_idx;
            }

            break;
    }

    gettimeofday(&end, NULL);
    MPI_CHECK(MPI_Win_unlock_all(win));

    duration = (end.tv_sec - start.tv_sec) *1000000 + (end.tv_usec -start.tv_usec);
    duration /= (double)n_notifs;
    duration -= compute_time;
    duration /= 2.0;

    return duration;
}

double perf_p2p(int size, int rank, int peer){
    struct timeval start, end;
    double duration;
    int id = 0;
    MPI_Request req;
    MPI_Status st;
    double time;

    for(int i=0, id=0; i< n_warmup + n_notifs; i++){
        if( i == n_warmup){
            gettimeofday(&start, NULL);
        }
        if(!rank){
            DBG("Send notif %d (%d)\n", id, i);
            COMPUTE; /* Expecting comm-computation overlap */
            MPI_CHECK(MPI_Send(sbuf, size, MPI_CHAR, peer, 0, MPI_COMM_WORLD));
            MPI_CHECK(MPI_Recv(rbuf, size, MPI_CHAR, peer, 0, MPI_COMM_WORLD, &st));
        } else {
            MPI_CHECK(MPI_Recv(rbuf, size, MPI_CHAR, peer, 0, MPI_COMM_WORLD, &st));
            DBG("Send notif %d (%d)\n", id, i);
            MPI_CHECK(MPI_Send(sbuf, size, MPI_CHAR, peer, 0, MPI_COMM_WORLD));
            COMPUTE; /* Expecting comm-computation overlap */
        }
    }
    gettimeofday(&end, NULL);

    duration = (end.tv_sec - start.tv_sec) *1000000 + (end.tv_usec -start.tv_usec);
    duration /= (double)n_notifs;
    duration -= compute_time;
    duration /= 2.0;

    return duration;
}

double perf_p2pNB(int size, int rank, int peer){
    struct timeval start, end;
    double duration;
    int id = 0;
    MPI_Request req;
    MPI_Status st;
    double time;

    for(int i=0, id=0; i< n_warmup + n_notifs; i++){
        if( i == n_warmup){
            gettimeofday(&start, NULL);
        }
        if(!rank){
            DBG("Send notif %d (%d)\n", id, i);
            MPI_CHECK(MPI_Isend(sbuf, size, MPI_CHAR, peer, 0, MPI_COMM_WORLD, &req));
            COMPUTE; /* Expecting comm-computation overlap */
            MPI_Wait(&req, MPI_STATUSES_IGNORE);
            MPI_CHECK(MPI_Irecv(rbuf, size, MPI_CHAR, peer, 0, MPI_COMM_WORLD, &req));
            COMPUTE; /* Expecting comm-computation overlap */
            MPI_Wait(&req, MPI_STATUSES_IGNORE);
        } else {
            MPI_CHECK(MPI_Irecv(rbuf, size, MPI_CHAR, peer, 0, MPI_COMM_WORLD, &req));
            COMPUTE; /* Expecting comm-computation overlap */
            MPI_Wait(&req, MPI_STATUSES_IGNORE);
            DBG("Send notif %d (%d)\n", id, i);
            MPI_CHECK(MPI_Isend(sbuf, size, MPI_CHAR, peer, 0, MPI_COMM_WORLD, &req));
            COMPUTE; /* Expecting comm-computation overlap */
            MPI_Wait(&req, MPI_STATUSES_IGNORE);
        }
    }
    gettimeofday(&end, NULL);

    duration = (end.tv_sec - start.tv_sec) *1000000 + (end.tv_usec -start.tv_usec);
    duration /= (double)n_notifs;
    duration -= 2 * compute_time;
    duration /= 2.0;

    return duration;
}

static void run_test(int rank, enum WINDOW type)
{
    int max_notif_idx = get_max_notification_id();
    int peer = (rank + 1) % 2;
    int size = 1;

    if (max_notif_idx < n_notifs) {
        fprintf(stderr, "ERROR: this test requires at least %d different notifications\n",
                n_notifs);
        exit(1);
    }

    win_and_memory_allocate(rank, (void **)&sbuf, (void **)&rbuf,
                            max_size * sizeof(char), type, &win);
    MPI_CHECK(MPI_Barrier(MPI_COMM_WORLD));

    if ( rank == 0){
        fprintf(stdout, "%-14s %13s %8s %13s \n",
                "Bytes", (mpi_comm == PUT ? "Put_notify(us)":"Get_notify(us)") , "Send(us)", "Isend(us)",
                compute_time);
    }
    if (rank < 2){
        for (size = min_size; size < max_size ; size = (size ? size*2: 1)){
            double dnot = 0, dp2p=0 , dp2pNB= 0;
            dnot = perf_notified(size,rank, peer, max_notif_idx);
            dp2p = perf_p2p(size,rank, peer);
            dp2pNB = perf_p2pNB(size,rank, peer);
            if ( rank == 0){
                fprintf(stdout, "%-8d %13.2f %13.2f %13.2f \n", size, dnot, dp2p, dp2pNB);
                fflush(stdout);
            }
        }
    }
    win_and_memory_free(sbuf, rbuf, type, win);
}

int main (int argc, char *argv[])
{
    int rank, nprocs;

    parse_env();
    n_notifs = n_iter;

    char * comm = getenv("COMM");
    if (NULL == comm || !strcmp(comm, "PUT")) {
        mpi_comm = PUT;
    } else if (!strcmp(comm, "GET")){
        mpi_comm = GET;
    } else {
        if ( rank == 0 ){
            fprintf(stderr, "Unknown test case '%s'\n", comm);
        }
        exit(1);
    }

    options.win = WIN_ALLOCATE_NOTIFY;
    options.sync = NOTIFICATIONS;
    options.bench = FUNCTIONAL_OK;

    MPI_CHECK(MPI_Init(&argc, &argv));
    MPI_CHECK(MPI_Comm_size(MPI_COMM_WORLD, &nprocs));
    MPI_CHECK(MPI_Comm_rank(MPI_COMM_WORLD, &rank));

    if(!rank){
        fprintf(stdout, "Pingpong latency (with %d us of computation)\n",
                compute_time) ;
    }

    run_test(rank, options.win);

    MPI_CHECK(MPI_Finalize());

    return EXIT_SUCCESS;
}
