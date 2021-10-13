/*
 *  (C) 2020 by Bull S. A. S.  All rights reserved.
 *
 *  This test measure performance for MPI_X_notified vs a X/Flush/Put pattern
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
char *sbuf=NULL, *rbuf=NULL;

double perf_x_flush_put(int size, int rank, int peer){
    struct timeval start, end;
    double duration;
    MPI_Win  notif_window;
    int * notifs;

    MPI_CHECK(MPI_Win_allocate(n_notifs * sizeof(int), sizeof(int), MPI_INFO_NULL,
                               MPI_COMM_WORLD, &notifs, &notif_window));
    memset(notifs, 0, n_notifs * sizeof(int));

    /* Warmup: lock/unlock a window and send some data to initialize mpi structures */
    MPI_CHECK(MPI_Win_lock_all(0, notif_window));
    MPI_CHECK(MPI_Win_lock_all(0, win));
    if (mpi_comm == PUT) {
        for(int i=0; i< n_warmup; i++){
            int zero = 0;
            MPI_CHECK(MPI_Put(sbuf, size , MPI_CHAR, peer, 0, size, MPI_CHAR, win));
            MPI_CHECK(MPI_Win_flush(peer, win));
            MPI_CHECK(MPI_Put(&zero, 1, MPI_CHAR, peer, 0, 1, MPI_CHAR, notif_window));
        }
    } else {
        for(int i=0; i< n_warmup; i++){
            int zero = 0;
            MPI_CHECK(MPI_Get(sbuf,size , MPI_CHAR, peer, 0, size, MPI_CHAR, win));
            MPI_CHECK(MPI_Win_flush(peer, win));
            MPI_CHECK(MPI_Put(&zero, 1, MPI_INT, peer, 0, 1, MPI_INT, notif_window));
        }
    }
    MPI_CHECK(MPI_Win_unlock_all(win));
    MPI_CHECK(MPI_Win_unlock_all(notif_window));


    gettimeofday(&start, NULL);

    MPI_CHECK(MPI_Win_lock_all(0, notif_window));
    MPI_CHECK(MPI_Win_lock_all(0, win));

    if (0 == rank) {
        if (mpi_comm == PUT) {
            for(int i=0; i< n_notifs; i++){
                DBG("Put %d \n", i);
                int notif_val = 1;
                MPI_CHECK(MPI_Put(sbuf, size, MPI_CHAR, peer, 0, size, MPI_CHAR, win));
                MPI_CHECK(MPI_Win_flush(peer, win));
                MPI_CHECK(MPI_Put(&notif_val, 1, MPI_INT, peer, i, 1, MPI_INT, notif_window));
                COMPUTE; /* Expecting comm-computation overlap */
            }
        } else {
            for(int i=0; i< n_notifs; i++){
                int zero = 0;
                DBG("Get %d \n", i);
                MPI_CHECK(MPI_Get(sbuf,size , MPI_CHAR, peer, 0, size, MPI_CHAR, win));
                MPI_CHECK(MPI_Win_flush(peer, win));
                MPI_CHECK(MPI_Put(&notif_window, 1, MPI_INT, peer, 0, 1, MPI_INT, notif_window));
            }
        }
    } else {
        for(int i=0; i< n_notifs; i++){
            COMPUTE; /* Expecting comm-computation overlap */
            DBG("Wait notif %d \n", i);
            do {
                MPI_Win_sync(notif_window);
            } while (notifs[i] == 0);
        }
    }

    MPI_CHECK(MPI_Win_unlock_all(win));
    MPI_CHECK(MPI_Win_unlock_all(notif_window));

    gettimeofday(&end, NULL);

    MPI_Win_free(&notif_window);

    duration = (end.tv_sec - start.tv_sec) *1000000 + (end.tv_usec -start.tv_usec);
    duration /= (double)n_notifs;
    duration -= compute_time;

    return duration;
}

double perf_put_notified(int size, int rank, int peer, int max_notif_idx){
    struct timeval start, end;
    double duration;
    int id = 0;

    /* Warmup: lock/unlock a window and send some data to initialize mpi structures */
    MPI_CHECK(MPI_Win_lock_all(0, win));
    for(int i=0; i< n_warmup; i++){
        MPI_CHECK(MPI_Put(sbuf,size , MPI_CHAR, peer, 0, size, MPI_CHAR, win));
    }
    MPI_CHECK(MPI_Win_unlock_all(win));


    gettimeofday(&start, NULL);
    MPI_CHECK(MPI_Win_lock_all(0, win));

    if (0 == rank) {
        if (mpi_comm == PUT){
            for(int i=0; i< n_notifs; i++){
                DBG("Send notif %d (%d)\n", id, i);
                MPI_CHECK(MPIX_Put_notify(sbuf, size, MPI_CHAR, peer,
                                          0, size, MPI_CHAR, win, id));
                id = (id + LARGE_PRIME) % max_notif_idx;
                COMPUTE; /* Expecting comm-computation overlap */
            }
        } else {
            for(int i=0; i< n_notifs; i++){
                DBG("Send notif %d (%d)\n", id, i);
                MPI_CHECK(MPIX_Get_notify(sbuf, size, MPI_CHAR, peer,
                                          0, size, MPI_CHAR, win, id));
                id = (id + LARGE_PRIME) % max_notif_idx;
                COMPUTE; /* Expecting comm-computation overlap */
            }
        }
    } else {
        for(int i=0; i< n_notifs; i++){
            COMPUTE; /* Expecting comm-computation overlap */
            DBG("Wait notif %d (%d)\n", id, i);
            MPI_CHECK(MPIX_Win_wait_notify(win, id));
            id = (id + LARGE_PRIME) % max_notif_idx;
        }
    }

    MPI_CHECK(MPI_Win_unlock_all(win));
    gettimeofday(&end, NULL);

    duration = (end.tv_sec - start.tv_sec) *1000000 + (end.tv_usec -start.tv_usec);
    duration /= (double)n_notifs;
    duration -= compute_time;

    return duration;
}

double perf_sendrecv(int size, int rank, int peer ){
    struct timeval start, end;
    double duration;
    MPI_Request req;

    for(int i=0; i< n_notifs + n_warmup; i++){
        if ( i == n_warmup){
            gettimeofday(&start, NULL);
        }
        if(!rank){
            MPI_Isend(sbuf, size, MPI_CHAR, peer, 0, MPI_COMM_WORLD, &req);
        } else {
            MPI_Irecv(rbuf, size, MPI_CHAR, peer, 0, MPI_COMM_WORLD, &req);
        }
        COMPUTE;
        MPI_Waitall(1, &req, MPI_STATUSES_IGNORE);
    }
    gettimeofday(&end, NULL);

    duration = (end.tv_sec - start.tv_sec) *1000000 + (end.tv_usec -start.tv_usec);
    duration /= (double)n_notifs;
    duration -= compute_time;

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
                            max_size * sizeof(int), type, &win);

    if(verbose || !rank){
        fprintf(stdout, "Bytes Notifications(us) %sFlushPut(us)  (Isend/Irecv(us)\n",
                mpi_comm == PUT ? "Put": "Get") ;
    }
    for (size = 1; size < max_size ; size*=2){
        double dnot = 0, dpfp = 0 , dsr = 0;
        dnot = perf_put_notified(size,rank, peer, max_notif_idx);
        dpfp = perf_x_flush_put(size,rank, peer);
        dsr = perf_sendrecv(size, rank, peer);
        if(verbose || !rank){
            fprintf(stdout, "%-8d %13.2f %13.2f %13.2f \n",
                    size, dnot, dpfp, dsr);
            fflush(stdout);
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

    if ( nprocs != 2 ){
        if ( rank == 0 ){
            fprintf(stderr, "This test requires exactly 2 processes."
                    " (called with %d processes)\n",
                    nprocs);
        }
        exit(1);
    }

    if(!rank){
        fprintf(stdout, "Unidir communication (with %d us of computation)\n",
                compute_time) ;
    }
    run_test(rank, options.win);

    MPI_CHECK(MPI_Finalize());

    return EXIT_SUCCESS;
}
