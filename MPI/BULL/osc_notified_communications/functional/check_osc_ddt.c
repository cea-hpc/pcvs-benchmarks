/*
 *  (C) 2021 by Bull S. A. S.  All rights reserved.
 *
 */


/* Check that vector datatype are correctly handled by MPI one sided
 * communications: send contiguous data or a vector to one of them on
 * the remote buffer (check all combinations) and check the received data.
 * Only Put, Rput, Get, Rget, Accumulate, Raccumulate, Get_accumulate and
 * Rget_accumulate are supported, since this test checks derived datatype and
 * requested version of communication calls.
 */

#define  INIT_GET -2
#define  INIT_EXPOSED -1

#include <stdio.h>
#include <stdlib.h>
#include <sys/time.h>
#include <mpi.h>

#include "util/common.h"
#include "util/common_output.h"

MPI_Win win;

/* Size of the data + gaps for ddt (Derived Data Types) */
#define BUFSIZE (max_size + n_block -1)

/* Send and receive buffers associated to MPI windows */
int *mybuf=NULL, *remote_buff=NULL, *get_buffer= NULL;

enum op{
    PUT = 0,
    GET,
    ACCUMULATE,
    GET_ACCUMULATE,
    NCOMMTPYE
};

char* op_names[10] = {
    [PUT] = "PUT",
    [GET] = "GET",
    [ACCUMULATE] = "ACCUMULATE",
    [GET_ACCUMULATE] = "GET_ACCUMULATE"
};

void do_comm(enum op type,
             int origin_dt_count, MPI_Datatype origin_dt,
             int peer,
             int target_dt_count, MPI_Datatype target_dt,
             int use_requests){
    switch(type){
        case GET_ACCUMULATE:
            if( use_requests){
                MPI_Request req;
                MPI_CHECK(MPI_Rget_accumulate(mybuf, origin_dt_count, origin_dt,
                                              get_buffer, origin_dt_count, origin_dt,
                                              peer, 0,
                                              target_dt_count, target_dt,
                                              MPI_MAX, win, &req));
                MPI_CHECK(MPI_Wait(&req, MPI_STATUS_IGNORE));
            } else {
                MPI_CHECK(MPI_Get_accumulate(mybuf, origin_dt_count, origin_dt,
                                             get_buffer, origin_dt_count, origin_dt,
                                             peer, 0,
                                             target_dt_count, target_dt,
                                             MPI_MAX, win));
            }
            break;
        case ACCUMULATE:
            if( use_requests){
                MPI_Request req;
                MPI_CHECK(MPI_Raccumulate(mybuf, origin_dt_count, origin_dt,
                                          peer, 0,
                                          target_dt_count, target_dt,
                                          MPI_MAX, win, &req));
                MPI_CHECK(MPI_Wait(&req, MPI_STATUS_IGNORE));
            } else {
                MPI_CHECK(MPI_Accumulate(mybuf, origin_dt_count, origin_dt,
                                         peer, 0,
                                         target_dt_count, target_dt,
                                         MPI_MAX, win));
            }
            break;
        case PUT:
            if( use_requests){
                MPI_Request req;
                MPI_CHECK(MPI_Rput(mybuf, origin_dt_count, origin_dt,
                                   peer, 0,
                                   target_dt_count, target_dt,
                                   win, &req));
                MPI_CHECK(MPI_Wait(&req, MPI_STATUS_IGNORE));
            } else {
                MPI_CHECK(MPI_Put(mybuf, origin_dt_count, origin_dt,
                                  peer, 0,
                                  target_dt_count, target_dt,
                                  win));
            }
            break;
        case GET:
            if( use_requests){
                MPI_Request req;
                MPI_CHECK(MPI_Rget(mybuf, origin_dt_count, origin_dt,
                                   peer, 0,
                                   target_dt_count, target_dt,
                                   win, &req));
                MPI_CHECK(MPI_Wait(&req, MPI_STATUS_IGNORE));
            } else {
                MPI_CHECK(MPI_Get(mybuf, origin_dt_count, origin_dt,
                                  peer, 0,
                                  target_dt_count, target_dt,
                                  win));
            }
            break;
        default:
            abort();
    }
}


int data_check(int * srcbuf, int * exposedbuf, int* fetchbuf, int bsize,
               int is_origin_ddt, int is_target_ddt){
    int errors = 0;
    int orig_shift = 0, target_shift = 0;

    for(int i = 0; i < bsize * n_block ; ++i){
        if (i > 0 && (i % bsize) == 0){
            if(is_origin_ddt){
                if(fetchbuf && fetchbuf[i + orig_shift] != INIT_GET){
                    /* Check that data between blocks has not changed */
                    errors ++;
                    fprintf(stderr, "Found %d at %d (result buf), expected %d\n",
                            fetchbuf[i + orig_shift], i + orig_shift, INIT_GET);
                }
                orig_shift++;
            }
            if(is_target_ddt){
                if(exposedbuf[i + target_shift] != INIT_EXPOSED){
                    /* Check that data between blocks has not changed */
                    errors ++;
                    fprintf(stderr, "Found %d at %d, expected %d\n",
                            exposedbuf[i + target_shift], i + target_shift, INIT_EXPOSED);
                }
                target_shift++;
            }
        }

        if( srcbuf[i] != i){
            /* Comms should never change the local buffer used to send data */
            errors ++;
            fprintf(stderr, "Byte #%d: Found %d at srcbuf[%d], expected %d\n",
                    i, srcbuf[i], i,i);
        }

        int expected = i + orig_shift;
        if( exposedbuf[i + target_shift] != expected ){
            /* Bad data written on remote buffer */
            errors ++;
            fprintf(stderr, "Byte #%d: Found %d at exposedbuf[%d], expected %d\n",
                    i, exposedbuf[i + target_shift], i + target_shift, expected);
        }
        if( fetchbuf && fetchbuf[i + orig_shift] != INIT_EXPOSED){
            /* Bad data fetched from remote buffer */
            errors ++;
            fprintf(stderr, "Byte #%d: Found %d at fetch(%p)[%d], expected %d\n",
                    i, fetchbuf[i + orig_shift], fetchbuf, i + orig_shift, INIT_EXPOSED);
        }
    }

    return errors;
}


static int run_test_ddt(int rank, enum WINDOW type, enum op commtype,
                        int is_origin_ddt, int is_target_ddt,
                        int use_requests)
{
    int peer = (rank + 1) % 2;
    MPI_Datatype my_vector;
    win_and_memory_allocate(rank, (void **)&mybuf, (void **)&remote_buff,
                            BUFSIZE * sizeof(int), type, &win);

    if( commtype == GET_ACCUMULATE){
        /* Comms tha need a third buffer */
        int page_size = getpagesize();
        assert(page_size <= MAX_ALIGNMENT);
        MEM_CHECK(posix_memalign((void**)&get_buffer, page_size, BUFSIZE * sizeof(int)));
    }

    if(!rank && verbose){
        /* Prints header */
        fprintf(stderr, "=============================\n");
        fprintf(stderr, "%s%s %s to %s\n",
                (use_requests? "R": ""), op_names[commtype],
                (is_origin_ddt ? "vector": "contiguous"),
                (is_target_ddt ? "vector": "contiguous"));
        fprintf(stderr, "=============================\n");
        fprintf(stderr, "%10s Status\n", "Bytes");
    }

    /* Test all sizes */
    for (int size = min_size; size <= max_size; size = (size ? size*2: 1)){

        int bsize = size/n_block;
        int origin_dt_count, target_dt_count;
        MPI_Datatype origin_dt, target_dt;

        /* Initialize buffer to distinct values */
        switch(commtype){
            case GET_ACCUMULATE:
                for(int i = 0; i <= (bsize + 1) * n_block -1; ++i){
                    /* Just reset the data range used by this size */
                    get_buffer[i] = INIT_GET;
                }
                /* GetAccumulate is an accumulate with a fetch before */
                __attribute__ ((fallthrough));
            case ACCUMULATE:
                /* Accumulate is an put with an operator */
                __attribute__ ((fallthrough));
            case PUT:
                for(int i = 0; i <= (bsize + 1) * n_block -1; ++i){
                    /* Just reset the data range used by this size */
                    mybuf[i] = i;
                    remote_buff[i] = INIT_EXPOSED;
                }
                break;
            case GET:
                for(int i = 0; i <= (bsize + 1) * n_block -1; ++i){
                    /* Just reset the data range used by this size */
                    mybuf[i] = INIT_EXPOSED;
                    remote_buff[i] = i;
                }
                break;
        }

        /* Init Datatypes */
        MPI_Type_vector(n_block, bsize, bsize + 1, MPI_INT, &my_vector);
        MPI_Type_commit(&my_vector);

        MPI_CHECK(MPI_Win_lock_all(0, win));

        if(is_target_ddt){
            target_dt = my_vector;
            target_dt_count = 1;
        } else {
            target_dt = MPI_INT;
            target_dt_count = bsize * n_block;
        }

        if(is_origin_ddt){
            origin_dt = my_vector;
            origin_dt_count = 1;
        } else {
            origin_dt = MPI_INT;
            origin_dt_count = bsize * n_block;
        }

        /* Make sure the data are ready before writting new data on remote */
        MPI_Barrier(MPI_COMM_WORLD);

        do_comm(commtype,
                origin_dt_count, origin_dt,
                peer,
                target_dt_count, target_dt,
                use_requests);

        MPI_CHECK(MPI_Win_unlock_all(win));
        MPI_Type_free(&my_vector);
        MPI_Barrier(MPI_COMM_WORLD);

        /* Data checks */
        switch(commtype){
            case GET_ACCUMULATE:
            case PUT:
            case ACCUMULATE:
                if(data_check(mybuf, remote_buff, get_buffer, bsize,
                              is_origin_ddt, is_target_ddt)){
                    fprintf(stderr, "%d bytes, %d blocks KO\n", size, n_block);
                    return EXIT_FAILURE;
                }
                break;
            case GET:
                if(data_check(remote_buff, mybuf, get_buffer,
                              bsize, is_target_ddt, is_origin_ddt)){
                    fprintf(stderr, "%d bytes, %d blocks KO\n", size, n_block);
                    return EXIT_FAILURE;
                }
                break;
        }
        if(!rank && verbose){
            /* Detailed logs for verbose mode */
            fprintf(stderr, "%10d OK\n", bsize*n_block);
        }
    }

    /* Short test case summary printed for non verbose mode */
    if(!rank && ! verbose){
        fprintf(stderr, "%s%-*s: %10s to %-10s   OK \n",
                (use_requests? "R": ""),
                (use_requests? 14: 15), op_names[commtype],
                (is_origin_ddt ? "vector": "contiguous"),
                (is_target_ddt ? "vector": "contiguous")
               );
    }

    win_and_memory_free(mybuf, remote_buff, type, win);

    return EXIT_SUCCESS;
}


int main (int argc, char *argv[])
{
    int rank, nprocs, ret;

    GETENV("VERBOSE", verbose, 0);
    GETENV("NBLOCK",n_block,2);
    GETENV("MIN",min_size,1);
    GETENV("MAX",max_size,2*1024*1024);

    options.win = WIN_CREATE;
    options.sync = LOCK_ALL;
    options.bench = FUNCTIONAL_OK;

    MPI_CHECK(MPI_Init(&argc, &argv));
    MPI_CHECK(MPI_Comm_size(MPI_COMM_WORLD, &nprocs));
    MPI_CHECK(MPI_Comm_rank(MPI_COMM_WORLD, &rank));

    if ( nprocs != 2 ){
        if ( rank == 0 ){
            fprintf(stderr, "This test requires exactly 2 processes. (call on %d processes)\n",
                    nprocs);
        }
        exit(1);
    }

    if(!rank){
        fprintf(stderr,"Test %d block ddt from %d to %d bytes\n", n_block,min_size,max_size);
    }

    /* Check all osc communications */
    for (int commtype = 0; commtype < NCOMMTPYE; ++ commtype){
        if(!rank){
            fprintf(stderr, "=============== %s ==============\n", op_names[commtype]);
        }
        /* Check contigous and non-contiguous types on source */
        for (int orig_ddt = 0; orig_ddt < 2; ++ orig_ddt){
            /* Check contigous and non-contiguous types on target */
            for (int targ_ddt = 0; targ_ddt < 2; ++ targ_ddt){
                /* Check requested and non requested communications */
                for (int use_requests = 0; use_requests < 2; ++ use_requests){
                    ret = run_test_ddt(rank, options.win, commtype,
                                       orig_ddt, targ_ddt, use_requests);
                    if(ret != EXIT_SUCCESS){
                        return ret;
                    }
                }
            }
        }
    }

    MPI_CHECK(MPI_Finalize());

    return ret;
}
