/* $Id: */
/*************************************************************************
 * Copyright (c) 2005, The Regents of the University of California
 * All rights reserved.
 *
 * Copyright 2005. The Regents of the University of California. 
 * This software was produced under U.S. Government contract W-7405-ENG-36 for 
 * Los Alamos National Laboratory (LANL), which is operated by the University 
 * of California for the U.S. Department of Energy. The U.S. Government has 
 * rights to use, reproduce, and distribute this software.  
 * NEITHER THE GOVERNMENT NOR THE UNIVERSITY MAKES ANY WARRANTY, EXPRESS OR 
 * IMPLIED, OR ASSUMES ANY LIABILITY FOR THE USE OF THIS SOFTWARE.  If 
 * software is modified to produce derivative works, such modified software 
 * should be clearly marked, so as not to confuse it with the version available
 * from LANL.
 * Additionally, redistribution and use in source and binary forms, with or 
 * without modification, are permitted provided that the following conditions 
 * are met:
 *      Redistributions of source code must retain the above copyright notice, 
 *      this list of conditions and the following disclaimer. 
 *      Redistributions in binary form must reproduce the above copyright 
 *      notice, this list of conditions and the following disclaimer in the 
 *      documentation and/or other materials provided with the distribution. 
 *      Neither the name of the University of California, LANL, the U.S. 
 *      Government, nor the names of its contributors may be used to endorse 
 *      or promote products derived from this software without specific prior 
 *      written permission. 
 * THIS SOFTWARE IS PROVIDED BY THE UNIVERSITY AND CONTRIBUTORS "AS IS" AND 
 * ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE 
 * IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE 
 * ARE DISCLAIMED. IN NO EVENT SHALL THE UNIVERSITY OR CONTRIBUTORS BE LIABLE 
 * FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL 
 * DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR 
 * SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER 
 * CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT 
 * LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY 
 * OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH 
 * DAMAGE.
*************************************************************************/
/*************************************************************************
 This programs combines MPI and posix threads to emulate many processes
 creating, writing/reading, and closing plfs files (N to N). Each one i
 of these events is timed and reported.  Use --help to see available command 
 line options.  e.g.  ./nton_plfs_thread_db.x  --help 

 Sample command examples:

 write:
 mpirun -np 32 ./nton_plfs_thread_db.machine.x --write --barriers bopen,bwrite
  --blocks 1 --experiment test_run --blocksize 32768 --numthreads 100 
  --nodb --experiment test-2-backends --target /var/tmp/plfs.user/user/out

 read:
 mpirun -np 32 ./nton_plfs_thread_db.machine.x --read --barriers bopen,bread
  --blocks 1 --experiment test_run --blocksize 32768 --numthreads 100 
  --nodb --experiment test-2-backends --target /var/tmp/plfs.user/user/out
 
 The original author was Gary Grider with modifications made by 
 Alfred Torrez (atorrez@lanl.gov).

*************************************************************************/
#define _GNU_SOURCE

#include <getopt.h>
#include <stdio.h>
#include <pthread.h>
#include <sys/time.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <fcntl.h>
#include <unistd.h>
#include <stdlib.h>
#include <string.h>
#include <mpi.h>
#include "utilities.h"

#include "plfs.h"

//#define MAX_NUM_THREADS 131072
#define MAX_NUM_THREADS 200 
#define PATHLEN 2050 
#define PATHSIZE_PLUS 2050
#define MB 1048576.0 

#define PLFS_DEBUG_ON 1

void *worker(void *);   /* thread routine */
//void find_min_max(double time_value, enum op_type operation, struct io_time_limits *io_time_lim);
//int i;


// Structure returned by thread
struct io_time 
{
   double open_time;
   double rw_time;
   double close_time;
};
 
enum op_type
{
   OPEN,
   RW,
   CLOSE
};

// Structure used by rank to calculate running min/max/avg
struct io_time_limits 
{
   double open_time_min;
   double open_time_max;
   double open_time_sum;
   double rw_time_min;
   double rw_time_max;
   double rw_time_sum;
   double close_time_min;
   double close_time_max;
   double close_time_sum;
};


// Structure passed to thread
struct thread_defs {
     char beginning_path[PATHSIZE_PLUS];
     int blocks;
     int blocksize;
     int bopen;
     int brw;
     int brweach;
     int bclose;
     int wflag;
     int rank;
     int id;
};

pthread_barrier_t barrier1; 
pthread_barrierattr_t attr;

struct Parameters params;

// Function prototypes
void usage1(int rank);
void parse_cmdop();
void delarg();
void find_min_max(double time_value, enum op_type operation, struct io_time_limits *io_time_lim);
void print_results(struct io_time_limits *limits_ptr, struct time_info *completed_times, struct Parameters *params, double total_time_diff, double total_data, int rank, int nproc, double aggregate_rate);
void handle_reduce_error(char *reduce_type);


int main(int argc, char *argv[])
{
    int nproc;
    char hname[128];
    int i;
    double tot_in_time, tot_out_time, tot_diff_time;
    double code_overhead_in_time,code_overhead_out_time;
    double code_overhead_diff_time = 0;
    double tot_data , tot_rate, agg_rate; 
    double max_rw_time, min_rw_time;
    double reducevar,max_total_time,min_total_time;
    double open_time_sum,close_time_sum,rw_time_sum = 0; 
    pthread_t tid[MAX_NUM_THREADS];      /* array of thread IDs */

    int rank;
    int provided;
    int init_return;
    int mpi_ret;

    struct time_info complete_time;
    struct State state;
    struct thread_defs thread_defs_array[MAX_NUM_THREADS];
//  struct Parameters params;

    if ( MPI_Init_thread(&argc, &argv, MPI_THREAD_MULTIPLE, &provided) != MPI_SUCCESS ) {
        fprintf( stderr, "ERROR: Unable to initialize MPI (MPI_Init).\n");
        return -1; 
    } 
    mpi_ret = MPI_Errhandler_set( MPI_COMM_WORLD, MPI_ERRORS_RETURN );
    if ( mpi_ret != MPI_SUCCESS ) {
        fprintf( stderr, "MPI_Errhandler set.\n");
        return -2;
    }

/*
if(rank==0 && provided<MPI_THREAD_MULTIPLE){
        printf("\nThe thread safety level supported is %d which is not the desired level %d\n",provided, MPI_THREAD_MULTIPLE);
}
*/
    if ( MPI_Comm_rank(MPI_COMM_WORLD, &rank) != MPI_SUCCESS ) {
        fprintf( stderr, "ERROR:  Cant get processor rank.\n");
        return -1;
    }
    if ( MPI_Comm_size(MPI_COMM_WORLD, &nproc) != MPI_SUCCESS ) {
        fprintf( stderr, "ERROR: Problem getting number of processors " 
                 " in MPI_COMM_WORLD.\n" );
        return -1;
    }

    gethostname(hname, 127);
    if (rank == 0) fprintf(stderr,"proc started host %s nproc %d rank %d arg1 %s\n",hname,nproc,rank,argv[1]);

    init_return = init( argc, argv, &params, &complete_time, &state, &thread_defs_array, rank, nproc);
    if ( init_return == -1 ) {
        MPI_Finalize();
        return -1;
    }

    if (rank == 0 ) fprintf(stderr,"starting -- nprocs %d path %s blocks %d blocksize %d wflag %d nthreads %d numfiles %d\n",
                          nproc,params.tfname, params.num_objs,params.obj_size,params.write_only_flag,
                          params.numthreads,params.numthreads*nproc);

    pthread_barrier_init(&barrier1, &attr, params.numthreads);

    tot_in_time = MPI_Wtime();

    // Create thread passing in thread_def_array address
    for ( i = 0; i < params.numthreads; i++) {
        pthread_create(&tid[i], NULL, worker, (void *) &thread_defs_array[i]);
    }
    struct io_time *times;
    struct io_time_limits min_max_times;
    struct io_time_limits *limits_ptr = &min_max_times;
    enum op_type operation;

    for ( i = 0; i < params.numthreads; i++) {
        pthread_join(tid[i], (void *)&times);

        code_overhead_in_time = MPI_Wtime();
     // save thread results 
        if (i == 0) {
            limits_ptr->open_time_min = times->open_time;
            limits_ptr->open_time_max = times->open_time;
            limits_ptr->close_time_min = times->close_time;
            limits_ptr->close_time_max = times->close_time;
            limits_ptr->rw_time_min = times->rw_time;
            limits_ptr->rw_time_max = times->rw_time;
            limits_ptr->open_time_sum = 0;
            limits_ptr->close_time_sum = 0;
            limits_ptr->rw_time_sum = 0;
        }
     // find running max, min, and sum for threads of this rank
        operation = OPEN;
        find_min_max(times->open_time, operation, limits_ptr);
        operation = CLOSE;
        find_min_max(times->close_time, operation, limits_ptr);
        operation = RW;
        find_min_max(times->rw_time, operation, limits_ptr);
 
        limits_ptr->open_time_sum += times->open_time;
        limits_ptr->close_time_sum += times->close_time;
        limits_ptr->rw_time_sum += times->rw_time;
        code_overhead_out_time = MPI_Wtime();
        code_overhead_diff_time += (code_overhead_out_time - code_overhead_in_time);
    }
    tot_out_time = MPI_Wtime();

    tot_diff_time=(tot_out_time-code_overhead_diff_time)-tot_in_time;


    tot_data = (unsigned long long int) params.num_objs * (unsigned long long int) params.obj_size * params.numthreads;
    tot_rate = tot_data/tot_diff_time;
    agg_rate = (tot_data*nproc)/tot_diff_time;
    state.total_mbs = (tot_data * nproc)/1048576.0;

    // Calling print results will determine max, mins, and averages for each rank
    print_results(&min_max_times, &complete_time, &params, tot_diff_time, tot_data, rank, nproc, agg_rate);

  // insert write results into database
    if ( params.use_db ) {
        if ( params.write_only_flag ) {
            params.thread_write=1;
            db_insert ( state.my_rank, 0, NULL, &params, &state, NULL, NULL, &complete_time); 
        }

  // insert write results into database
        else {
            params.thread_write=0;
            db_insert ( state.my_rank, 0, NULL, &params, &state, NULL, NULL, &complete_time); 
        }
  // reset the partial to NULL if no errors (mysql handles this)
        db_insert(state.my_rank, 0, "NULL", &params, &state, NULL, NULL, NULL );
    }


    fflush(stdout);
    MPI_Finalize();
    free(times);
    return (0);
}  /* main */

/*
  This is the tread that is initiated by main.  It is responsible for opening 
  files, writing and reading files, closing files, and using barriers when
  necessary.  It is passed in a structure so that times measured for each
  event described are recorded.   
*/
void *
worker(void *arg)
{
// variable declaration
    int workernum;
    char wpath[PATHLEN];
    void * buf;
    int fd, rc, j;
    int i;
    struct thread_defs *worker_defs;

    Plfs_fd *pfd = NULL;
    Plfs_fd *pfd1 = NULL;
    int ret;
    double open_in_time, open_out_time, open_diff_time;
    double rw_in_time, rw_out_time, rw_diff_time;
    double close_in_time, close_out_time, close_diff_time;
    struct io_time *times = (struct io_time*)malloc(sizeof(struct io_time));  

    worker_defs = (struct thread_defs *) arg;

    workernum = worker_defs->id;

    // malloc memory for block write size
    sprintf(wpath,"%s.%d",worker_defs->beginning_path,workernum);
    buf = (char *) valloc(worker_defs->blocksize);
    if (buf == NULL) {
        printf("malloc failed %d\n",workernum);
        return((void *)-1);
    }

    // check if barrier before open
    if (worker_defs->bopen) {
        pthread_barrier_wait(&barrier1);
        if (workernum == 0) {
            if ( MPI_Barrier(MPI_COMM_WORLD) != MPI_SUCCESS ) {
               fprintf( stderr, "ERROR:  Unable to call MPI_Barrier before open.\n");
               MPI_Abort(MPI_COMM_WORLD, -1 ); 
               exit(8);
            }
        }
        pthread_barrier_wait(&barrier1);
    }
    
    open_in_time = MPI_Wtime();

    // open file for writing
    if (worker_defs->wflag == 1) {

        ret = plfs_open( &pfd, wpath,O_CREAT | O_TRUNC | O_WRONLY , 0, 0666, NULL );
        if (ret != 0) {
            perror("openwrite");
            fprintf(stderr,"%d write open error on %s\n",worker_defs->rank,wpath);
            MPI_Abort(MPI_COMM_WORLD,-1);
            exit(8);
        }
    } 
    // open file for reading
    else {
        ret = plfs_open( &pfd, wpath, O_RDONLY, 0, 0, NULL );
        if (ret != 0) {
            perror("openread");
            fprintf(stderr,"%d read open error on %s\n",worker_defs->rank,worker_defs->beginning_path);
            MPI_Abort(MPI_COMM_WORLD,-1);
            exit(9);
        }
    }
    open_out_time = MPI_Wtime();

    // check if barrier before write or read
    if (worker_defs->brw) {
        pthread_barrier_wait(&barrier1);
        if (workernum == 0) {
            if ( MPI_Barrier(MPI_COMM_WORLD) != MPI_SUCCESS ) {
               fprintf( stderr, "ERROR:  Unable to call MPI_Barrier before write/read.\n");
               MPI_Abort(MPI_COMM_WORLD, -1 ); 
               exit(8);
            }
        }
        pthread_barrier_wait(&barrier1);
    }
    rw_in_time = MPI_Wtime();
    i = 0;
    plfs_error_t plfs_ret;
    ssize_t bytes;
    // loop to write or read blocks of data to file
    while (i < worker_defs->blocks) {
        // check if barrier before each write/read 
        if (worker_defs->brweach) {
            pthread_barrier_wait(&barrier1);
            if (workernum == 0) 
            if ( MPI_Barrier(MPI_COMM_WORLD) != MPI_SUCCESS ) { 
               fprintf( stderr, "ERROR:  Unable to call MPI_Barrier before each write/read.\n");
               MPI_Abort(MPI_COMM_WORLD, -1 ); 
               exit(8);
            }
            pthread_barrier_wait(&barrier1);
        }
        // write data to file
        if (worker_defs->wflag == 1) {
            plfs_ret = plfs_write( pfd, buf, worker_defs->blocksize, i*worker_defs->blocksize, 0, &bytes );
            if (plfs_ret != PLFS_SUCCESS || bytes != worker_defs->blocksize) {
                perror("write");
                fprintf(stderr,"%d write error \n",worker_defs->rank);
                MPI_Abort(MPI_COMM_WORLD,-1);
                exit(88);
            }
        } 
        // else, read data from file
        else {
            plfs_ret = plfs_read( pfd, buf, worker_defs->blocksize, i*worker_defs->blocksize, &bytes );
            if (plfs_ret != PLFS_SUCCESS || bytes != worker_defs->blocksize) {
                perror("read");
                fprintf(stderr,"%d read error \n",worker_defs->rank);
                MPI_Abort(MPI_COMM_WORLD,-1);
                exit(99);
            }
        }
        // increment block count
        i=i+1;
    } 
    rw_out_time = MPI_Wtime();

    // check if barrier before close
    if (worker_defs->bclose) {
        pthread_barrier_wait(&barrier1);
        if (workernum == 0) {
            if ( MPI_Barrier(MPI_COMM_WORLD) != MPI_SUCCESS ) {
               fprintf( stderr, "ERROR:  Unable to call MPI_Barrier before close.\n");
               MPI_Abort(MPI_COMM_WORLD, -1 ); 
               exit(8);
            }
        }
        pthread_barrier_wait(&barrier1);
    }

    close_in_time = MPI_Wtime();
    // close file written
    int flags;
    if (worker_defs->wflag == 1) {
        plfs_ret = plfs_close(pfd, 0, 0, O_WRONLY ,NULL, &flags);
        if (plfs_ret != PLFS_SUCCESS) {
            perror("closewrite");
            fprintf(stderr,"%d write open error on %s\n",worker_defs->rank,wpath);
            MPI_Abort(MPI_COMM_WORLD,-1);
            exit(8);
        }
    }
    // close file read
    else {
        plfs_ret = plfs_close(pfd, 0, 0, O_RDONLY ,NULL, &flags);
        if (plfs_ret != PLFS_SUCCESS) {
            perror("closeread");
            fprintf(stderr,"%d write open error on %s\n",worker_defs->rank,wpath);
            MPI_Abort(MPI_COMM_WORLD,-1);
            exit(8);
        }
    }

    pthread_barrier_wait(&barrier1);
    close_out_time = MPI_Wtime();

    // update structure with measured open, write, and close times
    times->open_time=open_out_time-open_in_time;
    times->rw_time=rw_out_time-rw_in_time;
    times->close_time=close_out_time-close_in_time;

    close(fd);
    free(buf);
    pthread_exit(times);
    return (NULL);
}

/*
  Function to print usage information
*/
void usage1(int rank)
{
    if (rank == 0) {
        fprintf(stderr, "\n");
        fprintf(stderr,"Usage: nton_mpi_threads ");
        fprintf(stderr, " --read or --write ");
        fprintf(stderr, " --blocks block_cnt");
        fprintf(stderr, " --blocksize block_size");
        fprintf(stderr, " --barriers [bopen],[bwrite],[bread],[beachread],[beachwrite],[bclose]");
        fprintf(stderr, " [ --numthreads thread_count per rank]");
        fprintf(stderr, " --target path/basefilename \n\n");
        fprintf(stderr, "--experiment experiment_tag");
        fprintf(stderr, "--nodb");
        fprintf(stderr, "--target path/basefilename \n\n");
    }
}


/*
  Function to find min, and max times for open, write/read and close operations 
*/
void find_min_max(double time_value, enum op_type operation, struct io_time_limits *io_time_lim) {
     if (operation == OPEN ) {
         if ( time_value > io_time_lim->open_time_max ) io_time_lim->open_time_max = time_value;
         if ( time_value < io_time_lim->open_time_min ) io_time_lim->open_time_min = time_value;
     }
     else if (operation == CLOSE) {
         if ( time_value > io_time_lim->close_time_max ) io_time_lim->close_time_max = time_value;
         if ( time_value < io_time_lim->close_time_min ) io_time_lim->close_time_min = time_value;
     } 
     else if (operation == RW) {
         if ( time_value > io_time_lim->rw_time_max ) io_time_lim->rw_time_max = time_value;
         if ( time_value < io_time_lim->rw_time_min ) io_time_lim->rw_time_min = time_value;
     } 
}


/*
  This function gathers program arguments, initializes variables, and calls utility functions
  shared with fs_test to initilize the database interface.
*/
int init( int argc, char **argv, struct Parameters *params, struct time_info *write_read_times, 
          struct State *state, struct thread_defs *threads_array,
          int my_rank, int nproc)
{
    int mpi_ret;
    int myhost_num;
    int i;
    char my_host[MPI_MAX_PROCESSOR_NAME];
    char target_path[PATHSIZE_PLUS];
    int target_path_specified = 0;


    // define function arguments
    struct option longopts[] = {
      { "barriers",   required_argument,  0, 'a'},
      { "write",      no_argument,        0, 'b'},
      { "read",       no_argument,        0, 'c'},
      { "blocksize",  required_argument,  0, 'd'},
      { "blocks",     required_argument,  0, 'e'},
      { "numthreads", required_argument,  0, 'f'},
      { "target",     required_argument,  0, 'g'},
      { "help",       no_argument,        0, 'h'},
      { "experiment", required_argument,  0, 'i'},
      { "nodb",       no_argument,        0, 'j'},
      { 0 }
    };


    int option_index = 0;
    int c;

    //  initalize appropriate structures
    memset( params, 0, sizeof( *params ) );
    memset( write_read_times, 0, sizeof( *write_read_times ) );
    memset( state, 0, sizeof( *state ) );
    params->use_db              = 1;

    state->efptr    = stderr;
    state->ofptr    = stdout;
    state->pagesize = getpagesize(); 

    // determin program arguments
    while ((c = getopt_long(argc, argv, "a:bcd:e:f:g:", longopts, &option_index)) != -1) {
         switch (c) {
             case 'a':
                 params->barriers = strdup( optarg );
                 break;
             case 'b':
                 params->write_only_flag = 1;
                 break;
             case 'c':
                 params->write_only_flag = 0;
                 break;
             case 'd':
                 params->obj_size = atoi( optarg );
                 break;
             case 'e':
                 params->num_objs = atoi( optarg );
                 break;
             case 'f':
                 params->numthreads = atoi( optarg );
                 break;
             case 'g':
                 snprintf(target_path, strlen(optarg)+sizeof(my_rank)+2, "%s.%d", optarg,my_rank);
                 target_path_specified = 1;
                 break;
             case 'h':
                 usage1(my_rank);
                 return(-1);
                 break;
             case 'i':
                 params->experiment = strdup( optarg );
                 break;
             case 'j':
                 params->use_db = 0;
                 break;
             default:
                fatal_error( state->efptr, state->my_rank, 0, NULL, 
                "Problem with command line arguments.\n" );
         } 
    } 

    // determine validity of program arguments
    if ( !target_path_specified ) {
        if (my_rank == 0) {
            fprintf(stderr,"target path must be specified\n");
            usage1(my_rank);
        }
        return -1;
    }
    params->tfname = strdup(target_path);

    if ( params->num_objs < 0 ) {
        if (my_rank == 0) {
            fprintf(stderr,"block must be > -1 \n");
            usage1(my_rank);
        }
        return -1;
    }

    if (params->obj_size < 1) {
        if (my_rank == 0) {
            fprintf(stderr,"blocksize must be > 0 \n");
            usage1(my_rank);
        }
        return -1;
    }

    int bopen=0;
    int brw=0;
    int brweach=0;
    int bclose=0;

    //set local copy of barriers from params structure
    if (params->barriers !=NULL ) {
       if ( strstr( params->barriers, "bopen" ) != NULL ) bopen=1;
       if ( strstr( params->barriers, "bread" ) != NULL ) brw=1;
       if ( strstr( params->barriers, "bwrite" ) != NULL ) brw=1;
       if ( strstr( params->barriers, "beachread" ) != NULL ) brweach=1;
       if ( strstr( params->barriers, "beachwrite" ) != NULL ) brweach=1;
       if ( strstr( params->barriers, "bclose" ) != NULL ) bclose=1;
    }

    // Initialize input thread input structure with program arguments
    // including barriers
    for (i = 0; i < MAX_NUM_THREADS; i++) {
        threads_array[i].bopen = bopen;
        threads_array[i].brw = brw;
        threads_array[i].brweach = brweach;
        threads_array[i].bclose = bclose;
        threads_array[i].wflag = params->write_only_flag;
        threads_array[i].blocks = params->num_objs;
        threads_array[i].blocksize = params->obj_size;
        threads_array[i].rank = my_rank;
        threads_array[i].id = i;
        strncpy(threads_array[i].beginning_path, params->tfname, PATHSIZE_PLUS );
    }

    params->totalthreads = params->numthreads*nproc;
    params->num_procs_world = nproc;

    state->my_rank = my_rank;

    // get other info for the database
    if ( params->tmpdirname == NULL ) {
        params->tmpdirname = "/users/atorrez/tmp";
    }
    // call utilities procedure to gather additional information for database
    // entry
    collect_additional_config( params, state->my_rank, argc, argv );
    if(!print_input_environment(state->my_rank, 
                  myhost_num, 
                  my_host,
                  params,
			      state->ofptr, 
                  state->efptr))
    {
        fatal_error( state->efptr, state->my_rank, 0, NULL, 
              "Problem detected printing input and envirnoment variables.\n" );
    }
    if ( params->use_db ) {
        db_insert( state->my_rank, 0, "partial", params, state, NULL, NULL, NULL );
    }
    return 0;
}

/*
  This function print out a single time as defined by parent function: print results 
*/
void print_single_time(char *time_string, double value, struct Parameters *params, int my_rank, double *write_time, double *read_time, int avg )
{
    double print_value = value;

    if (avg) {
        print_value=value/params->totalthreads;
    }
    if (my_rank == 0) {
        fprintf(stdout,"%s %f\n",time_string, print_value);
        if ( params->write_only_flag ) {
            *write_time = print_value;
        }
        else {
            *read_time = print_value;
        }
    }
}

/*
   This function prints all results.  It uses MPI_Reduce to gather min and max 
   as well as to sum results for an average calculation
*/

void print_results(struct io_time_limits *limits_ptr, struct time_info *completed_times, struct Parameters *params, double total_time_diff, double total_data, int rank, int nproc, double aggregate_rate) 
{

    double reducevar,max_total_time,min_total_time;
    double max_rw_time, min_rw_time;
    int do_avg = 0;

// open times   
    if (rank == 0) fprintf(stdout,"\n");

    // find max
    if ( MPI_Reduce(&limits_ptr->open_time_max, &reducevar, 1, MPI_DOUBLE,
                     MPI_MAX, 0, MPI_COMM_WORLD) != MPI_SUCCESS ) {
        handle_reduce_error("max open");
    }
    print_single_time("max open time", reducevar, params, rank, 
                      &completed_times->write_file_open_wait_time_max,
                      &completed_times->read_file_open_wait_time_max, do_avg );


    // find min
    if ( MPI_Reduce(&limits_ptr->open_time_min, &reducevar, 1, MPI_DOUBLE, 
                    MPI_MIN, 0, MPI_COMM_WORLD) != MPI_SUCCESS ) {
        handle_reduce_error("min open");
    }
    print_single_time("min open time", reducevar, params, rank, 
                      &completed_times->write_file_open_wait_time_min,
                      &completed_times->read_file_open_wait_time_min, do_avg );

    // get sum 
    if ( MPI_Reduce(&limits_ptr->open_time_sum, &reducevar, 1, MPI_DOUBLE, 
         MPI_SUM, 0, MPI_COMM_WORLD) != MPI_SUCCESS ) {
        handle_reduce_error("sum open");
    }
    do_avg = 1;
    print_single_time("average open time", reducevar, params, rank, 
                      &completed_times->write_file_open_wait_time,
                      &completed_times->read_file_open_wait_time, do_avg );

  // read/write time
    do_avg = 0;
    if (rank == 0) fprintf(stdout,"\n");

    // find max
    if ( MPI_Reduce(&limits_ptr->rw_time_max, &reducevar, 1, MPI_DOUBLE, 
                    MPI_MAX, 0, MPI_COMM_WORLD) != MPI_SUCCESS) {
        handle_reduce_error("max read/write");
    }
    max_rw_time=reducevar;
    print_single_time("max rw time", reducevar, params, rank, 
                      &completed_times->write_total_op_time_max,
                      &completed_times->read_total_op_time_max, do_avg );

    // find min
    if ( MPI_Reduce(&limits_ptr->rw_time_min, &reducevar, 1, MPI_DOUBLE, 
         MPI_MIN, 0, MPI_COMM_WORLD) != MPI_SUCCESS ) {
        handle_reduce_error("min read/write");
    }
    min_rw_time=reducevar;
    print_single_time("min rw time", reducevar, params, rank, 
                      &completed_times->write_total_op_time_min,
                      &completed_times->read_total_op_time_min, do_avg );

    // get sum
    do_avg = 1;
    if ( MPI_Reduce(&limits_ptr->rw_time_sum, &reducevar, 1, MPI_DOUBLE, MPI_SUM, 
                    0, MPI_COMM_WORLD) != MPI_SUCCESS) {
        handle_reduce_error("sum read/write");
    }
    print_single_time("average rw time", reducevar, params, rank, 
                      &completed_times->write_total_op_time,
                      &completed_times->read_total_op_time, do_avg );


// close times
    do_avg = 0;
    if (rank == 0) fprintf(stdout,"\n");
    if ( MPI_Reduce(&limits_ptr->close_time_max, &reducevar, 1, MPI_DOUBLE, 
                    MPI_MAX, 0, MPI_COMM_WORLD) != MPI_SUCCESS ) {
        handle_reduce_error("max close");
        fprintf( stderr, "ERROR:  Unable to reduce max for close.\n");
        MPI_Abort(MPI_COMM_WORLD, -1 ); 
    }
    print_single_time("max close time", reducevar, params, rank, 
                      &completed_times->write_file_close_wait_time_max,
                      &completed_times->read_file_close_wait_time_max, do_avg );

    if ( MPI_Reduce(&limits_ptr->close_time_min, &reducevar, 1, MPI_DOUBLE, 
                    MPI_MIN, 0, MPI_COMM_WORLD) != MPI_SUCCESS ) {
        handle_reduce_error("min close");
    }
    print_single_time("min close time", reducevar, params, rank, 
                      &completed_times->write_file_close_wait_time_min,
                      &completed_times->read_file_close_wait_time_min, do_avg );

    do_avg = 1;
    if ( MPI_Reduce(&limits_ptr->close_time_sum, &reducevar, 1, MPI_DOUBLE,
                    MPI_SUM, 0, MPI_COMM_WORLD) != MPI_SUCCESS ) {
        handle_reduce_error("sum close");
    }
    print_single_time("average close time", reducevar, params, rank, 
                      &completed_times->write_file_close_wait_time,
                      &completed_times->read_file_close_wait_time, do_avg );

  //total_time
    do_avg = 0;
    if (rank == 0) fprintf(stdout,"\n");
    if ( MPI_Reduce(&total_time_diff, &reducevar, 1, MPI_DOUBLE, 
                    MPI_MAX, 0, MPI_COMM_WORLD) != MPI_SUCCESS ) {
        handle_reduce_error("max total time");
    }
    max_total_time=reducevar;
    print_single_time("max tot time", reducevar, params, rank, 
                      &completed_times->write_total_time_max,
                      &completed_times->read_total_time_max, do_avg );

    if ( MPI_Reduce(&total_time_diff, &reducevar, 1, MPI_DOUBLE, 
                    MPI_MIN, 0, MPI_COMM_WORLD) != MPI_SUCCESS ) {
        handle_reduce_error("min total time");
    }
    min_total_time=reducevar;
    print_single_time("min tot time", reducevar, params, rank, 
                      &completed_times->write_total_time_min,
                      &completed_times->read_total_time_min, do_avg );

    if (rank == 0) fprintf(stdout,"\n");
    if (rank == 0) fprintf(stdout,"max effective data rate %f MB/sec\n",
                   ((total_data*nproc)/min_total_time)/MB);
    if (rank == 0) fprintf(stdout,"min effective data rate %f MB/sec\n",
                   ((total_data*nproc)/max_total_time)/MB);
    if (rank == 0) fprintf(stdout,"\n");
    if (rank == 0) fprintf(stdout,"max raw data rate %f MB/sec\n",
                   ((total_data*nproc)/min_rw_time)/MB);
    if (rank == 0) fprintf(stdout,"min raw data rate %f MB/sec\n",
                   ((total_data*nproc)/max_rw_time)/MB);


    if (rank == 0) fprintf(stdout,"\n");
    MPI_Reduce(&aggregate_rate, &reducevar, 1, MPI_DOUBLE, MPI_MAX, 0, MPI_COMM_WORLD);
    if (rank == 0) fprintf(stdout,"aggregate data rate %f\n",reducevar);
    if (rank == 0) fprintf(stdout,"\n");

}

/* 
   This function is used to print out an error and MPI_Abort when a reduce
   error occurs
*/ 
void handle_reduce_error(char *reduce_type) 
{
    char *error_message = "ERROR: Unable to reduce for ";

    snprintf(error_message, strlen(error_message) + strlen(reduce_type) +1, "%s%s", reduce_type, "\n" ); 
    fprintf( stderr, error_message);
    MPI_Abort(MPI_COMM_WORLD, -1 ); 
}
