/****************************************************************
* AUTHOR:    John Bent 
* DATE:      July 10, 2006
*
*      LOS ALAMOS NATIONAL LABORATORY
*      An Affirmative Action/Equal Opportunity Employer
*
* Copyright (c) 2005
* the Regents of the University of California.
*
* Unless otherwise indicated, this information has been authored by an
* employee or employees of the University of California, operator of the Los
* Alamos National Laboratory under Contract No. W-7405-ENG-36 with the U. S.
* Department of Energy. The U. S. Government has rights to use, reproduce, and
* distribute this information. The public may copy and use this information
* without charge, provided that this Notice and any statement of authorship
* are reproduced on all copies. Neither the Government nor the University
* makes any warranty, express or implied, or assumes any liability or
* responsibility for the use of this information.
******************************************************************/

#ifndef   __UTILITIES_H_INCLUDED
#define   __UTILITIES_H_INCLUDED

#define MPI_IO_TEST_VERSION "1.00.012"

#define O_CONCURRENT_WRITE 020000000000
#define TRUE 1
#define FALSE 0

#define WRITE_MODE 1
#define READ_MODE 0
#define INIT_MODE 2

#define BIG_FILE_OFFSET 8

#ifdef __cplusplus
extern "C" {
#endif

enum
IOType {
     IO_MPI, IO_POSIX, IO_PLFS 
};

#include <getopt.h>
#include <stdio.h>
#include <stddef.h>
#include <unistd.h>
#include <errno.h>
#include <sys/types.h>

#ifdef HAS_PLFS
#include "plfs.h"
#endif

#include "hashtable.h"
#include "hashtable_itr.h"


extern struct Parameters params;

struct myoption {
    const char *name;
    int  has_arg;
    int *flag;
    int val;
    char *help;
};

/* Note that in fs_test.c's "init" function, memset is used to set all
 * values of this structure to 0 (zero).
 */

struct time_values {
    double file_open_wait_time; 
    double file_open_wait_start_time;
    double file_open_wait_end_time;
    double file_open_wait_elapsed_time;
    double file_close_wait_time; 
    double file_close_wait_start_time;
    double file_close_wait_end_time;
    double file_close_wait_elapsed_time;
    double file_stat_wait_time; 
    double file_stat_wait_start_time;
    double file_stat_wait_end_time;
    double file_stat_wait_elapsed_time;
    double barrier_wait_time; 
    double barrier_wait_start_time; 
    double barrier_wait_end_time; 
    double barrier_wait_elapsed_time;
    double file_sync_wait_time; 
    double file_sync_wait_start_time; 
    double file_sync_wait_end_time; 
    double file_sync_wait_elapsed_time; 
    double file_op_wait_time;
    double file_op_wait_start_time;
    double file_op_wait_end_time;
    double file_op_wait_elapsed_time;
    double total_op_time;
    double total_op_start_time;
    double total_op_end_time;
    double total_op_elapsed_time;
    double total_time;
    double total_start_time;
    double total_end_time;
    double total_elapsed_time;
    double stat_time;
    double stat_start_time;
    double stat_end_time;
    double stat_elapsed_time;
    double unlink_time;
    double unlink_start_time;
    double unlink_end_time;
    double unlink_elapsed_time;
    double plfs_flatten_time;
};

struct read_error {
    off_t file_offset;
    off_t length;
    int   all_zeros;
    char  *expected;
    char  *received;
    struct read_error *next;
    struct read_error *prev;
    off_t str_length;
};

/* Note that in fs_test.c's "init" function, memset is used to set all
 * values of this structure to 0 (zero).
 */

struct State {
/* @@@
 * If we ever implement a mode where N-N I/O can have each process operate
 * on more than one file, we may need to have an array of file handles
 * whose size won't be known until we know how many files each process
 * will use.
 *
    int *fds;
    MPI_File *mpi_fhs;
*/
    int fd;
    char *error_string;
    MPI_File mpi_fh;
    FILE *ofptr;
    FILE *efptr;
    int pagesize;
    int my_rank;
    unsigned objs_written;
    unsigned *objs_written_all;
    double total_mbs;
    unsigned min_objs;
    unsigned max_objs;
    unsigned ave_objs;
    uid_t uid;
    #ifdef HAS_PLFS
/* @@@
 * If we ever implement a mode where N-N I/O can have each process operate
 * on more than one file, we may need to have an array of file handles
 * whose size won't be known until we know how many files each process
 * will use.
 *
    Plfs_fd **plfs_fds;
*/
    Plfs_fd *plfs_fd;
    #endif
};

/* Note that in fs_test.c's "init" function, memset is used to set all
 * values of this structure to 0 (zero).
 */

struct Parameters {
  int    test_type;
  long   test_time; 
  size_t num_objs;
  size_t obj_size;
  size_t max_offset;
  size_t max_size;
  int superblocks;
  int supersize;
  MPI_Info info;
  int    info_allocated_flag;
  int    shift_flag;
  int    rank_flag;
  int    plfs_flatten;
  char   *mpiversion;
  char   *barriers;         /* User flag about when to barrier */
  char   *db_key;
  char   *datestr;
  char   *system;
  char   *hints;
  char   *efname;
  char   *ofname;
  char   *tfname;
  char   *user;
  char   *experiment;       /* User flag to give name to DB to group runs*/
  char   *full_args;        /* complete set of command line args */
  char   *full_hints;       /* complete set of mpi-io hints */
  char   *host_list;
  float  procs_per_node;
  char   *panfs_srv;
  char   *os_version;
  int    error_rank;
  int    *io_procs_list;
  int    touch;
  int    strided_flag;
  int    num_procs_world;   /* how many procs are in this program? */
  int    write_only_flag;  /* Write target file only                */
  int    read_only_flag;   /* Read target file only                 */
  int    check_data_ndx;
  int    sync_flag;        /* Sync the data to file before close       */
  int    sleep_seconds;    /* Amount of time to slepp between write & read */
  int    truncate_flag;        /* Flag denoting truncate the target file     */
  int    stat_flag;         /* whether to stat the file before close */
  int    delete_flag;          /* Flag denoting delete the target file     */
  int    collective_flag;      /* Use collective read/write calls */
  int    verbose_flag;         /* Print times for all processes           */
  int    trenddata;            /* Print output in Gazebo trend data format */
  int    time_limit;           /* whether to only run for a fixed time */
  unsigned int num_nn_dirs;    /* Number of directories for non-PLFS N-N I/O files */
  char         *nn_dir_prefix; /* The prefix of the NN directory names for distributing NN I/O */
/* @@@
 * If we ever implement a mode where N-N I/O can have each process operate
 * on more than one file, we have to know how many files each process
 * will use.
 *
  unsigned int num_nn_files;   
*/
  int panfs_info_valid;
  int panfs_type;
  int panfs_stripe;
  int panfs_width;
  int panfs_depth;
  int panfs_comps;
  int panfs_visit;
  int panfs_concurrent_write;

  int io_type;
  char *io_type_str;

  int no_extra;  // turn off the need for an FS_TEST_EXTRA script
  int use_db;   // still only on if DEFINES set in Makefile, but this allows
                // the code to be there and selectively disabled at runtime

  struct hashtable *ht;
  char   *tmpdirname;
  int    numthreads;
  int    totalthreads;
  int    thread_write;
};


// this structure is used for threaded nton runs
struct time_info
{
    double write_file_open_wait_time_max;
    double write_file_open_wait_time_min;
    double write_file_open_wait_time;
    double write_total_op_time_max;
    double write_total_op_time_min;
    double write_total_op_time;
    double write_file_close_wait_time_max;
    double write_file_close_wait_time_min;
    double write_file_close_wait_time;
    double write_total_time_max;
    double write_total_time_min;
    double write_total_time;
    double read_file_open_wait_time_max;
    double read_file_open_wait_time_min;
    double read_file_open_wait_time;
    double read_total_op_time_max;
    double read_total_op_time_min;
    double read_total_op_time;
    double read_file_close_wait_time_max;
    double read_file_close_wait_time_min;
    double read_file_close_wait_time;
    double read_total_time_max;
    double read_total_time_min;
    double read_total_time;

};


#define VALLOC(x) Valloc(x,efptr,my_rank)
#define MALLOC(x) Malloc(x,efptr,my_rank)
#define CALLOC(x,y) Calloc(x,y,efptr,my_rank)

void set_using_db( int using_db );

int get_using_db( void );

void setTime( double *value, double *value_time_start, 
        double *value_time_end, double initial, double end_time, 
        double start_time, char *name, char *file, int line, 
        FILE *efptr, int my_rank );

void *Calloc( size_t nmemb, size_t size, FILE *fp, int rank );
void *Malloc( size_t size, FILE *fp, int rank );
void *Valloc( size_t size, FILE *fp, int rank );

char printable_char( int index );
/*
 * This function fills "nn_dir_num_buf" with a 0 (zero) padded number, whose
 * total length is equal to "max_num_nn_dirs_len". The buffer should
 * be pre-allocated and have enough room for that, plus the \0 (NULL) byte at
 * the end.
 *
 * This function returns 0 (zero) on success, -1 otherwise.
 */

int make_nn_dir_num_string(
      char *nn_dir_num_buf,
      unsigned int nn_dir_num,
      size_t max_num_nn_dirs_len,
      struct State *state );

/*
 * This function allocates memory for and fills "nn_dir_buf" with the concatenation of:
 *
 * parent_dir
 * "/"
 * nn_dir_prefix
 * string representation of "nn_dir_num" that is zero-padded to "max_num_nn_dirs_len".
 *
 * This function returns 0 (zero) on success, -1 otherwise.
 *
 * The caller must free "nn_dir_buf".
 */

int make_nn_dir_string(
    char **nn_dir_buf,
    char *parent_dir,
    char *nn_dir_prefix,
    unsigned int nn_dir_num,
    size_t max_num_nn_dirs_len,
    struct State *state );

/*
 * Make the subdirectories for the N-N I/O job under "parent_dir" with the
 * base name "nn_dir_prefix" and numbers appended to that.
 *
 * This function returns 0 if successful, otherwise it prints an error and
 * returns -1.
 *
 * Don't forget to make sure nn_dir_prefix has a value before calling this.
 */

int make_nn_dirs(
      char *parent_dir,
      char *nn_dir_prefix,
      unsigned int num_nn_dirs,
      struct State *state );

/*
 * Remove the subdirectories for the N-N I/O job under "parent_dir" with the
 * base name "nn_dir_prefix" and numbers appended to that.
 *
 * This function returns 0 if successful, otherwise it prints an error and
 * returns -1.
 *
 * Don't forget to make sure nn_dir_prefix has a value before calling this.
 */

int remove_nn_dirs(
      char *parent_dir,
      char *nn_dir_prefix,
      unsigned int num_nn_dirs,
      struct State *state );

/*
 * If the user provided a value for nn_dir_prefix, this function does nothing.
 * If the user did not provide a value for nn_dir_prefix, this function sets
 * it to the default value.
 *
 * This function returns 0 if successful, otherwise it prints an error and
 * returns -1.
 */

int set_nn_dir_prefix(
      char **nn_dir_prefix,
      struct State *state );

/*
 * This function takes in path and returns a newly allocated string that is
 * the concatenation of:
 * 
 * dirname( path )
 * "/"
 * nn_dir_prefix (if NULL, then the default is "nn_dir"
 * "%d" (the expansion indicator for the N-N dir number calculated in expand_path)
 * "/"
 * basename( path )
 *
 * Also, if nn_dir_prefix is the NULL or empty string, a default value will be
 * assigned to it and returned to the caller via a call to set_nn_dir_prefix.
 */
char * expand_tfname_for_nn(
         char *path,
         char **nn_dir_prefix,
         struct State *state );

char * expand_path(
         char *str,
         long timestamp,
         unsigned int num_nn_dirs,
         struct State *state );
int parse_size(int my_rank, char *chbytes, long long int *out_value);

void fill_buf( char *check_buf, int blocksize, int rank, int i, int pagesize,
        int touch );
int  verify_buf( int rank, double *expected, double *received, char *filename, 
    long which_block, long blocksize, int touch );

int compare_double( char *file, int rank, long block, long block_off, long bs, 
	double actual, double expected );

void print_double( char *header, double one );

int get_min_sum_max(   int my_rank, 
                                double base_num, 
                                double *min, 
                                int *min_ndx,
                                double *sum, 
                                double *max, 
                                int *max_ndx, 
                                char *op,
                                FILE *ofptr, 
                                FILE *efptr);

void get_all_proc_min_max_time( int     my_rank,
                                double  rank_min_value,
                                double  rank_max_value,
                                char    *op,
                                FILE    *ofptr,
                                FILE    *efptr,
                                double  *elapsed_value);

ssize_t Pread64(int fildes, void *buf, size_t nbyte, off_t offset);
ssize_t Pwrite64(int fildes, const void *buf, size_t nbyte, off_t offset);
ssize_t Write(int fd, const void *vptr, size_t nbyte );
ssize_t Read(int fd, void *vptr, size_t nbyte );

int warning_msg( FILE *fp, int my_rank, int mpi_ret, MPI_Status *mstat,
        const char *format, ... );
int fatal_error(FILE *fp, int my_rank,  
			    int mpi_ret, MPI_Status *mstat, const char *fmt,...);
int debug_msg  ( FILE *fp, int my_rank,  const char *fmt, ... );

int get_panfs_file_info( struct Parameters *params );

void set_hostlist( struct Parameters *params, int rank );

int collect_additional_config( struct Parameters *params, int rank, int argc,
        char **argv );

void insert_panfs_info( char *key, char *value ); 

int db_insert(  int my_rank,
                int failure,
                char *error_msg,
                struct Parameters *params, 
                struct State *state,
                struct time_values *write_times,
                struct time_values *read_times,
                struct time_info *thread_times );

#ifdef __cplusplus
}
#endif

#endif 
