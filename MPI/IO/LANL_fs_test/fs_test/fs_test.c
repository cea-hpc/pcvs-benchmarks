/* $Id: fs_test.c,v 1.112 2011/11/07 15:47:03 brettk Exp $ */
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

#include <getopt.h>
#include <math.h>
#include <unistd.h>
#include <string.h>
#include <signal.h>
#include <assert.h>
#include <ctype.h>
#include <fcntl.h>
#include <sys/stat.h>
#include <sys/types.h>
#include <math.h>
#include <stdarg.h>
#include <stddef.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <stdio.h>
#include <unistd.h>
#include <time.h>
#include <limits.h>
#include <libgen.h>
#include "mpi.h"
#include "utilities.h"
#include "print.h"
#include <pthread.h>

#ifdef HAS_PLFS
#include "plfs.h"
#endif

/*
   TODO:
    read should be affected by time limit also
    pass min and max blocks when time limit used
*/

int init( int argc, char **argv, struct Parameters *params,
        struct time_values *write_times,
        struct time_values *read_times,
        struct State *state );
int read_write_file( struct Parameters *params,
            struct time_values *times,
            struct State *state,
            int read_write );
#ifdef HAS_PLFS
/*
 * This function can only be called if the following are all true:
 *   params->plfs_flatten is non-zero (true)
 *   is_plfs_target is non-zero (true)
 *   params->test_type == 2 (N-1 I/O)
 *
 * This call in parse_command_line ensures these conditions are all true:
 *   check_illogical_args( params, state, 
 *          (( params->plfs_flatten ) && ( !is_plfs_target || params->test_type != 2 )),
 *          "Can only use -flatten flag for PLFS targets using N-1 I/O" );
 */

void flatten_file( struct Parameters *,struct State *, struct time_values *);
#endif
void fini( struct Parameters *params, struct State *state );
void Usage( struct Parameters *params, struct State *state, char *field,
            char *missing );
void print_time(const char *what, struct State *state);

// global so can be externed for more useful error messages outside this file
struct Parameters params;       /* Parameters passed from user   */ 

// HEY!  These are sorted by the flag.  Please maintain. 




struct myoption mylongopts[50]; 

void init_opt()
{
	struct myoption localopt[] = {	{ "barriers",   required_argument,  NULL,                            'b',
					"When to barrier.  Comma seperated.\n"
					"\te.g. -barriers bopen,aopen,bwrite,awrite,bread,aread,bclose,bsync,async,btrunc,atrunc,bstat,astat,aclose" },
					{ "check",      required_argument,  NULL,                           'C',
						"0 don't fill buffer nor verify, 1 check first byte in each block,\n"
						"\t2 check first byte in each page, 3 check every byte"           },
					{ "num_nn_dirs", required_argument,  NULL,                           'd',
						"For non-PLFS N-N I/O, number of subdirectories for files"        },
					{ "errout",     required_argument,  NULL,                            'e',
						"The path to the error file"                                        },
					{ "tmpdirname", required_argument, NULL,                            'f',
						"The path for writing temporary files"                              },
					#ifdef HAS_PLFS
					{ "flatten",      required_argument,  NULL,                          'F',
						"Whether to flatten the index of a plfs file after writing"         },
					#endif
					{ "target",     required_argument,  NULL,                           'g',
						"The path to the data file"                                         },
					{ "hints",      required_argument,  NULL,                           'h',
						"Any hints to pass to MPI such as panfs_concurrent_write (x=y[,a=b])" },
					{ "help",       optional_argument,  NULL,                           'H',
						"General help or help about a specific argument"                    },
					{ "io",         required_argument,  NULL,                           'i',
						"Whether to use POSIX, MPI, or PLFS IO routines (mpi|posix|plfs)"   },
					{ "time",       required_argument,  NULL,                           'l',
						"Whether to only run for a set time limit (in seconds)"             },
					{ "op",         required_argument,  NULL,                           'o',
						"Whether to read only (read) or write only (write)"                 },
					{ "output",     required_argument,  NULL,                           'O',
						"The path to the output file"                                       },
					{ "nobj",       required_argument,  NULL,                           'n',
						"The number of objects written/read by each proc"                   },
					{ "posix",      required_argument,  NULL,                           'P' ,
						"Whether to use posix I/O instead of MPI I/O routines"              },
					{ "sleep",      required_argument,  NULL,                           'p',
						"Number of seconds to sleep between write and read phases"          },
					{ "rank",       required_argument,  NULL,                           'r',
						"Whether to adjust the initial rank.  For 1 NP jobs only"           },
					{ "strided",    required_argument,  NULL,                           's',
						"Whether to use a strided pattern (for N-1 only)"                   },
					{ "supersize",  required_argument,  NULL,                           'S',
						"When running N-1 non-strided with time limit, must specify how "
						"many objects per superblock"                                       },
					{ "touch",      required_argument,  NULL,                           'T',
						"0 don't fill buffer nor verify, 1 touch first byte in each block,\n"
						"\t2 touch first byte in each page, 3 touch every byte"           },
					{ "trenddata",      no_argument,  &(params.trenddata),               'G',
						"Print output in Gazebo format,\n"                                  },
					{ "type",       required_argument,  NULL,                           't',
						"Whether to do N-N (1) or N-1 (2)"                                  },
					{ "experiment", required_argument,  NULL,                           'x',
						"Assign an arbitrary label to this run in the database"             },
					{ "nn_dir_prefix", required_argument,  NULL,                          'X',
						"For non-PLFS N-N I/O, prefix for subdirectories for files"        },
					{ "size",       required_argument,  NULL,                           'z',
						"The size of each object"                                           },
					{ "maxoff",       required_argument,  NULL,                           'M',
						"Set a maximum offset to force IO's to wrap"                        },
					{ "maxsize",       required_argument,  NULL,                           'Z',
						"Set a maximum file size as an exit condition"                      },
					{ "statfile", no_argument,        &(params.stat_flag),           1,
						"Whether to query the file size before close"                       },
					{ "deletefile", no_argument,        &(params.delete_flag),           1,
						"Whether to delete the file before exiting"                         },
					{ "truncatefile", no_argument,        &(params.truncate_flag),           1,
						"Whether to truncate the file before exiting"                         },
					{ "sync",       no_argument,        &(params.sync_flag),             1,
						"Whether to sync the file after writing"                            },
					{ "nodb",       no_argument,        &(params.use_db),                0,
						"Turn off the database code at runtime"                             },
					{ "noextra",    no_argument,        &(params.no_extra),                1,
						"Use the database but not the FS_TEST_EXTRA"                        },
					{ "collective", no_argument,        &(params.collective_flag),       1,
						"Whether to use collective I/O (for N-1, mpi-io only)"              },
					{ "shift",      no_argument,        &(params.shift_flag),            1,
						"Whether to shift ranks to avoid read caching"                      },
					{ NULL,         0,                  NULL,                    0 ,
						NULL                                                        },
					};

	memcpy( mylongopts, localopt, sizeof(localopt) );
}





char *prog_name = NULL;
char is_plfs_target = 0;

void 
print_time(const char *what, struct State *state) {
    if(state->my_rank==0){
        time_t lt = time(NULL);
        char *ts = asctime(localtime(&lt));
        ts[strlen(ts)-1] = '\0';    // remove the extraneous newline
        fprintf(state->ofptr,"%s: at operation %s.\n",ts,what);
        fflush(state->ofptr);
    }
}

int 
main( int argc, char *argv[] )
{
    init_opt();
	
	
    struct time_values write_times;         /* holder for all measured times */
    struct time_values read_times;         /* holder for all measured times */
    struct State       state;
    int rc;
    memset( &write_times, 0, sizeof(struct time_values) );
    memset(  &read_times, 0, sizeof(struct time_values) );
    prog_name = strdup(argv[0]);


    int i;
    
    if ( (rc = init( argc, argv, 
                        &params, &write_times, &read_times, &state )))
    {
        if ( state.my_rank == 0 ) {
            fprintf( stderr, "%d: Early exit due to init error: %d\n", 
                    state.my_rank,rc );
        }
        return -1;
    }
    
    MPI_Barrier( MPI_COMM_WORLD );

    if ( params.use_db ) {
        db_insert(state.my_rank,0,"partial",&params,&state,NULL,NULL, NULL );
    }

    if(state.my_rank == 0) {
        const char *version = "$Revision: 1.112 $";
        time_t lt = time(NULL);
        fprintf(state.ofptr,"MPI-IO TEST v%s: %s\n",
            version, asctime(localtime(&lt)));
    }

    // write the file
    if ( !params.read_only_flag ) {
        read_write_file( &params, &write_times, &state, WRITE_MODE );
        if ( params.use_db ) {
            db_insert(state.my_rank, 0, NULL,&params,&state,&write_times,NULL,
                    NULL);
        }
    }

#ifdef HAS_PLFS
    // flatten and/or sleep as directed  (flatten is a weird plfs thing)
    if (params.plfs_flatten) flatten_file(&params,&state,&read_times);
#endif

    if (params.sleep_seconds) sleep(params.sleep_seconds); 

    // read the file
    if ( !params.write_only_flag ) {
        read_write_file( &params, &read_times, &state, READ_MODE );
      if ( params.use_db ) {
          db_insert(state.my_rank, 0, NULL, &params, &state, NULL, &read_times, 
                  NULL);
      }
    }

    // reset the partial to NULL if no errors (mysql handles this)
    if ( params.use_db ) {
        db_insert(state.my_rank, 0, "NULL", &params, &state, NULL, NULL, NULL );
    }
    print_time("end", &state );
    fini( &params, &state );
    return 0;
}

void
fini( struct Parameters *params, struct State *state ) {

    if (params->ofname != NULL && state->my_rank == 0)   fclose(state->ofptr);
    if (params->efname != NULL)   fclose(state->efptr);

    if(params->info && params->info != MPI_INFO_NULL){
        MPI_Info_free(&(params->info));
    }

    if (MPI_Finalize() != MPI_SUCCESS) {
        fprintf(state->efptr,"Rank %d ERROR: MPI_Finalize failed.\n", 
                state->my_rank);
    }
    exit( 0 );
}


void
check_illogical_args( struct Parameters *params, struct State *state, 
        int illogical, const char *format, ... ) 
{
    if ( illogical ) {
        if ( state->my_rank == 0 ) {
            char buffer[1024];
            va_list args;
            va_start( args, format );
            vsnprintf( buffer, 1024, format, args );
            fprintf( state->efptr, "USAGE ERROR: %s\n", buffer );
            va_end( args );
        }
        Usage( params, state, NULL, NULL );
    }
}

/*******************************************
 *   ROUTINE: parse_command_line
 *   PURPOSE: Parse the command line
 *   DATE:   January 3, 2005 
 *   LAST MODIFIED: February 14, 2006 [swh]
 *******************************************/

int parse_command_line(int my_rank, int argc, char *argv[], 
                struct Parameters *params, struct State *state )
{
  int i = 1;
  int index = 0;
  int mpi_ret;
  int ch;
  char strtok_buf[65536];
  char getopt_buf[65536];

  long long int temp_num;
  struct myoption *current = mylongopts;
  int    num_opts = 0;
  struct option *longopts;

  
  MPIO_lock_shared();
  
  optarg = NULL;
  optind = 0;
  opterr = 0;
  optopt = 0;
  
  // copy our format of longopts to the gnu version (ours has extra fields
  // for help messages, etc)
  while( current->name ) {
      num_opts++;
      current++;
  }
  num_opts++;   // one extra space for the final NULL one 
  longopts = (struct option *)Malloc( sizeof(struct option) * num_opts,
                                      state->efptr, state->my_rank );
  getopt_buf[0] = (char)0;;
  for( i = 0; i < num_opts; i++ ) {
      if ( mylongopts[i].flag == NULL && mylongopts[i].name != NULL ) {
          int cur_len = strlen( getopt_buf );
          snprintf( &(getopt_buf[cur_len]), 65536 - cur_len, "%c%s",
              (char)mylongopts[i].val,
               ( mylongopts[i].has_arg == required_argument ? ":" : "" ) );
      }
      longopts[i].name    = mylongopts[i].name;
      longopts[i].has_arg = mylongopts[i].has_arg;
      longopts[i].flag    = mylongopts[i].flag;
      longopts[i].val     = mylongopts[i].val;

#ifdef _DEBUG_FSTEST
      if ( my_rank == 0 ) {
        fprintf( state->efptr, "longopts[%d].name is \"%s\"\n", i, longopts[i].name );
      }
#endif

	
  }
  //if ( my_rank == 0 ) fprintf( stderr, "Built opt buf: %s\n", getopt_buf );

/******************************************************************
* allocate the MPI_Info structure. If no hints are set, the structure 
* will be freed at the end of this routine.
******************************************************************/
  if ((mpi_ret = MPI_Info_create(&params->info)) != MPI_SUCCESS){
    if (my_rank == 0) {
      fatal_error( stderr, my_rank, mpi_ret, NULL,
				"Unable to create MPI_info structure.\n");
    }
  } else {
    params->info_allocated_flag = 1;
  }

/******************************************************************
* Now read and load all command line arguments. 
******************************************************************/
  while ((ch = getopt_long_only( argc, argv,
              getopt_buf, longopts, &index )) != -1 )
  {
      int temp_int;

#ifdef _DEBUG_FSTEST
      if ( my_rank == 0 ) {
        fprintf( state->efptr, "Processing a \"%c\" command...\n", ch );
      }
#endif

      switch(ch) {
        case 'b':
            params->barriers = strdup( optarg );
            break;
        case 'd':   
            temp_int = atoi( optarg );
            if ( temp_int < 1 ) {
              check_illogical_args( params, state, 1,
                "Invalid value for nn_num_dirs of \"%s\". num_nn_dirs must be a postive integer.", optarg );
            } else {
              params->num_nn_dirs = temp_int;
            }
            break;
        case 'e':   
            params->efname = strdup( optarg );
            break;
#ifdef HAS_PLFS
        case 'F':
            params->plfs_flatten = atoi(optarg);
            break;
#endif
        case 'f':
            params->tmpdirname = strdup( optarg );
            break;
        case 'g':
            params->tfname = strdup( optarg );
            // is_plfs_target is initialized to 0 (zero), or false.
            if ( strlen( params->tfname ) >= strlen( "plfs:" )) {
              is_plfs_target = !strncmp( params->tfname, "plfs:", strlen( "plfs:" ));
            }

            #ifdef HAS_PLFS
              if ( !is_plfs_target ) {
                struct stat st_temp;
                char *tfname_dir_part;

                tfname_dir_part = strdup( params->tfname );
                tfname_dir_part = dirname( tfname_dir_part );
                is_plfs_target = !plfs_getattr( NULL, tfname_dir_part, &st_temp, 0 );
                free( tfname_dir_part );
              }
            #endif
            break;
        case 'i':
            params->io_type_str = strdup( optarg );
            if ( !strcmp( optarg, "mpi" ) ) {
                params->io_type = IO_MPI;
            } else if ( !strcmp( optarg, "posix" ) ) {
                params->io_type = IO_POSIX;
            } else if ( !strcmp( optarg, "plfs" ) ) {
                #ifdef HAS_PLFS
                    params->io_type = IO_PLFS;
                #else
                    check_illogical_args( params, state, 1,
                        "Can't do PLFS io unless built with -DHAS_PLFS." );
                #endif
            } else {
                    check_illogical_args( params, state, 1,
                    "Unknown io type %s.  Use posix|mpi|plfs.", optarg );
            }
            break;
        case 'h':
            {
            const char *outer_delim = ",";
            const char inner_delim = '=';
            char *hint; 
            //printf( "handling hints %s\n", optarg );
            params->info_allocated_flag = TRUE;
            params->hints = strdup( optarg );
            snprintf( strtok_buf, 65536, "%s", optarg );
            hint = strtok( strtok_buf, outer_delim );
            while( hint ) {
                char *equal = strchr( hint, (int)inner_delim );
                if ( ! equal ) {
                    fprintf( stderr, 
                            "No val passed to key %s for hints %s\n",
                            hint, optarg );
                    exit( 1 );
                }
                (*equal) = (char)0;
                equal++;
                if ( (mpi_ret = MPI_Info_set( params->info, hint, equal ))
                        != MPI_SUCCESS )
                {
                    printf( "Set hint %s -> %s: %d\n", hint, equal, mpi_ret );
                }
                hint = strtok( NULL, outer_delim );   // get next one
            }
            break;
            }
        case 'H':
            {
            char *flag = NULL;
            if ( argv[optind] && argv[optind][0] != '-' ) {
                flag = argv[optind];
            }
            Usage( params, state, flag, NULL );
            }
            break;
        case 'l':
            params->time_limit = atoi( optarg ); 
            break;
        case 'n':
            params->num_objs = (size_t)strtoul(optarg, NULL, 10);
            break;
        case 'o':
            if ( !strcmp( optarg, "write" ) ) {
                params->write_only_flag = TRUE;
            } else if (!strcmp(optarg, "read")){
                params->read_only_flag = TRUE;
            } else {
                check_illogical_args( params, state, 1,
                    "Unknown operation %s.  Only write|read supported", optarg);
            }
            break;
        case 'O':
            params->ofname = strdup( optarg );
            break;
        case 'P':
            if ( atoi(optarg) == 1 ) {
                params->io_type = IO_POSIX;
                params->io_type_str = strdup( "posix" );
            } else if ( atoi(optarg) == 0 ) {
                params->io_type = IO_MPI;
                params->io_type_str = strdup( "mpi" );
            }
            break;
        case 'p':
            params->sleep_seconds = atoi(optarg);
            break;
        case 'r':
            if ( params->num_procs_world != 1 ) {
                check_illogical_args( params, state, 1,
                        "Can only use -rank if NP==1" );
            }
            params->rank_flag = atoi(optarg);
            fprintf( stderr, "Changed rank to %d\n", state->my_rank );
            break;
        case 's':
            params->strided_flag  = atoi(optarg);
            break;
        case 'S':
            params->supersize = atoi(optarg);
            break;
        case 'M':
            {
                    // parse_size screws with the string and then full_args
                    // isn't correct
                char *tmp = strdup(optarg);
                parse_size( state->my_rank, tmp, &temp_num );
                params->max_offset = temp_num;
                free( tmp );
            }
            break;
        case 'Z':
            {
                    // parse_size screws with the string and then full_args
                    // isn't correct
                char *tmp = strdup(optarg);
                parse_size( state->my_rank, tmp, &temp_num );
                params->max_size = temp_num;
                free( tmp );
            }
            break;
        case 't':
            params->test_type   = atoi(optarg);
            break;
        case 'C':
            params->check_data_ndx   = atoi(optarg);
            break;
        case 'T':
            params->touch   = atoi(optarg);
            break;
        case 'x':
            params->experiment = strdup( optarg );
            break;
        case 'X':   
            params->nn_dir_prefix = strdup( optarg );
            break;
        case 'z':
            {
                    // parse_size screws with the string and then full_args
                    // isn't correct
                char *tmp = strdup(optarg);
                parse_size( state->my_rank, tmp, &temp_num );
                params->obj_size = temp_num;
                free( tmp );
            }
            break;
        case ':':
            check_illogical_args( params, state, 1,
                "No arg passed for required arg (index %d)\n", index );
            break;
        case '?':
            // think ones w/out args come here maybe ...
            check_illogical_args( params, state, 1,
                    "Unknown arg %s (index %d) passed\n", 
                    argv[optind-1], index );
            break;
        default:
            // think ones w/out args come here maybe ....
            if ( state->my_rank == 0 ) {
                //fprintf( state->efptr, 
                //    "Unknown arg %s (index %d) passed\n", 
                //    argv[optind-1], index );
            }
            //fatal_error( state->efptr, state->my_rank, 0, NULL,
            //        "WTF: getopt_long\n" );
            //if ( state->my_rank == 0 ) {
            //    fprintf( stderr, "Bad args?\n" );
            //}
            break;
      }
  }
  free( longopts );

  set_using_db( params->use_db );
#if defined(MYSQL_HOST) || defined(MYSQL_FILE)
  if ( ! getenv("FS_TEST_EXTRA") && ! params->no_extra ) {
      check_illogical_args( params, state, 1, 
              "You are using the mysql integration "
              "but you don't have FS_TEST_EXTRA set.\n" 
              "Either set it or run with -noextra.\n"
              );
  }
  if ( state->my_rank == 0 ) {
      if ( params->use_db ) {
          fprintf( stderr, "Running with mysql integration\n" ); 
      } else {
          fprintf( stderr, "Running without mysql integration, though MYSQL"
                   " environment variables are defined - "
                   "Used \"-nodb\" switch\n" );
      }
  }
#else 
  if ( ! params->use_db ) {
      if ( state->my_rank == 0 ) {
          fprintf( stderr, "Running without mysql integration (%d)\n", 
              params->use_db );
      }
  } else {  
      check_illogical_args( params, state, 1, 
              "You have not set up the mysql integration. "
              "Either set it up or run with -nodb." );
  }
#endif

    if ( optind < argc ) {
        check_illogical_args( params, state, 1,
            "Unknown arg used.  Crosscheck against -help.\n");
    }
    if ( params->barriers ) {
        check_illogical_args( params, state,  
            params->time_limit && params->barriers 
            && strstr( params->barriers, "write" )
            || params->time_limit && params->barriers
            && strstr (params->barriers, "read" ),
            "Barriers bwrite, bread, awrite, and aread cannot be "
            "used with time limit option.");
    }
    check_illogical_args( params, state, 
            ! params->supersize && ! params->strided_flag 
            && params->test_type==2 && params->time_limit,
            "Must specify supersize for N-1 non-strided with time limit" );
    check_illogical_args( params, state, 
            ! params->supersize && ! params->strided_flag 
            && params->test_type==2 && params->max_size,
            "Must specify supersize for N-1 non-strided with max size" );
    check_illogical_args( params, state, 
            params->rank_flag && ! params->read_only_flag,
            "Can only use -rank flag in read-only mode" );
    check_illogical_args( params, state, 
            (( params->plfs_flatten ) && ( !is_plfs_target || params->test_type != 2 )),
            "Can only use -flatten flag for PLFS targets using N-1 I/O" );
    /*
    if ( state->my_rank == 0 ) {
      fprintf( stderr, "Checking illogical args for num_nn_dirs\n" );
    }
    */
    check_illogical_args( params, state, 
            (( params->num_nn_dirs>1 ) && ( is_plfs_target || params->io_type==IO_PLFS || params->test_type!=1 )),
            "Can only use -num_nn_dirs flag for non-PLFS targets using N-N I/O" );

    // if no hints were set, free the MPI_Info struct
  if(!(params->info_allocated_flag)){
    if( (mpi_ret = MPI_Info_free(&params->info)) != MPI_SUCCESS){
        fatal_error( state->efptr, state->my_rank, 0, NULL, 
                "MPI_Info_free failed\n" );
    }
    params->info = MPI_INFO_NULL;
  }

  if ( params->test_type == 1 && params->strided_flag == 1 ) {
    params->strided_flag = 0;
    if ( state->my_rank == 0 ) {
      fprintf( stderr, "Ignoring non-sensical strided arg for N-N\n" );
    }

  }

/* @@@
 * This section needed to implement a mode where in N-N each process
 * operates on more than one file.
 *
   * If it's N-N and we're doing multiple files per proc, add the ".%i" to
   * params->tfname. Also, allocate space to store num_nn_files worth of
   * fds, mpi_fhs, or *plfs_fds; depending on what type of I/O we're doing.

  if (( params->test_type == 1 ) && ( params->num_nn_files > 1 )) {
    char *tfname_dup;
    char *temp_tfname;

    temp_tfname = params->tfname;
    tfname_dup = strdup( params->tfname );
    params->tfname = ( char * )malloc(
        strlen( tfname_dup ) +
        strlen( ".%i" )      +
        1 );
    params->tfname = strcpy( params->tfname, tfname_dup );
    params->tfname = strcat( params->tfname, ".%i" );

     * Calling free on these causes a segmentation fault. Not sure why as
     * the documentation says that we should be able to free something that
     * was created with strdup.
     *
     * Have to comment out the two "free" calls unless you can figure out
     * how not to have it crash when they are called.
     *
    free( temp_tfname );
    free( tfname_dup );

    switch( params->io_type ) {
      case IO_POSIX:
        state->fds = ( int * )malloc( sizeof( int ) * num_nn_files );
        break;
      case IO_MPI:
        state->mpi_fhs = ( MPI_File * )malloc( sizeof( MPI_File ) * num_nn_files );
        break;
      case IO_PLFS:
#ifdef HAS_PLFS
        state->plfs_fds = ( Plfs_fd ** )malloc( sizeof( Plfs_fd * ) * num_nn_files );
#endif
        break;
      default:
        break;
    }
  } else {
    switch( params->io_type ) {
      case IO_POSIX:
        state->fds = ( int * )malloc( sizeof( int ));
        break;
      case IO_MPI:
        state->mpi_fhs = ( MPI_File * )malloc( sizeof( MPI_File ));
        break;
      case IO_PLFS:
#ifdef HAS_PLFS
        state->plfs_fds = ( Plfs_fd ** )malloc( sizeof( Plfs_fd * ));
#endif
        break;
      default:
        break;
    }
    just allocate one element to each of fds, mpi_fhs, and *plfs_fds.
  }
*/
  
  MPIO_unlock_shared();
  
  return(1);
}

FILE *
set_output( struct Parameters *params, char *path, struct State *state ) {
    FILE *fp = fopen( path, "w" );
    if ( ! fp ) {
        fatal_error( state->efptr, state->my_rank, 0, NULL,
                "Unable to open output file %s: %s\n",
                path, strerror(errno) );
    }
    return fp;
}

void
barrier( struct State *state, struct time_values *times ) 
{
    double barrier_start;
    if ( times ) {
        barrier_start = MPI_Wtime();
    }
    int mpi_ret;
    if ( (mpi_ret = MPI_Barrier(MPI_COMM_WORLD)) != MPI_SUCCESS ) {
        fatal_error( state->efptr, state->my_rank, mpi_ret, NULL,
                "Unable to call MPI_Barrier before write.\n" );
    }
    if ( times ) {
        setTime( &(times->barrier_wait_time),
            &(times->barrier_wait_start_time),
            &(times->barrier_wait_end_time),
            times->barrier_wait_time,
            MPI_Wtime(), barrier_start,
            "barrier_wait_time", __FILE__, __LINE__, 
            state->efptr, state->my_rank );
    }
}

int 
init( int argc, char **argv, struct Parameters *params,
        struct time_values *write_times,
        struct time_values *read_times,
        struct State *state ) 
{
    int mpi_ret;
    int myhost_num;
    int i;
    char my_host[MPI_MAX_PROCESSOR_NAME];
    char *pidch;
    char *temp_tfname;
    char *temp_tfname_dir;

    memset( params, 0, sizeof( *params ) );
    memset( write_times, 0, sizeof( *write_times ) );
    memset( read_times, 0, sizeof( *read_times ) );
    memset( state, 0, sizeof( *state ) );
    params->superblocks         = 1;
    params->use_db              = TRUE;
    params->io_type             = IO_MPI;
    params->io_type_str         = "mpi";


    state->efptr    = stderr;
    state->ofptr    = stdout;
    state->pagesize = getpagesize(); 
    for( i = 0; i < argc; i++ ) {
        //fprintf( stderr, "Arg %d: %s\n", i, argv[i] );
    } 

    if (MPI_Init(&argc, &argv) != MPI_SUCCESS) {
        fprintf(state->efptr, "ERROR: Unable to initialize MPI (MPI_Init).\n");
        return -1;
    }

    // tell MPI that we want to handle errors.  This should be good
    // because we always do our own error checking of MPI routines and
    // then pass any errors to fatal_error which in turn does MPI_Abort
    mpi_ret = MPI_Errhandler_set( MPI_COMM_WORLD, MPI_ERRORS_RETURN );
    if ( mpi_ret != MPI_SUCCESS ) {
        fprintf(state->efptr, "MPI_Errhandler set.\n");
        return -2;
    }

  
        // figure out rank and total procs
    if (MPI_Comm_rank(MPI_COMM_WORLD, &(state->my_rank) ) != MPI_SUCCESS) {
        fprintf(state->efptr, 
                "ERROR: Cant get processor rank (MPI_Comm_rank).\n");
        return -3;
    }
    if (MPI_Comm_size(MPI_COMM_WORLD, &(params->num_procs_world))!=MPI_SUCCESS){
        fprintf(state->efptr, "[RANK %d] ERROR: Problem getting number of "
                        "processors in MPI_COMM_WORLD.\n", state->my_rank);
        return -4;
    }
    print_time("begin", state );

    // Need the uid
    // now parse the args

#ifdef _DEBUG_FSTEST
    if ( state->my_rank == 0 ) {
      fprintf( state->efptr, "Calling parse_command_line...\n" );
    }
#endif

    if( !parse_command_line(state->my_rank, argc, argv, params, state ) ) {
        if ( state->my_rank == 0 ) {
            fprintf(state->efptr, 
                "ERROR [RANK %d]: Problem parsing the command line.\n",
                state->my_rank);
        }
        MPI_Finalize();
        return -5;
    }
    if ( params->tfname == NULL ) {
        Usage( params, state, NULL, "-target" );
    }

    // Verify that target file name does not have %p specified along 
    // with a shift argument in command line
    // This causes problem with read since pid no longer associated with rank.
    if ( (pidch = strstr(params->tfname, "%p")) != NULL ) {
        if ( params->shift_flag == 1 ) {  
            check_illogical_args( params, state, 1,
            "Cannot use shift argument with %%p identifier in target file name" );
        }
    }
    if ( params->test_type == 0 ) {
        Usage( params, state, NULL, "-type" );

    }

    // set up the output files
    if ( params->efname ) {
        state->efptr = set_output( params, params->efname, state );
    }
    if ( params->ofname && state->my_rank == 0 ) {
        state->ofptr = set_output( params, params->ofname, state );
    }
    if ( params->tmpdirname == NULL ) {
        params->tmpdirname = "/tmp";
    }
 
    // get other info for the database
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

    /*
     * Adjust the tfname for spreading N-N I/O over multiple subdirectories
     * after all other initialization processing is done.
     *
     * First, ensure that we are doing N-N I/O, then that we are not putting
     * all the files in 1 (one) directory, then that the target is not a PLFS
     * file system, and finally that we are not using the PLFS API to do the I/O.
     */

    /*
    fprintf( stderr, "About to check if we will expand_tfname_for_nn\n" );
    */
    if (( params->test_type == 1 )     &&
        ( params->num_nn_dirs > 1 )    &&
        ( !is_plfs_target )            &&
        ( params->io_type != IO_PLFS )) {

      /*
       * We will insert an expansion string for the subdirectories in which the
       * N-N I/O files will be found.
       *
       * If the user did not provide the nn_dir_prefix argument, after the call
       * to expand_tfname_for_nn it will have a value loaded in it, the default
       * value.
       */

      temp_tfname = params->tfname;
      /*
      fprintf( stderr, "Before expand_tfname_for_nn, tfname is \"%s\"\n", params->tfname );
      fprintf( stderr, "Before expand_tfname_for_nn, nn_dir_prefix is \"%s\"\n", params->nn_dir_prefix );
      */
      params->tfname = expand_tfname_for_nn( params->tfname, &( params->nn_dir_prefix ), state );
      /*
      fprintf( stderr, "After expand_tfname_for_nn, tfname is \"%s\"\n", params->tfname );
      fprintf( stderr, "After expand_tfname_for_nn, nn_dir_prefix is \"%s\"\n", params->nn_dir_prefix );
      */

      /*
       * Now if we are rank 0 (zero) and we will be writing, we will let rank
       * 0 create all the subdirectories over which the N-N I/O will be spread.
       *
       * All other ranks will barrier and wait for rank 0 to create all the
       * subdirectories.
       */

      if ( !params->read_only_flag ) {
        if ( state->my_rank == 0 ) {
          temp_tfname_dir = strdup( temp_tfname );
          temp_tfname_dir = dirname( temp_tfname_dir );
          /*
          fprintf( stderr, "Before make_nn_dirs, temp_tfname_dir is \"%s\"\n", temp_tfname_dir );
          */
          if ( make_nn_dirs(
                   temp_tfname_dir,
                   params->nn_dir_prefix,
                   params->num_nn_dirs,
                   state )) {
            fprintf(
                state->efptr, 
                "ERROR [RANK %d]: Problem making non-PLFS N-N I/O subdirectories.\n",
                state->my_rank);
            MPI_Finalize();
            return -6;
          }
          /*
          fprintf( stderr, "After make_nn_dirs. About to free temp_tfname_dir.\n", temp_tfname_dir );
          */
          free( temp_tfname_dir );

          barrier( state, NULL );
        } else {
          barrier( state, NULL );
        }
      }

      free( temp_tfname );
    }

    return 0;
}

void
check_hints( struct Parameters *params,
             struct State *state ) 
{
    int nkeys, i, dummy_int;
    MPI_Info info_used = MPI_INFO_NULL;
    char key[MPI_MAX_INFO_KEY];
    char val[MPI_MAX_INFO_VAL];
    if ( state->my_rank != 0 || params->io_type != IO_MPI ) return;

    MPI_File_get_info( state->mpi_fh, &info_used ); 
    MPI_Info_get_nkeys( info_used, &nkeys );
    fprintf( state->ofptr, "\nMPI_info %d Hints Used (RANK 0):\n", nkeys );
    for( i = 0; i < nkeys; i++ ) {
        MPI_Info_get_nthkey( info_used, i, key );
        char *save_key = strdup( key );
        MPI_Info_get( info_used, key, MPI_MAX_INFO_VAL, val, &dummy_int );

	    fprintf(state->ofptr, "Key: %s = %s\n", save_key, val );
        if ( strstr( save_key, "panfs_" ) ) {
            //printf( "Inserting panfs info: %s %s\n", save_key, val );
            insert_panfs_info( save_key, val ); // inserting panfs_info
        }
        free( save_key );
    }
}

int
open_file(  struct Parameters *params,
            struct time_values *times,
            struct State *state,
            int read_write,
            double begin_time,
            char *target ) 
{
    int mpi_ret = MPI_SUCCESS;
    int mpi_mode;
    int posix_mode;
    int success = 0;
    int file_exists = 1;
    MPI_Comm comm_file = MPI_COMM_WORLD;

    if ( read_write == WRITE_MODE ) {
      if ( params->test_type == 2 ) {
        // For MPI, all processes have to use the same mode.
        // We'd like to have rank 0 set the size to zero.
        // But, MPI_File_set_size is a collective that all ranks must
        // call with the same parameters.
        mpi_mode   = MPI_MODE_WRONLY | MPI_MODE_CREATE;

        // in N-1 mode, only rank 0 truncates the file
        if ( state->my_rank != 0 ) {
          posix_mode = O_CREAT | O_WRONLY ;
        } else {
          posix_mode = O_CREAT | O_TRUNC | O_WRONLY ;
        }
      } else { 
        // in N-N mode, everyone does truncate
        mpi_mode   = MPI_MODE_WRONLY | MPI_MODE_CREATE | MPI_MODE_EXCL | MPI_MODE_UNIQUE_OPEN;
        posix_mode = O_CREAT | O_TRUNC | O_WRONLY | O_CONCURRENT_WRITE; 
        comm_file  = MPI_COMM_SELF;
      }
    } else {
      mpi_mode   = MPI_MODE_RDONLY; 
      posix_mode = O_RDONLY; 
    }
    
    switch( params->io_type ) {
    case IO_POSIX:
      // For N-1 write, All other ranks wait for Rank 0 to open the file.
      // this is bec 0 does truncate and rest don't
      // it would be dangerous for all to do truncate as they might
      // truncate each other's writes
      if (( read_write == WRITE_MODE ) &&
          ( params->test_type == 2 )   &&
          ( state->my_rank != 0 )) {
        barrier(state, times);
      }
  
      state->fd = open( target, posix_mode, 0666 );
  
      // For N-1 write, Rank 0 waits for the other ranks to open the file after it has.
      if (( read_write == WRITE_MODE ) &&
          ( params->test_type == 2 )   &&
          ( state->my_rank == 0 )) {
        barrier(state,times);
      }
  
      if ( state->fd >= 0 ) {
        success = 1;
      }
		  
      break;
    case IO_MPI:
      if ( read_write == WRITE_MODE ) {
        if ( params->test_type == 2 ) {
/*
 * Here's an alternative implementation that is cleaner, but it seems to increase the average open
 * time from around 6.5 seconds to 30+ seconds.
 *
 * Interestingly, this method writes about 25% more data in a timed test, but is much slower to open.

          if ( state->my_rank == 0 ) {
            mpi_ret =
              MPI_File_open( MPI_COMM_SELF, target, mpi_mode, params->info, &(state->mpi_fh) );

            if ( mpi_ret == MPI_SUCCESS ) {
              mpi_ret = MPI_File_set_size( state->mpi_fh, 0 );
              if ( mpi_ret != MPI_SUCCESS ) {
                break; // An error occurred, so get out of this routine.
              }

              mpi_ret = MPI_File_close( &( state->mpi_fh ));
              if ( mpi_ret != MPI_SUCCESS ) {
                break; // An error occurred, so get out of this routine.
              }
            } else {
              break; // An error occurred, so get out of this routine.
            }
          }

          // Now everyone must get to this point before we move on.

          barrier( state, times );

          // Now everyone opens the newly created or truncated file.

          mpi_ret =
            MPI_File_open( comm_file, target, mpi_mode, params->info, &(state->mpi_fh) );

          if ( mpi_ret == MPI_SUCCESS ) {
            success = 1;
          }
*/

          // We don't know whether or not the target file exists. If it does, we'll need to call
          // MPI_File_set_size. That is a costly call, so we need to figure out whether or not
          // we really need to call it.

          // Have every process, but 0 barrier and wait for 0 to test if the file exists or not.

          if ( state->my_rank != 0 ) {
            barrier( state, times );
          } else {

            // Have rank 0 only open the file for read. If it succeeds, then the file already exists.

            mpi_ret =
              MPI_File_open( MPI_COMM_SELF, target, MPI_MODE_RDONLY, params->info, &( state->mpi_fh ));

            // The default is that the file exists, file_exists = 1.

            if ( mpi_ret != MPI_SUCCESS ) {
              file_exists = 0;
            } else {
              mpi_ret = MPI_File_close( &( state->mpi_fh ));
              if ( mpi_ret != MPI_SUCCESS ) {
                break; // get out of this block.
              }
            }

            // Now Rank 0 can go to the barrier to release all the others.

            barrier( state, times );
          }

          // Now rank 0 will broadcast the value of "file_exists" to everyone.

          MPI_Bcast( &file_exists, 1, MPI_INT, 0, comm_file );

          // Both MPI_File_open and MPI_File_set_size are collectives that all ranks must
          // call with the same parameters.

          mpi_ret =
            MPI_File_open( comm_file, target, mpi_mode, params->info, &(state->mpi_fh) );

          if ( mpi_ret == MPI_SUCCESS ) {
            if ( file_exists ) {
              mpi_ret = MPI_File_set_size( state->mpi_fh, 0 );
              if ( mpi_ret == MPI_SUCCESS ) { 
                success = 1;
              }
            } else {
              success = 1;
            }
          }
        } else { // ( params->test_type == 1 ))
          mpi_ret =
            MPI_File_open( comm_file, target, mpi_mode, params->info, &(state->mpi_fh) );

          if ( mpi_ret == MPI_SUCCESS ) {
            success = 1;
          } else {
            // This is the condition ( mpi_ret == MPI_ERR_FILE_EXISTS ).
            // This tells us that we tried to open an existing file.
            // Now we need to open it without MPI_MODE_EXCL and set its size to 0.

            mpi_mode = mpi_mode & ~MPI_MODE_EXCL;
            mpi_ret =
              MPI_File_open( comm_file, target, mpi_mode, params->info, &(state->mpi_fh) );
            if ( mpi_ret == MPI_SUCCESS ) {
              // ok, now we opened an already existing file. truncate it.
              mpi_ret = MPI_File_set_size( state->mpi_fh, 0 );
              if ( mpi_ret == MPI_SUCCESS ) {
                success = 1;
              } else {
                fatal_error( state->efptr, state->my_rank, mpi_ret, NULL,
                   "Unable to truncate file %s to 0 (zero) size for %s.\n",
                    target, "write" );
              }
            }
          }
        }
      } else { // ( read_write == READ_MODE )
        // if n to n, set Communicator to MPI_COMM_SELF 
        if ( params->test_type == 1 ) {
          comm_file  = MPI_COMM_SELF;
        }
        mpi_ret =
          MPI_File_open( comm_file, target, mpi_mode, params->info, &(state->mpi_fh) );

        if ( mpi_ret == MPI_SUCCESS ) {
          success = 1;
        }
      }
		
      break;
    case IO_PLFS:
      #ifdef HAS_PLFS
      // later can optimize this especially if -collective is passed
      // like ad_plfs_open: first w/ 0, then 1 per node, then everyone else
		
      // For N-1 write, All other ranks wait for Rank 0 to open the file.
      // this is bec 0 does truncate and rest don't
      // it would be dangerous for all to do truncate as they might
      // truncate each other's writes
      if (( read_write == WRITE_MODE ) &&
          ( params->test_type == 2 )   &&
          ( state->my_rank != 0 )) {
        barrier(state, times);
      }

      state->plfs_fd = NULL;
      int ret = plfs_open(&(state->plfs_fd), target, 
              posix_mode, state->my_rank, 0666,NULL);

      // For N-1 write, Rank 0 waits for the other ranks to open the file after it has.
      if (( read_write == WRITE_MODE ) &&
          ( params->test_type == 2 )   &&
          ( state->my_rank == 0 )) {
        barrier(state,times);
      }

      if ( ret == 0 ) {
        success = 1;
      } else {
        errno = -ret;
      }

      #endif
      break;
    default:
        break;
    }

    if ( ! success ) {
      fatal_error( state->efptr, state->my_rank, mpi_ret, NULL,
        "Unable to open file %s for %s.\n",
         target,
         read_write == WRITE_MODE ? "write" : "read" );
    }

    setTime( &(times->file_open_wait_time),
        &(times->file_open_wait_start_time),
        &(times->file_open_wait_end_time),
        0, MPI_Wtime(), begin_time,
        "file_open_wait_time", __FILE__, __LINE__, 
        state->efptr, state->my_rank );
    return mpi_ret;
}

off_t
get_offset( struct Parameters *params, struct State *state, int write_num ) {
    off_t offset = 0;
    switch( params->test_type ) {
    case 1: // N-N
        offset = write_num * params->obj_size;
        break;
    case 2: // N-1
        if ( params->strided_flag ) {
            off_t stride_size   = (off_t)params->num_procs_world 
                                * (off_t)params->obj_size;
            off_t stride_offset = (off_t)state->my_rank 
                                * (off_t)params->obj_size;
            offset = ((off_t)write_num * stride_size) + stride_offset;
        } else {
                // maybe if we add a time mode, it might go beyond the 
                // intended maximum size
            off_t num_objs, superblock_size, which_supersuper,
                  superblock_offset, offset_within_super, supersuper_size,
                  supersuper_offset; 
            if ( params->time_limit ) {
                if ( ! params->supersize && state->my_rank == 0 ) {
                    fatal_error( state->efptr, state->my_rank, 0, NULL,
                            "Bad args: must specify supersize "
                            "for N-1 non-strided with a time limit\n" );
                }
                num_objs = params->supersize;
            } else {
                num_objs = params->num_objs;
            }
            superblock_size       = (off_t)params->obj_size * (off_t)num_objs;
            superblock_offset     = (off_t)superblock_size 
                                  * (off_t)state->my_rank;
            which_supersuper      = (off_t)floor((off_t)write_num 
                                    / (off_t)num_objs);
            supersuper_size       = superblock_size 
                                  * (off_t)params->num_procs_world; 
            supersuper_offset     = supersuper_size * which_supersuper;
            offset_within_super   = (off_t)write_num * (off_t)params->obj_size;
            offset = supersuper_offset     // which supersuper
                   + superblock_offset     // which super
                   + offset_within_super;  // how far in
        }
        break;
    default:
        fatal_error( state->efptr, state->my_rank, 0, NULL, 
                "Unknown test_type %d\n", params->test_type );
        offset = -1;
        break;
    }
    //fprintf( stderr, "%d: offset %lu write num %d stride_offset %lu\n", 
    //            state->my_rank, offset, write_num,stride_offset );
    // if the user wants the IO's to wrap around, here's an easy way.
    if ( params->max_offset ) {
        offset %= params->max_offset;
    }
    return offset;
}

int
read_write_buf( struct Parameters *params,
           struct State *state,
           struct time_values *times,
           off_t offset,
           char *buffer,
           int read_write ) 
{
    int ret, success = 0;
    #ifdef HAS_PLFS
    plfs_error_t plfs_ret = PLFS_SUCCESS;
    #endif
    ssize_t bytes;
    MPI_Status io_stat;
    MPI_Status *status = NULL;
    const char *op_name = (read_write == WRITE_MODE) ? "write" : "read";
    //fprintf( stderr, "%d: %s at %lld\n", state->my_rank, op_name, offset );
    if ( params->io_type == IO_MPI ) {
        status = &io_stat;
    }

        // now do the actual data sending
    switch( params->io_type ) {
        case IO_POSIX:
            if ( read_write == WRITE_MODE ) {
                ret = Pwrite64( state->fd, buffer, params->obj_size, offset ); 
            } else {
                ret = Pread64( state->fd, buffer, params->obj_size, offset ); 
            }
            if ( ret == params->obj_size ) success = 1;
            break;
        case IO_MPI:
            if ( params->collective_flag == 1 ) {   // collective IO
                if ( read_write == WRITE_MODE ) {
                    ret = MPI_File_write_at_all( 
                        state->mpi_fh, 
                        offset, 
                        buffer,
                        params->obj_size,
                        MPI_CHAR,
                        &io_stat );
                } else {
                    ret = MPI_File_read_at_all( 
                        state->mpi_fh, 
                        offset, 
                        buffer,
                        params->obj_size,
                        MPI_CHAR,
                        &io_stat );
                }
            } else {                                // independent IO
                #ifndef ROMIO
                MPI_Request request;
                #else
                MPIO_Request request;
                #endif
                double wait_begin;
                
                    // seek
                ret = MPI_File_seek( state->mpi_fh, offset, MPI_SEEK_SET );
                if ( ret != MPI_SUCCESS ) {
                    fatal_error( state->efptr, state->my_rank, ret, NULL,
                            "Unable to seek for %s\n", op_name );
                }

                    // do the independent IO, then wait
                if ( read_write == WRITE_MODE ) {
                    ret = MPI_File_iwrite( 
                        state->mpi_fh,
                        buffer,
                        params->obj_size,
                        MPI_CHAR,
                        &request );
                } else {
                    ret = MPI_File_iread( 
                        state->mpi_fh,
                        buffer,
                        params->obj_size,
                        MPI_CHAR,
                        &request );
                }
                if ( ret != MPI_SUCCESS ) {
                    fatal_error( state->efptr, state->my_rank, ret, NULL,
                            "MPI_File_i%s.", op_name );
                }
                wait_begin = MPI_Wtime();
                ret = MPIO_Wait( &request, &io_stat );
                setTime( &(times->file_op_wait_time ),
                    &(times->file_op_wait_start_time),
                    &(times->file_op_wait_end_time),
                    times->file_op_wait_time,
                    MPI_Wtime(),  wait_begin,
                    "file_op_wait_time ", 
                    __FILE__, __LINE__ , 
                    state->efptr, state->my_rank );
            }
            if ( ret == MPI_SUCCESS ) success = 1;
            break;
        case IO_PLFS:
            #ifdef HAS_PLFS
            if ( read_write == WRITE_MODE ) {
                plfs_ret = plfs_write( state->plfs_fd, buffer, params->obj_size, 
                        offset, state->my_rank, &bytes );
            } else {
                plfs_ret = plfs_read( state->plfs_fd, buffer, params->obj_size,
                        offset, &bytes );
            }
            if ( plfs_ret != PLFS_SUCCESS ) {
                errno = plfs_error_to_errno(plfs_ret);
                ret = errno; // copy errno into ret
            }
            if ( bytes == params->obj_size ) success = 1;
            #endif
            break;
        default:
            fatal_error( state->efptr, state->my_rank, ret, NULL,
                "%s: Unknown io type: %d\n", __FUNCTION__, params->io_type );
            break;
    }

        // check for any errors
    if ( ! success ) {
        fatal_error( state->efptr, state->my_rank, ret, status,
                "%s:%d %s io %s, ret %d, offset %lld, obj_size %lld\n",
                __FUNCTION__,
                __LINE__,
                op_name,
                params->io_type_str,
                ret,
                offset,
                params->obj_size );
    }

    return ret;
}

void
Usage( struct Parameters *params, struct State *state, char *which_field,
       char *missing ) 
{
    if ( state->my_rank == 0 ) {
        struct myoption *opt = mylongopts;
        int field_found = 0;
        fprintf( stderr, "Usage: %s\n", prog_name );
        while( opt->name ) {
            if ( which_field ) {
                if ( ! strcmp( which_field, opt->name ) ) {
                    field_found = 1;
                    fprintf( stderr, "--%s\n" 
                                     "\t%s\n",
                                     opt->name,
                                     opt->help );
                    break;
                }
            } else {
                fprintf( stderr, "\t--%s", opt->name );
                if ( opt->has_arg == required_argument ) {
                    fprintf( stderr, " arg" );
                } else if ( opt->has_arg == optional_argument ) {
                    fprintf( stderr, " [arg]" );
                }
                fprintf( stderr, "\n" );
            }
            opt++;
        }
        if ( which_field && !field_found ) {
            fprintf( stderr, "%s is not a valid option to %s\n", 
                which_field, prog_name );
        }
        if ( missing ) {
            fprintf( stderr, "ERROR: Did not specify required field %s.\n",
                    missing );
        }
    }
    fini( params, state);
}

// returns whether or not it barriered
int
conditional_barrier( struct State *state, struct time_values *times,
        struct Parameters *params, char *this_barrier, int print_flag )
{
    char *full_barriers = params->barriers;
    if(print_flag) print_time( this_barrier, state );
    if ( full_barriers && strstr( full_barriers, this_barrier ) ) {
        /*
        if ( state->my_rank==0 ) {
            fprintf( state->efptr, "Doing barrier for %s\n", this_barrier );
        }
        */
        barrier( state, times );
        return 1;
    } else {
        return 0;
    }
}

// keep unlinking as long as we get eagain
int 
Unlink( const char *target, struct State *state ) {
    int ret = 0;
    while( 1 ) {
        if ( ( ret = unlink( target ) ) != 0 ) {
            perror( "perror unlink" );
            if ( errno == EAGAIN ) {
                fprintf(state->efptr, "Got EAGAIN for unlink...\n");
                sleep( 2 );
            } else {
                break;
            }
        } else {
            break;
        }
    }
    return ret;
}

int
get_file_size( struct State *state, struct Parameters *params, 
        char *target, off_t *filesize ) 
{
    int success = 0;
    struct stat buf;
    int ret;
    MPI_Offset moff;
    switch( params->io_type ) {
        case IO_POSIX:
            ret = stat( target, &buf ); 
            *filesize = buf.st_size;
            if ( ret == 0 ) success = 1;
            break;
        case IO_MPI:
            ret = MPI_File_get_size( state->mpi_fh, &moff );
            *filesize = (off_t)moff;
            if ( ret == MPI_SUCCESS ) success = 1;
            break;
        case IO_PLFS:
            #ifdef HAS_PLFS
            ret = plfs_getattr( state->plfs_fd, target, &buf, 1 );
            *filesize = buf.st_size;
            if ( ret == 0 ) success = 1;
            else errno = -ret;
            #endif
            break;
        default:
            break;
    }
    return success;
}

int
close_file( struct Parameters *params,
            struct time_values *times,
            struct State *state,
            double begin_time,
            int read_write,
            int errs,
            char *target )
{
    double wait_start = MPI_Wtime(); 
    int mpi_ret;
    int success = 0;

        // sync the file
    if(read_write == WRITE_MODE && params->sync_flag) {
        conditional_barrier( state, times, params, "bsync", 1 );
        wait_start = MPI_Wtime();
        switch( params->io_type ) {
            case IO_POSIX:
                mpi_ret = fsync( state->fd );
                if ( mpi_ret == 0 ) success = 1;
                break;
            case IO_MPI:
                mpi_ret = MPI_File_sync( state->mpi_fh );
                if ( mpi_ret == MPI_SUCCESS ) success = 1;
                break;
            case IO_PLFS:
                #ifdef HAS_PLFS
//                mpi_ret = plfs_sync( state->plfs_fd, state->my_rank ); 
                mpi_ret = plfs_sync( state->plfs_fd ); 
                if ( mpi_ret == 0 ) success = 1;
                #endif
                break;
            default:
                fatal_error( state->efptr, state->my_rank, mpi_ret, NULL, 
                        "%s Unknown io type %s\n", __FUNCTION__, 
                        params->io_type_str );
                break;
        }
        if ( ! success )  {
            fatal_error( state->efptr, state->my_rank, mpi_ret, NULL, 
                    "sync returned unsuccessfully.\n" );
        }
        conditional_barrier( state, times, params, "async", 1 );
        setTime( &(times->file_sync_wait_time ),
            &(times->file_sync_wait_start_time),
            &(times->file_sync_wait_end_time),
            0,   MPI_Wtime() ,  
            wait_start,
            "file_sync_wait_time ", __FILE__, __LINE__ , 
            state->efptr, state->my_rank );
    }

    if ( params->truncate_flag && (params->test_type==1 || state->my_rank==0)) {
        off_t filesize;
        success = 0;
        conditional_barrier( state, times, params, "btrunc", 1 );
        switch( params->io_type ) {
            case IO_POSIX:
                mpi_ret = ftruncate( state->fd, 0 );
                if ( mpi_ret == 0 ) success = 1;
                break;
            case IO_MPI:
                mpi_ret = MPI_File_set_size( state->mpi_fh, 0 );
                if ( mpi_ret == MPI_SUCCESS ) success = 1;
                break;
            case IO_PLFS:
                #ifdef HAS_PLFS
                // for PLFS versions prior to 2.1, there is no 4th arg here
                mpi_ret = plfs_trunc( state->plfs_fd, target, 0, 1 );
                if ( mpi_ret == 0 ) success = 1;
                else errno = -mpi_ret;
                #endif
                break;
            default:
                break;
        }
        if ( ! success ) {
            fatal_error( state->efptr, state->my_rank, mpi_ret, NULL, 
                     "file truncate failed.\n" );
        }

            // check the size and make sure it truncated
        int barriered = conditional_barrier(state, times, params, "atrunc", 1);
        if ( ! barriered ) barrier( state, times );
        success = get_file_size(state,params,target,&filesize);
        if ( ! success )  {
            fatal_error( state->efptr, state->my_rank, mpi_ret, NULL, 
                     "file stat after truncate failed.\n" );
        }
        if ( filesize != 0 ) {
            fatal_error( state->efptr, state->my_rank, mpi_ret, NULL, 
                     "truncate failed %ld != 0.\n", filesize );
        } else {
            printf( "truncate suceeded\n" );
        }
    }

    // stat the file, have to do it before the close maybe for MPI....
    wait_start = MPI_Wtime();
    if(state->my_rank == 0 && params->stat_flag) {
        off_t filesize;
        conditional_barrier(state, times, params, "bstat", 1);
        success = get_file_size(state,params,target,&filesize);
        conditional_barrier(state, times, params, "astat", 1);
        if(success) {
            printf("File size after close: %ld\n",(long)filesize);
        } else {
            fatal_error(state->efptr, state->my_rank, mpi_ret, NULL,
                    "Couldn't get file size after close: %s\n",
                    strerror(errno));
        }
    }
    setTime( &(times->file_stat_wait_time),
        &(times->file_stat_wait_start_time),
        &(times->file_stat_wait_end_time), 
        0,   MPI_Wtime() ,  
        wait_start,
        "file_stat_wait_time ", __FILE__, __LINE__ , 
        state->efptr, state->my_rank );

        // close the file
    wait_start = MPI_Wtime();
    success = 0;
    int flags;
    int open_handles;
    #ifdef HAS_PLFS
    plfs_error_t plfs_ret = PLFS_SUCCESS;
    #endif
    switch( params->io_type ) {
        case IO_POSIX:
            mpi_ret = close( state->fd );
            if ( mpi_ret == 0 ) success = 1;
            break;
        case IO_MPI:
            mpi_ret = MPI_File_close( &(state->mpi_fh) );
            if ( mpi_ret == MPI_SUCCESS ) success = 1;
            break;
        case IO_PLFS:
            #ifdef HAS_PLFS
            flags = ( read_write == READ_MODE ? 
                        O_RDONLY : O_CREAT | O_WRONLY );
            plfs_ret = plfs_close(state->plfs_fd,state->my_rank,state->uid,
                    flags,NULL, &open_handles);
            if ( plfs_ret == PLFS_SUCCESS ) success = 1;
            else errno = plfs_error_to_errno(plfs_ret);
            #endif
            break;
        default:
            break;
    }
    if ( ! success )  {
        fatal_error( state->efptr, state->my_rank, mpi_ret, NULL, 
                     "file close failed.\n" );
    }
    setTime( &(times->file_close_wait_time),
        &(times->file_close_wait_start_time),
        &(times->file_close_wait_end_time), 
        0,   MPI_Wtime() ,  
        wait_start,
        "file_close_wait_time ", __FILE__, __LINE__ , 
        state->efptr, state->my_rank );

    //  set final time 
    setTime( &(times->total_time ),
        &(times->total_start_time),
        &(times->total_end_time),
        0,  MPI_Wtime(),  begin_time,
        "total_time ", __FILE__, __LINE__ , 
        state->efptr, state->my_rank );

        // have we been asked to delete the file?
        // don't delete if we encountered any errors
    if(params->delete_flag && (read_write==READ_MODE||params->write_only_flag)){
        //fprintf( stderr, "%d Trying to unlink file %s now (%d %d %d %d)\n", 
        //        state->my_rank, target,
        //        params->delete_flag, read_write, params->write_only_flag, 
        //        params->test_type );
        // who should delete?  only 0 for n-1, everybody for n-n.
        if ( params->test_type == 2 )
        {
            // need to do a barrier for POSIX N-1 so that 0 doesn't delete
            // before everyone else finishes writing since panfs and plfs 
            // doesn't preserve posix
            // unlink semantics across multiple clients
            // TODO: probably we need to add bclose to the barriers flag
            barrier( state, times );
        }
        begin_time = MPI_Wtime(); 
        // don't actually do the unlink if there's an error
        if((params->test_type == 1 || state->my_rank == 0) && !errs) {
            success = 0;
            switch( params->io_type ) {
                case IO_POSIX:
                    mpi_ret = Unlink( target, state );
                    if ( mpi_ret == 0 ) success = 1;
                    break;
                case IO_MPI:
                    mpi_ret = MPI_File_delete(target, params->info);
                    if( mpi_ret == MPI_SUCCESS) success = 1;
                    break;
                case IO_PLFS:
                    #ifdef HAS_PLFS
                    mpi_ret = plfs_unlink( target );
                    if ( mpi_ret == 0 ) success = 1;
                    else errno = -mpi_ret;
                    #endif
                    break;
                default:
                    break;
            }
            if ( ! success ) {
                fatal_error( state->efptr, state->my_rank, mpi_ret, NULL, 
                    "Unable to remove file %s.\n", target );
            }
		}

        setTime( &(times->unlink_time ), 
            &(times->unlink_start_time),
            &(times->unlink_end_time),
            0,  MPI_Wtime(),  begin_time,
            "unlink_time ", __FILE__, __LINE__ , 
            state->efptr, state->my_rank );
    }

    return mpi_ret;
}

int 
shift_rank( int rank, int np ) {
    int shift = np / 2;
    int newrank = ( rank + shift ) % np; 
    //fprintf( stderr, "Shifting rank from %d to %d\n", rank, newrank );
    return newrank;
}

int
is_exit_condition(  int num_ios, 
                    struct Parameters *params, 
                    struct State *state,
                    double begin_time,
                    int read_write ) 
{
        // it's possible that the user said no objects AND no time
        // (i.e. just interested in measuring open and close times
    if (params->time_limit==0 && params->num_objs==0 && params->max_size==0){
        return 1;
    }

        // check whether we've exceeded our time limit 
    if ( params->time_limit && MPI_Wtime() - begin_time >= params->time_limit ){
        return 1; 
    } 

        // check whether we've exceeded our max size 
    if ( params->max_size ) {
        size_t cur_size = num_ios * params->obj_size * params->num_procs_world;
        //printf("%d: Computing %ld with %ld\n", state->my_rank,
        //        cur_size, params->max_size);
        if ( cur_size > params->max_size ) return 1;
    }

        // check whether we've exceeded our num_objs
    if ( params->num_objs && num_ios >= params->num_objs ) {
        return 1; 
    }

        // read mode and time limit: don't read more than was written
    if (read_write == READ_MODE && params->time_limit && !params->read_only_flag) {
        size_t max_objs;
        if ( params->shift_flag ) {
            // here is how we know how much *this* rank wrote.  we
            // shifted our rank so we can't just use our own variable
            // for how much we wrote, instead we have to use what the
            // other one did
            max_objs = state->objs_written_all[state->my_rank];
        } else {
            // objs_written_all only populated when shifting
            max_objs = state->objs_written;
        }
        return num_ios >= max_objs;
    }

    return 0;  // keep going!
}

void
compute_amount_written( struct Parameters *params, struct State *state ) {
    double total_objs = 0.0;
    double max_d, min_d;
    int min_ndx = 0, max_ndx = 0;
    int mpi_ret;
    if( (mpi_ret = get_min_sum_max(state->my_rank, state->objs_written, &min_d, 
                    &min_ndx, &total_objs, &max_d, &max_ndx, 
                    "finding total_objs", state->ofptr, state->efptr)) 
            != MPI_SUCCESS)
    {
        fatal_error( state->efptr, state->my_rank, mpi_ret, NULL, 
                "problem finding total_objs" ); 
    }
    // now total_mbs = total number of objs
    state->min_objs  = (unsigned)min_d;
    state->max_objs  = (unsigned)max_d;
    state->ave_objs  = (unsigned)(total_objs / params->num_procs_world);
    state->total_mbs = (total_objs * params->obj_size)/(1024.0*1024.0);
    if ( state->my_rank == 0 ) {
        fprintf( state->efptr, "%d my objs %d, min objs %.0f, max objs %.0f, "
                         "total_objs = %.0f, total_mbs = %f\n", 
            state->my_rank, (int)state->objs_written, 
            min_d, max_d,
            total_objs,
            state->total_mbs );
    }

    // everybody needs to know how much everybody else wrote if we do a 
    // shift so that everybody can read the correct amount.  actually,
    // we only *need* to know for whoever was our old rank but easier to
    // do an all-al, 
    if ( params->shift_flag ) {
        int verbose_array = 0;
        state->objs_written_all = (unsigned*)
                    Malloc(sizeof(unsigned)*params->num_procs_world,
                            state->efptr, state->my_rank);
        state->objs_written_all[0] = state->objs_written;
        mpi_ret = MPI_Allgather( (void*)&(state->objs_written), 1, MPI_UNSIGNED,
                (void*)state->objs_written_all, 
                1, MPI_UNSIGNED, MPI_COMM_WORLD );
        if ( state->my_rank == 0 && verbose_array ) {  // debugging in here
            int i;
            fprintf( stderr, "Array of objs written: " );
            for( i = 0; i < params->num_procs_world; i++ ) {
                fprintf( stderr, "%d ", state->objs_written_all[i] );
            }
            fprintf( stderr, "\n" );
        }
    }
}

char *
make_str_copy( char *orig, int length, struct State *state ) {
    char *new_str = (char *)Malloc( length, state->efptr, state->my_rank );
    strncpy( new_str, orig, length );
    return new_str;
}

void
catch_gdb() {}

int
test_set_char( char correct_char, char *location, int read_write, 
        off_t offset, struct State *state, struct read_error **err_tail ) 
{
    struct read_error *err = NULL;

    if ( read_write == WRITE_MODE ) {
        *location = correct_char;
        return 0;
    }

        // read_mode check for error, add error to struct
    if ( correct_char != *location ) {
        if ( *err_tail && 
                (*err_tail)->file_offset + (*err_tail)->length == offset ) 
        { 
            // just add this error to the previous one
            err = *err_tail;
            // don't grow, the error string is plenty long enough
            /*
            if ( err->length + 1 >= err->str_length ) { // grow strings
                char *tmp;
                err->str_length *= 2;
                tmp = make_str_copy( err->expected, err->str_length, state );
                free( err->expected );
                err->expected = tmp;
                tmp = make_str_copy( err->received, err->str_length, state );
                free( err->received );
                err->received = tmp;
            }
            */
        } else {
            // here's where we need to construct an error
            err = (struct read_error*)Malloc( sizeof( struct read_error ), 
                                            state->efptr, state->my_rank );
            err->next         = NULL;
            err->prev         = *err_tail;
            (*err_tail)->next = err;
            *err_tail         = err;    // move the tail
            err->expected     = (char*)Malloc(64,state->efptr,state->my_rank);
            err->received     = (char*)Malloc(64,state->efptr,state->my_rank);
            err->str_length   = 64;
            err->file_offset  = offset;
            err->length       = 0;
            err->all_zeros    = 1;
        }
            // now print expected and received into the strings
            // sprintf won't allow a char at the last position. 
            // only record the first so many bytes . . . no need for all of them
        if ( err->length + 1 < err->str_length ) {
            sprintf( &(err->expected[err->length]), "%c", correct_char );
            sprintf( &(err->received[err->length]), "%c", *location );
        }
        err->length++;
        if ( (int)*location != 0 ) {
            err->all_zeros = 0;
        }
        return 1;
    } else {
        return 0;
    }
}

/*
 * This function determines the level of checking that is desired.
 * It will either test that the proper characters are read or write
 * the proper characters for later testing.
 *
 * touch/check == 0: do nothing
 * touch/check == 1: set/test the first char in each block
 * touch/check == 2: set/test the first char in each page
 * touch/check == 3: set/test every char in the buffer 
 */

int
test_set_buf  (
    char *buffer, int blocksize, struct State *state,
    int which_block, int pagesize, int touch,
    int check, int read_write,
    off_t file_offset, struct read_error **err_tail ) {

  int j = 0, i = 0, errors = 0;
  char correct_char;
  off_t buf_offset; 
  int rank = state->my_rank;
  int descent_level;
  
// set it up to do the initial fill of nothing but rank
  if ( read_write == INIT_MODE ) {
    descent_level = 3;
    read_write = WRITE_MODE;
  } else {
    descent_level = ( read_write == WRITE_MODE ? touch : check );
  }

  if ( descent_level >= 1 ) { 
    if (( touch >= 1 ) || ( check >= 1 )) {
      correct_char = printable_char( file_offset + rank ); 
    } else {
      correct_char = printable_char( rank );
    }

    errors += test_set_char (
                correct_char,
                &(buffer[0]),
                read_write,
                file_offset,
                state,
                err_tail );

    if ( descent_level >= 2 ) {
      for( i = 0; i < blocksize / pagesize; i++ ) {
        buf_offset = pagesize*i;
        if (( touch >= 2 ) || ( check >= 2 )) {
          correct_char = printable_char( 
          file_offset + buf_offset + rank);
        } else {
          correct_char = printable_char( rank );
        }
// don't check very first one, it was already checked by touch==1
        if ( i > 0 ) {
          errors += test_set_char (
                      correct_char,
                      &(buffer[buf_offset]),
                      read_write,
                      file_offset + buf_offset,
                      state,
                      err_tail );
        }
        if ( descent_level == 3 ) { 
          for( j = 1; j < pagesize; j++ ) { // j=0 already set/tested
            buf_offset = pagesize*i + j;
              if (( touch == 3 ) || ( check == 3 )) {
// possible we touched less than 3 but want to check all
                correct_char = printable_char( file_offset + buf_offset + rank );
              } else {
                correct_char = printable_char( rank );
              }
              errors += test_set_char (
                          correct_char,
                          &(buffer[buf_offset]),
                          read_write,
                          file_offset + buf_offset,
                          state,
                          err_tail );
            }
          }
        }
      }
    }

  return errors;
}

void
report_errs( struct State *state, struct read_error *head ) {
    int errs_reported = 0;
    long errs_not_reported = 0;
    for( ; head != NULL; head = head->next ) {
        if ( errs_reported++ <= 5 ) {
            if ( head->all_zeros ) {
                warning_msg( state->efptr, state->my_rank, 0, NULL,
                    "%lld bad bytes at file offset %llu.  Nothing but zeroes.\n",
                    head->length,
                    head->file_offset );
            } else  {
                warning_msg( state->efptr, state->my_rank, 0, NULL,
                    "%lld bad bytes at file offset %llu.  "
                    "Expected %40s, received %40s\n",
                    head->length,
                    head->file_offset,
                    head->expected,
                    head->received );
            }
        } else {
            errs_not_reported++;
        }
    }
    if ( errs_not_reported ) {
        warning_msg( state->efptr, state->my_rank, 0, NULL,
            "%ld additional errors not reported.", errs_not_reported );

    }
}

#ifdef HAS_PLFS
/*
 * This function can only be called if the following are all true:
 *   params->plfs_flatten is non-zero (true)
 *   is_plfs_target is non-zero (true)
 *   params->test_type == 2 (N-1 I/O)
 *
 * This call in parse_command_line ensures these conditions are all true:
 *   check_illogical_args( params, state, 
 *          (( params->plfs_flatten ) && ( !is_plfs_target || params->test_type != 2 )),
 *          "Can only use -flatten flag for PLFS targets using N-1 I/O" );
 */

void
flatten_file( struct Parameters *p, struct State *s, struct time_values *t ) {

  double begin_time;
  char *target;
  int ret;


  if( s->my_rank == 0 ) {
    fprintf(
        s->efptr,
        "INFO RANK[%d]: Flattening plfs index\n", s->my_rank );
  }

  begin_time = MPI_Wtime();

  /*
   * have to get any adio prefix: off
   */

  target = expand_path( p->tfname, p->test_time, p->num_nn_dirs, s );

  if ( strchr( target, ':' )) {
    target = strchr( target, ':' ) + 1;
  }

  ret = plfs_flatten_index( NULL, target );

  if ( ret == 0) {
    t->plfs_flatten_time = MPI_Wtime()-begin_time;
  }

  if ( s->my_rank == 0 ) {
    fprintf(
        s->efptr,
        "INFO RANK[%d]: Flattened plfs index of %s in %.2f seconds: %d\n",
        s->my_rank, target,MPI_Wtime()-begin_time, ret );
  }

  barrier( s, NULL );
}
#endif

int
read_write_file( struct Parameters *params,
            struct time_values *times,
            struct State *state,
            int read_write ) 
{
    int mpi_ret;
    double begin_time, io_begin, check_buf_time, verify_begin, time_out;
    int i;
    int errs = 0;
    int orig_rank       = state->my_rank;  // save in case we shift
    char *buffer        = NULL;
    const char *op_name = (read_write == WRITE_MODE) ? "Write" : "Read";
    char *target, *temp_target1;
    check_buf_time = 0.0;
    struct read_error head;
    struct read_error *tail = &head;
    head.next = NULL;

        // shift the ranks to avoid read caching if specified
        // or to test a reread of possible bad data
    if ( params->shift_flag && read_write == READ_MODE ) {
        state->my_rank = shift_rank( state->my_rank, params->num_procs_world );
    }
    if ( params->rank_flag ) {
        state->my_rank = params->rank_flag; 
    }

        // create the file path
    target = expand_path(
               params->tfname,
               params->test_time,
               params->num_nn_dirs,
               state );
    //fprintf( stderr, "Expanded %s --> %s\n", params->tfname, target );

        // allocate the buffer
    //fprintf( stderr, "%d in mode %s for %d %s of %s\n", 
    //      state->my_rank, op_name, params->num_objs, op_name, target );
    buffer    = (char*)Malloc( params->obj_size, state->efptr, state->my_rank );
    memset( buffer, 0, params->obj_size );
    test_set_buf( buffer, params->obj_size, state, i, state->pagesize, 
            params->touch, params->check_data_ndx, INIT_MODE, 0, NULL );

        // start the clock and open the file
    begin_time = MPI_Wtime();
    conditional_barrier( state, times, params, "bopen", 1 );
    mpi_ret    = open_file(params, times, state, read_write, begin_time,target);
    conditional_barrier( state, times, params, "aopen", 1 );

        // for each specified I/O, get offset and send/recv data
        // for the time limit, exclude the open time
    io_begin   = MPI_Wtime();
    time_out   = io_begin;  // io_begin: only write time, begin_time: open too 
    for( i = 0; !is_exit_condition(i,params,state,time_out,read_write); i++ ){
        // the [b|a][write|read] barriers can only be done w/ a specified number
        // of objects.  If we're doing a timeout, then different ranks can do 
        // different numbers of reads/writes and these barriers won't work
        // we should probably check this when parsing command line 
        // with check_illogical_args
        off_t offset = get_offset( params, state, i );    
        if ( read_write == WRITE_MODE ) {
            test_set_buf( buffer, params->obj_size, state, i, 
                    state->pagesize, params->touch, 
                    params->check_data_ndx, read_write, offset, NULL );
        } else {
        }
        if ( params->time_limit == 0 || i==0 ) {
          conditional_barrier( state, times, params, 
                  read_write == WRITE_MODE ? "bwrite":"bread", (i==0?1:0));
        }
        read_write_buf( params, state, times, offset, buffer, read_write );
        if ( params->time_limit == 0 ) {
          conditional_barrier( state, times, params, 
                  read_write == WRITE_MODE ? "awrite":"aread", 0 );
        }
        if ( read_write == READ_MODE ) {
            verify_begin = MPI_Wtime();
            errs += test_set_buf( buffer, params->obj_size, state, i, 
                    state->pagesize, params->touch, 
                    params->check_data_ndx, read_write, offset, &tail );
            check_buf_time += (MPI_Wtime() - verify_begin);
            // this is a little goofy.  We use objs_written to measure bandwidth
            // but it read_only we don't set objs_written, so fake it here
            if ( params->read_only_flag ) {
                state->objs_written++;
            }
        } else {
            state->objs_written++;
        }
    }

    setTime( &(times->total_op_time),
        &(times->total_op_start_time),
        &(times->total_op_end_time),
        0,  MPI_Wtime(),
        io_begin + check_buf_time,
        "total_op_time ", 
        __FILE__, __LINE__ , state->efptr, state->my_rank ); 

        
        // check hints, close the file, do barrier, print stats
    if ( read_write == WRITE_MODE ) {
        check_hints( params, state );
    }
    //begin_time = MPI_Wtime(); // don't reset this, this is for effective
    //fprintf( stderr, "closing file in %s mode\n", op_name );
    conditional_barrier( state, times, params, "bclose", 1 );
    mpi_ret = close_file(   params, times, state, 
                            begin_time, read_write, errs, target);
    conditional_barrier( state, times, params, "aclose", 1 );

        // if we're in time_limit mode, we need to compute explicitly how
        // much we wrote, so the read phase knows how much to read
    if (read_write == WRITE_MODE || params->read_only_flag){
        compute_amount_written( params, state );
    }

        // unshift the rank
    state->my_rank = orig_rank;

    if(collect_and_print_time(state->my_rank, times, params, state, 
                (char *)op_name, state->ofptr, state->efptr) != MPI_SUCCESS)
    {
        fatal_error( state->efptr, state->my_rank, 0, NULL, 
                    "collect_and_print_time routine.\n" );
    }

    /*
     * Only attempt to remove the directories if we've ended a write mode
     * and the write_only_flag is set, or if we've ended a read mode.
     *
     * I know that after collect_and_print_time that all pes have had to sync
     * because collect_and_print_time calls collect_and_print_single_time,
     * which calls get_min_sum_max, which calls MPI_Reduce, a collective.
     */

    if (( read_write == READ_MODE )                                  ||
        (( read_write == WRITE_MODE ) && ( params->write_only_flag ))) {
    
      if (( state->my_rank == 0 )        &&
          ( params->delete_flag )        &&
          ( params->test_type == 1 )     &&
          ( params->num_nn_dirs > 1 )    &&
          ( !is_plfs_target )            &&
          ( params->io_type != IO_PLFS )) { 

        temp_target1 = strdup( target );

        if ( remove_nn_dirs(
               dirname( dirname( temp_target1 )),
               params->nn_dir_prefix,
               params->num_nn_dirs,
               state )) {
          fprintf(
            state->efptr, 
            "ERROR [RANK %d]: Problem removing  non-PLFS N-N I/O subdirectories.\n",
            state->my_rank);
        }

        free( temp_target1 );
      }
    }

        // now report any read errors
    if ( errs ) {
        assert( read_write == READ_MODE );
        assert( head.next  != NULL );
        report_errs( state, head.next );
    }

    free( buffer );
    
    return errs;
}
