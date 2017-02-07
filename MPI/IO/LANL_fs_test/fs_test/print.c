 /******************************************************************
* Copyright (c) 2003
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

/******************************************************************
* PROGRAM:   print
*
* PURPOSE:   Routines that deal with program output, 
*
* AUTHOR:    James Nunez 
* DATE:      February 12, 2001
* LAST MODIFIED: September 7, 2005 [jnunez]
* VERSION:   1.00.011
*
*      LOS ALAMOS NATIONAL LABORATORY
*      An Affirmative Action/Equal Opportunity Employer
*
******************************************************************/

#include "print.h"
#include "utilities.h"

#include <string.h>
#include <stdlib.h>

/*******************************************
 *   ROUTINE: print_input_environment
 *   PURPOSE: 
 *   LAST MODIFIED: August 20, 2005 [jnunez]
 *******************************************/
int 
print_input_environment(        int my_rank, 
                                int myhost_num, 
                                char *my_host, 
                                struct Parameters *params,
                                FILE *ofptr, 
                                FILE *efptr)
{
  int i = 0;
  int mpi_ret;

/******************************************************************
* Print input parameters
******************************************************************/
  if(my_rank == 0){
    fprintf(ofptr, "Input Parameters:\n");
    if(params->test_type == 1){
      fprintf(ofptr, "I/O Testing type N -> N: %d\n", params->test_type);
    }
    else if(params->test_type == 2){
      fprintf(ofptr, "I/O Testing type N -> 1: %d\n", params->test_type);
    }
    fprintf(ofptr, "Total number of processors (N): %d\n", 
            params->num_procs_world);
    
    fprintf(ofptr, "Number of objects (-nobj): %d\n", (int)params->num_objs);
    fprintf(ofptr, "Number of bytes in each object (-size): %lld\n", 
	    (long long)params->obj_size);
    fprintf(ofptr, "Check data read (-chkdata): %d\n", params->check_data_ndx);
    fprintf(ofptr, "Use collective read/write calls (-collective): %d\n", 
	    params->collective_flag);
    fprintf(ofptr, "Flush data before file close (-sync): %d\n", 
            params->sync_flag);
    fprintf(ofptr, "Strided data layout (-strided): %d\n", 
            params->strided_flag);
    fprintf(ofptr, "Delete data file when done reading (-deletefile): %d\n", 
	        params->delete_flag);
    fprintf(ofptr, "Data written to target file (-target): %s\n", 
            params->tfname);
    fflush(ofptr);
    fprintf(ofptr, "Touch object option (-touch): %d\n", params->touch);
    fprintf(ofptr, "MPI_WTIME_IS_GLOBAL: %d\n", MPI_WTIME_IS_GLOBAL);
    fprintf(ofptr, "Timing results written to (-output): %s\n", 
              ( params->ofname ? params->ofname : "stdout" ) );
    fprintf(ofptr, "Errors and warnings written to (-errout): %s\n",
            (params->efname ? params->efname : "stderr" ) );

    fflush(ofptr);
  }
 
  MPI_Barrier(MPI_COMM_WORLD);

  return 1;
}

int
collect_and_print_single_time( 
        int     my_rank,
        int     num_procs,
        double  total_mbs,
        double  time_value,
        char    *op,
        char    *bandwidth_type,    // NULL to not print bandwidth
        char    *time_type,         // NULL to not print time
        int     verbose_flag,
        int     trenddata,
        int     elapsed,
        FILE    *ofptr, 
        FILE    *efptr)
{
    double min_time, sum_time, max_time;
    double max_elapsed_time, min_elapsed_time, elapsed_time; 
    int    min_ndx,  max_ndx;
    int mpi_ret;
    

    if( (mpi_ret = get_min_sum_max(my_rank, time_value, &min_time, &min_ndx,
         &sum_time, &max_time, &max_ndx, op, ofptr, efptr)) != MPI_SUCCESS)
    {
        fatal_error( efptr, my_rank, mpi_ret, NULL,
                     "Problem computing min, max, and sum "
                     "of %s time.\n", op );
    }
        
    if ( my_rank == 0 ) {
        if ( bandwidth_type ) {
            if ( !elapsed ) {
                fprintf(ofptr, "=== %17s Bandwidth (min [rank] "
                           "avg max [rank]): "
                           "%.2e [%4d] %.2e %.2e [%4d] Mbytes/s.\n", 
                           bandwidth_type,
                           total_mbs/max_time, 
                           max_ndx,
                           (num_procs*total_mbs)/sum_time,
                           total_mbs/min_time, 
                           min_ndx);
                if(trenddata){

                    if(!strcmp(bandwidth_type,op)){
                        fprintf(ofptr," <td>  %s_Bandwidth_min %.2e MB/s\n",
                                bandwidth_type,     total_mbs/max_time);
                        fprintf(ofptr," <td>  %s_Bandwidth_ave %.2e MB/s\n",
                                bandwidth_type,(num_procs*total_mbs)/sum_time);
                        fprintf(ofptr," <td>  %s_Bandwidth_max %.2e MB/s\n",
                                bandwidth_type,     total_mbs/min_time);
                    }
                    else{
                        fprintf(ofptr," <td>  %s_%s_Bandwidth_min %.2e MB/s\n",
                                bandwidth_type,op,  total_mbs/max_time);
                        fprintf(ofptr," <td>  %s_%s_Bandwidth_ave %.2e MB/s\n",
                              bandwidth_type,op,(num_procs*total_mbs)/sum_time);
                        fprintf(ofptr," <td>  %s_%s_Bandwidth_max %.2e MB/s\n",
                                bandwidth_type,op,  total_mbs/min_time);
                    }
                }
            }
            else {
                fprintf(ofptr, "=== %37s Elapsed Bandwidth: "
                        "%24.2e %25s\n",
                        bandwidth_type, total_mbs/time_value, "Mbytes/s." 
                        ); 
                if(trenddata) {
                    if(!strcmp(bandwidth_type,op)){
                        fprintf(ofptr," <td> %s_Elapsed_Bandwidth %.2e MB/s\n",
                                bandwidth_type,     total_mbs/time_value);
                    }
                    else {
                      fprintf(ofptr," <td> %s_%s_Elapsed_Bandwidth %.2e MB/s\n",
                                bandwidth_type,op,  total_mbs/time_value);
 
                    } 
                }
            }
        }
        if ( time_type ) {
            if ( !elapsed ) {
                fprintf(ofptr, "=== %22s Time (min [rank] avg max [rank]): "
                               "%.2e [%4d] %.2e %.2e [%4d] sec.\n", 
                               time_type,
                               min_time, 
                               min_ndx, 
                               sum_time/num_procs, 
                               max_time, 
                               max_ndx);

                if(trenddata){

                   if(!strcmp(time_type,op)){
                      fprintf(ofptr," <td>  %s_Time_min %.2e s\n",
                              time_type,min_time          );
                      fprintf(ofptr," <td>  %s_Time_ave %.2e s\n",
                              time_type,sum_time/num_procs);
                      fprintf(ofptr," <td>  %s_Time_max %.2e s\n",
                              time_type,max_time          );
                   }
                   else{
                      fprintf(ofptr," <td>  %s_%s_Time_min %.2e s\n",
                              time_type,op,min_time          );
                      fprintf(ofptr," <td>  %s_%s_Time_ave %.2e s\n",
                              time_type,op,sum_time/num_procs);
                      fprintf(ofptr," <td>  %s_%s_Time_max %.2e s\n",
                              time_type,op,max_time          );
                   }
                }
            }
            else {
                fprintf(ofptr, "=== %42s Elapsed Time: "
                        "%24.2e %20s\n", time_type, time_value, "sec."); 
                if(trenddata) {
                    if(!strcmp(time_type,op)){
                        fprintf(ofptr," <td> %s_Elapsed_Time %.2e s\n",
                                time_type,            time_value);
                    }
                    else {
                       fprintf(ofptr," <td> %s_%s_Elapsed_Time %.2e s\n",
                               time_type,op,           time_value);
 
                    } 
                }

            }
        }
    }
    if(verbose_flag){
        fflush(ofptr);
        MPI_Barrier(MPI_COMM_WORLD);
    
        if ( bandwidth_type ) {
            //fprintf(ofptr, "VERBOSE [%d] %s Bandwidth = %.2e\n", 
            //                my_rank, bandwidth_type, total_mbs/time_value);

            if(trenddata){
                if(!strcmp(bandwidth_type,op)){
                    fprintf(ofptr, "<td> Proc_%d_%s_Bandwidth = %.2e MB/s\n", 
                                my_rank, bandwidth_type, total_mbs/time_value);
                }
                else{
                    fprintf(ofptr, "<td> Proc_%d_%s_%s_Bandwidth = %.2e MB/s\n", 
                            my_rank, op, bandwidth_type, total_mbs/time_value);
                }

            }
        }
        if ( time_type ) {
            //fprintf(ofptr, "VERBOSE [%d] %s Time = %.2e\n", 
            //                my_rank, time_type, time_value );
            if(trenddata){
               if(!strcmp(time_type,op)){
                   fprintf(ofptr, "<td> Proc_%d_%s_Time = %.2e s\n", 
                               my_rank, time_type, time_value );
               }
               else{
                   fprintf(ofptr, "<td> Proc_%d__%s_%s_Time = %.2e s\n", 
                               my_rank, op, time_type, time_value );
               }
            }
        }
    
        fflush(ofptr);
        MPI_Barrier(MPI_COMM_WORLD);
    }  
  
	return 0;
}

/*******************************************
 *   ROUTINE: collect_and_print_time
 *   PURPOSE: Collect and print minimum, average, and maximum of input 
 *            processor wait and file open and close times across all procs
 *   LAST MODIFIED: July 24, 2005 [jnunez]
 *******************************************/
int 
collect_and_print_time( int my_rank, 
                        struct time_values *times,
                        struct Parameters *params,
                        struct State *state,
                        char *op, 
                        FILE *ofptr, 
                        FILE *efptr)
{
    double total_mbs = 0.0; 
    char fancy_description[1024];

    int num_procs = params->num_procs_world;
    int elapsed_flag = 0;
    double reduce_min_value, reduce_max_value;
  
        // need to compute total_size_mb differently if we used a time
        // limit
    if ( params->time_limit ) {
        total_mbs = state->total_mbs;
    } else {
        total_mbs = ((double)(num_procs*params->num_objs*params->superblocks) 
              * (params->obj_size))/(1024.0*1024.0);
    }
    //fprintf( stderr, "%d using %.2f as total_mbs\n", my_rank, total_mbs );

    // total time
    collect_and_print_single_time( my_rank, 
                                   num_procs,
                                   total_mbs,
                                   times->total_time, 
                                   op,
                                   "Effective",     // yes, print "Effective" BW
                                   "Total",         // yes, print "Total" time
                                   params->verbose_flag,
                                   params->trenddata,
                                   elapsed_flag=0,
                                   ofptr,
                                   efptr );

    // total op time
    collect_and_print_single_time( my_rank,
                                   num_procs,
                                   total_mbs,
                                   times->total_op_time,
                                   op,
                                   op,  // print op Bandwidth
                                   op,  // print op Time
                                   params->verbose_flag,
                                   params->trenddata,
                                   elapsed_flag=0,
                                   ofptr,
                                   efptr );

    // open time
    snprintf( fancy_description, 1024, "File_%s_Open", op );
    collect_and_print_single_time( my_rank,
                                   num_procs,
                                   total_mbs,
                                   times->file_open_wait_time,
                                   fancy_description,
                                   NULL,               // don't print the BW
                                   fancy_description,  // print the time
                                   params->verbose_flag,
                                   params->trenddata,
                                   elapsed_flag=0,
                                   ofptr,
                                   efptr );

    // wait time : don't know what this one is actually, maybe not valid
    snprintf( fancy_description, 1024, "File_%s_Wait", op );
    collect_and_print_single_time( my_rank,
                                   num_procs,
                                   total_mbs,
                                   times->file_op_wait_time,
                                   fancy_description,
                                   NULL,                // don't print the BW
                                   fancy_description,   // print the time
                                   params->verbose_flag,
                                   params->trenddata,
                                   elapsed_flag=0,
                                   ofptr,
                                   efptr );

    // barrier time
    snprintf( fancy_description, 1024, "%s_Barrier_wait", op );
    collect_and_print_single_time( my_rank,
                                   num_procs,
                                   total_mbs,
                                   times->barrier_wait_time,
                                   fancy_description,
                                   NULL,                        // no BW
                                   fancy_description,  // print the time
                                   params->verbose_flag,
                                   params->trenddata,
                                   elapsed_flag=0,
                                   ofptr,
                                   efptr );


    // the sync time (only valid for writes)
    if( params->sync_flag ){
        snprintf( fancy_description, 1024, "%s_File_Sync", op );
        collect_and_print_single_time( my_rank,
                                       num_procs,
                                       total_mbs,
                                       times->file_sync_wait_time,
                                       fancy_description,
                                       NULL,                        // no BW
                                       fancy_description,   // print the time
                                       params->verbose_flag,
                                       params->trenddata,
                                       elapsed_flag=0,
                                       ofptr,
                                       efptr );
    }
    // close time
    snprintf( fancy_description, 1024, "%s_File_Close_Wait", op );
    collect_and_print_single_time( my_rank,
                                   num_procs,
                                   total_mbs,
                                   times->file_close_wait_time,
                                   fancy_description,
                                   NULL,                        // no BW
                                   fancy_description,   // print the time
                                   params->verbose_flag,
                                   params->trenddata,
                                   elapsed_flag=0,
                                   ofptr,
                                   efptr );

    // Print elapsed times now
    // Elapsed Effective Bandwidth and Total Time
    get_all_proc_min_max_time( my_rank,
                               times->total_start_time,
                               times->total_end_time,
                               op,
                               ofptr,
                               efptr,
                               &(times->total_elapsed_time));
    collect_and_print_single_time( my_rank,
                                   num_procs,
                                   total_mbs,
                                   times->total_elapsed_time,
                                   op,
                                   "Effective",     // yes, print "Effective" BW
                                   "Total",         // yes, print "Total" time
                                   params->verbose_flag,
                                   params->trenddata,
                                   elapsed_flag=1,
                                   ofptr,
                                   efptr );

    // Elapsed Bandwidth and Time
    get_all_proc_min_max_time( my_rank,
                               times->total_op_start_time,
                               times->total_op_end_time,
                               op,
                               ofptr,
                               efptr,
                               &(times->total_op_elapsed_time));
    collect_and_print_single_time( my_rank,
                                   num_procs,
                                   total_mbs,
                                   times->total_op_elapsed_time,
                                   op,
                                   op,  // print op Bandwidth
                                   op,  // print op Time
                                   params->verbose_flag,
                                   params->trenddata,
                                   elapsed_flag=1,
                                   ofptr,
                                   efptr );

    // Elapsed File Open Time
    snprintf( fancy_description, 1024, "File_%s_Open", op );
    get_all_proc_min_max_time( my_rank,
                               times->file_open_wait_start_time,
                               times->file_open_wait_end_time,
                               op,
                               ofptr,
                               efptr,
                               &(times->file_open_wait_elapsed_time));
    collect_and_print_single_time( my_rank,
                                   num_procs,
                                   total_mbs,
                                   times->file_open_wait_elapsed_time,
                                   fancy_description,
                                   NULL,               // don't print the BW
                                   fancy_description,  // print the time
                                   params->verbose_flag,
                                   params->trenddata,
                                   elapsed_flag=1,
                                   ofptr,
                                   efptr );

    // Elapsed File Sync Time
    if( params->sync_flag ){
        snprintf( fancy_description, 1024, "%s_File_Sync", op );
        get_all_proc_min_max_time( my_rank,
                                   times->file_sync_wait_start_time,
                                   times->file_sync_wait_end_time,
                                   op,
                                   ofptr,
                                   efptr,
                                   &(times->file_sync_wait_elapsed_time));
        collect_and_print_single_time( my_rank,
                                       num_procs,
                                       total_mbs,
                                       times->file_sync_wait_elapsed_time,
                                       fancy_description,
                                       NULL,                        // no BW
                                       fancy_description,   // print the time
                                       params->verbose_flag,
                                       params->trenddata,
                                       elapsed_flag=1,
                                       ofptr,
                                       efptr );

    }

    // Elapsed File Stat Time
    // stat time
    snprintf( fancy_description, 1024, "%s_File_Stat_Wait", op );
    get_all_proc_min_max_time( my_rank,
                               times->file_stat_wait_start_time,
                               times->file_stat_wait_end_time,
                               op,
                               ofptr,
                               efptr,
                               &(times->file_stat_wait_elapsed_time));
    collect_and_print_single_time( my_rank,
                                   num_procs,
                                   total_mbs,
                                   times->file_stat_wait_elapsed_time,
                                   fancy_description,
                                   NULL,                        // no BW
                                   fancy_description,   // print the time
                                   params->verbose_flag,
                                   params->trenddata,
                                   elapsed_flag=1,
                                   ofptr,
                                   efptr );


    // Elapsed File Close Time
    // close time
    snprintf( fancy_description, 1024, "%s_File_Close_Wait", op );
    get_all_proc_min_max_time( my_rank,
                               times->file_close_wait_start_time,
                               times->file_close_wait_end_time,
                               op,
                               ofptr,
                               efptr,
                               &(times->file_close_wait_elapsed_time));
    collect_and_print_single_time( my_rank,
                                   num_procs,
                                   total_mbs,
                                   times->file_close_wait_elapsed_time,
                                   fancy_description,
                                   NULL,                        // no BW
                                   fancy_description,   // print the time
                                   params->verbose_flag,
                                   params->trenddata,
                                   elapsed_flag=1,
                                   ofptr,
                                   efptr );


    if ( my_rank == 0 ) {
        fprintf( ofptr, "=== Completed IO %s.\n", op);
        fflush( ofptr );
    }
  
    return MPI_SUCCESS;
}
