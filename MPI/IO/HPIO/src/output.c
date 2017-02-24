#include "test.h"
#include "tmpi/tmpi.h"
#include "output.h"

/* Print the hints */
static int print_hints(FILE *stream, struct test_params_s *test_params_p)
{
    int i, hint_key_len, hint_val_len, hint_nkeys, flag;
    char hint_key[MPI_MAX_INFO_KEY], *hint_val;

    MPI_Info_get_nkeys(*(test_params_p->info_p), &hint_nkeys);
    for (i = 0; i < hint_nkeys; i++)
    {
	MPI_Info_get_nthkey(*(test_params_p->info_p), i, hint_key);
	hint_key_len = strlen(hint_key);
	MPI_Info_get_valuelen(*(test_params_p->info_p), hint_key,
			      &hint_val_len, &flag);
	assert(flag);

	hint_val = malloc((hint_val_len + 1)*sizeof(char));
	if (!hint_val)
	{
	    debug_stderr("hint_val malloc of size %d failed.\n",
			 hint_val_len);
	    return -1;
	}

	MPI_Info_get(*(test_params_p->info_p), hint_key,
		     hint_val_len + 1, hint_val, &flag);
	assert(flag);
	debug_fprintf(
	    stream,
	    "hint %d \"%30s\" = %s\n",
	    i, hint_key, hint_val);
	free(hint_val);
    }
    if (!hint_nkeys)
	debug_fprintf(
	    stream,
	    "hints                                   = N/A\n");

    return 0;
}

/* Print the settings */
int print_header(FILE *stream, struct test_params_s *test_params_p) 
{
    debug_fprintf(
	stream,
	"#################### Experimental Settings ####################\n"
	"procs                                   = %d\n"
	"dir                                     = %s\n"
	"output_dir                              = %s\n"
	"default: region count                   = %d\n"
	"default: region size                    = %d\n"
	"default: region spacing                 = %d\n"
	"default: test (check single mode only)  = %s\n"
	"pattern datatype:                       = %s\n"
	"rw: WRITE                               = %d\n"
	"rw: READ                                = %d\n"
	"bandwidth test: region_count            = %d\n"
	"bandwidth test: region_size             = %d\n"
	"bandwidth test: region_spacing          = %d\n"
	"bandwidth test: single mode             = %d\n"
	"check test: human                       = %d\n"
	"check test: defined                     = %d\n"
	"check test: single mode                 = %d\n"
	"(M) Contig      | (F) Contig            = %d\n"
	"(M) NonContig   | (F) Contig            = %d\n"
	"(M) Contig      | (F) NonContig         = %d\n"
	"(M) NonContig   | (F) NonContig         = %d\n"
	"individual I/O                          = %d\n"
	"collective I/O                          = %d\n"
	"reps                                    = %d\n"
	"rep maximum time (seconds)              = %f\n"
	"average method                          = %s\n"
	"verify                                  = %s\n"
	"enable fsync                            = %d\n"
	"fsync method                            = %s\n"
	"same file                               = %d\n"
	"generate files                          = %s\n"
	"keep files                              = %d\n"
	"enable cache                            = %d\n"
	"cache size (MBytes)                     = %d\n"
	"wait time (seconds between runs)        = %f\n"
	"check required space mode               = %d\n"
	"enable resume test mode                 = %d\n"
	"atomic mode                             = %d\n",
	test_params_p->numprocs,
	test_params_p->dir,
	test_params_p->output_dir,
	test_params_p->def_region_params[REGION_COUNT],
	test_params_p->def_region_params[REGION_SIZE],
	test_params_p->def_region_params[REGION_SPACING],
	test_params_p->def_check_test,
	pattern_dtype_name[test_params_p->pattern_dtype],
	test_params_p->rw[WRITE],
	test_params_p->rw[READ],
	test_params_p->test[REGION_COUNT],
	test_params_p->test[REGION_SIZE],
	test_params_p->test[REGION_SPACING],
	test_params_p->test[BAND_SINGLE],
	test_params_p->test[HUMAN],
	test_params_p->test[DEFINED],
	test_params_p->test[CHECK_SINGLE],
	test_params_p->noncontig[C_C],
	test_params_p->noncontig[NC_C],
	test_params_p->noncontig[C_NC],
	test_params_p->noncontig[NC_NC],
	test_params_p->io_method[INDIVIDUAL],
	test_params_p->io_method[COLLECTIVE],
	test_params_p->reps,
	test_params_p->rep_max_time,
	average_name[test_params_p->average_method],
	verify_name[test_params_p->verify],
	test_params_p->enable_fsync,
	fsync_method_name[test_params_p->fsync_method],
	test_params_p->same_file,
	gen_files_print[test_params_p->gen_files],
	test_params_p->keep_files,
	test_params_p->enable_cache,
	test_params_p->cache_size,
	(double) test_params_p->wait_time / 1000000.0,
	test_params_p->estimate_space,
	test_params_p->enable_resume,
	test_params_p->atomicity);

    print_hints(stream, test_params_p);

    if (test_params_p->enable_resume == TRUE)
    {
	debug_fprintf(
	    stream,
	    "Beginning read/write at                 = %s\n"
	    "Beginning test at                       = %s\n"
	    "Beginning noncontiguous type at         = %s\n"
	    "Beginning I/O method at                 = %s\n"
	    "Beginning test arr[%2d] at               = %d\n",
	    rw_name[test_params_p->resume_arr[VAR_RW]],
	    test_type_name[test_params_p->resume_arr[VAR_TEST]],
	    noncontig_type_name[test_params_p->resume_arr[VAR_NONCONTIG]],
	    io_method_name[test_params_p->resume_arr[VAR_IO_METHOD]],
	    test_params_p->resume_arr[VAR_PARAM_IDX],
	    (params_arr_p[test_params_p->resume_arr[VAR_TEST]])
	    [test_params_p->resume_arr[VAR_PARAM_IDX]]);

    }
    if (test_params_p->test[BAND_SINGLE] == TRUE ||
	test_params_p->test[CHECK_SINGLE] == TRUE)
    {
	int64_t data_accessed, data_extent;
	int region_count, region_size, region_spacing;
	int test_type, param_val = -1;
	MPI_Datatype base_dtype;
	MPI_Aint base_dtype_lb = -1;
	MPI_Offset fileview_disp;
	int base_dtype_sz = -1;
	MPI_Aint base_dtype_ext = -1;

	if (test_params_p->test[BAND_SINGLE] == TRUE) 
	{
	    test_type = BAND_SINGLE;
	}
	else
	{
	    test_type = CHECK_SINGLE;
	    param_val = test_params_p->def_check_test_param_val;
	}

	/* Not the best use of this function since we are only
	 * interested in the base_dtype and fileview_disp */
	create_test_parameters(test_type,
			       param_val,
			       test_params_p,
			       &region_count,
			       &region_size,
			       &region_spacing,
			       &base_dtype,
			       &fileview_disp);

	MPI_Type_size(base_dtype, &base_dtype_sz);
	MPI_Type_lb(base_dtype, &base_dtype_lb);
	MPI_Type_extent(base_dtype, &base_dtype_ext);

	data_accessed = (int64_t) test_params_p->numprocs * 
	    test_params_p->def_region_params[REGION_SIZE] *
	    test_params_p->def_region_params[REGION_COUNT] * 
	    base_dtype_sz;
	data_extent = 
	    ((int64_t) test_params_p->numprocs *
	     (((int64_t) test_params_p->def_region_params[REGION_SIZE] *
	       base_dtype_ext) + 
	      test_params_p->def_region_params[REGION_SPACING]) * 
	     test_params_p->def_region_params[REGION_COUNT]) -
	    test_params_p->def_region_params[REGION_SPACING];

	if (base_dtype != MPI_CHAR)
	    MPI_Type_free(&base_dtype);

	debug_fprintf(
	    stream,
	    "total data accessed                     = %Ld bytes - %f MBytes\n"
	    "total data extent (contig file)         = %Ld bytes - %f MBytes\n"
	    "total data extent (noncontig file)      = %Ld bytes - %f MBytes\n"
	    "###############################################################"
	    "\n\n",
	    data_accessed,
	    data_accessed / 1024.0 / 1024.0,
	    fileview_disp + data_accessed,
	    (fileview_disp + data_accessed) / 1024.0 / 1024.0,
	    fileview_disp + base_dtype_lb + data_extent,
	    (fileview_disp + base_dtype_lb + data_extent) / 1024.0 / 1024.0);
    }
    else
    {
	debug_fprintf(
	    stream,
	    "###############################################################"
	    "\n\n"
	    );
    }

    return 0;
}

/* Print out a local header for the min_timing */
int preprint_min_output(FILE *stream,
			int rw_type,
			int test_type,
			int noncontig_type,
			int io_method_type)
{
    debug_fprintf(stream,
		  "%s | %s | %s | %s\n", 
		  rw_name[rw_type], 
		  test_type_name[test_type], 
		  noncontig_type_name[noncontig_type], 
		  io_method_name[io_method_type]);
    
    debug_fprintf(stream,
		  "----------------time (seconds)--------------|"
		  "-bandwidth (MB/s)|---parameter---\n"
		  "  open  |   io   |  sync  | close  | total  |"
		  "   IO   |  IOsyn | %s\n", test_param_name[test_type]);

    return 0;
}

/* Print out the stats for the screen */
int print_min_timing(FILE *stream,
		     int reps_completed, int test_type,
		     int region_count, int region_size, int region_spacing,
		     int param_val, double *final_time_arr, 
		     struct test_params_s *test_params_p)
{
    int64_t total_bytes = (int64_t) region_count * region_size;

    debug_fprintf(
	stream,
	"%7.3f |%7.3f |%7.3f |%7.3f |%7.3f |%7.3f |%7.3f |",
	final_time_arr[TIME_OPEN],
	final_time_arr[TIME_IO],
	final_time_arr[TIME_SYNC],
	final_time_arr[TIME_CLOSE],
	final_time_arr[TIME_TOTAL],
	total_bytes * test_params_p->numprocs / 1024.0 / 1024.0 /
	final_time_arr[TIME_IO],
	total_bytes * test_params_p->numprocs / 1024.0 / 1024.0 /
	(final_time_arr[TIME_IO] +
	 final_time_arr[TIME_SYNC]));
    if (test_type == BAND_SINGLE)
	debug_fprintf(stream, " %d,%d,%d", 
		      region_count, region_size, region_spacing);
    else if (test_type == CHECK_SINGLE)
	debug_fprintf(stream, " %s", test_params_p->def_check_test);
    else if (test_type == HUMAN)
	debug_fprintf(stream, " %s", human_name[param_val]);
    else if (test_type == DEFINED)
	debug_fprintf(stream, " %s", defined_name[param_val]);
    else
	debug_fprintf(stream, " %d", param_val);
    
    if (reps_completed != test_params_p->reps)
	debug_fprintf(stream, "(%d rep)\n", reps_completed);
    else
	debug_fprintf(stream, "\n");

    return 0;
}

int print_full_timing_header(FILE *stream)
{
    debug_fprintf(stream,
		  "procs                    |"
		  "dir                      |"
		  "output dir               |"
		  "disp                     |"
		  "region count (b)         |"
		  "region_size (b)          |"
		  "region_spacing (b)       |"
		  "pattern datatype (b)     |"
		  "rw                       |"
		  "test                     |"
		  "noncontig type           |"
		  "I/O method               |"
		  "reps                     |"
		  "average method           |"
		  "verify                   |"
		  "aggregator method        |"
		  "aggregator count         |"
		  "PFR size (b)             |"
		  "enable fsync             |"
		  "fsync method             |"
		  "same file                |"
		  "generate files           |"
		  "keep files               |"
		  "enable cache             |"
		  "cache size (MB)          |"
		  "wait time (s)            |"
		  "parameter                |"
		  "total data accessed (b)  |"
		  "total data extent (b)    |"
		  "open (s)                 |"
		  "io (s)                   |"
		  "sync (s)                 |"
		  "close (s)                |"
		  "total (s)                |"
		  "IO bandwidth (MB/s)      |"
		  "IO sync bandwidth (MB/s) |"
		  "\n");
    return 0;
}

/* Print out the stats for an output file */
int print_full_timing(FILE *stream, int test_type,
		      int noncontig_type, int pattern_dtype, int rw_type, 
		      int io_method, int reps_completed, 
		      MPI_Offset general_disp, MPI_Datatype *base_dtype_p,
		      int region_count, int region_size, int region_spacing,
		      int param_val, double *final_time_arr, 
		      struct test_params_s *test_params_p)
{
    int base_dtype_sz = -1;
    MPI_Aint base_dtype_lb = -1;
    int64_t agg_total_extent;
    int64_t agg_total_bytes = -1;

    MPI_Type_lb(*base_dtype_p, &base_dtype_lb);
    MPI_Type_size(*base_dtype_p, &base_dtype_sz);

    agg_total_bytes = (int64_t) test_params_p->numprocs * region_count * 
	region_size * base_dtype_sz;
    
    if (noncontig_type == C_C || noncontig_type == NC_C)
    {
	agg_total_extent = general_disp + base_dtype_lb + agg_total_bytes;
    }
    else
    {
	agg_total_extent = general_disp + base_dtype_lb + 
	    (((int64_t) test_params_p->numprocs *
	      (((int64_t) region_size * base_dtype_sz) + region_spacing) * 
	      region_count) - region_spacing);	
    }

    fprintf(stream,
	    "%24d |"
	    "%24s |"
	    "%24s |"
	    "%24Ld |"
	    "%24d |"
	    "%24d |"
	    "%24d |"
	    "%24s |"
	    "%24s |"
	    "%24s |"
	    "%24s |"
	    "%24s |"
	    "%24d |"
	    "%24s |"
	    "%24s |"
	    "%24d |"
	    "%24s |"
	    "%24d |"
	    "%24s |"
	    "%24d |"
	    "%24d |"
	    "%24d |"
	    "%24d |",
	    test_params_p->numprocs,
	    test_params_p->dir,
	    test_params_p->output_dir,
	    general_disp,
	    region_count,
	    region_size,
	    region_spacing,
	    pattern_dtype_name[pattern_dtype],
	    rw_name[rw_type],
	    test_type_name[test_type],
	    noncontig_type_name[noncontig_type],
	    io_method_name[io_method],
	    test_params_p->reps,
	    average_name[test_params_p->average_method],
	    verify_name[test_params_p->verify],
	    test_params_p->enable_fsync,
	    fsync_method_name[test_params_p->fsync_method],
	    test_params_p->same_file,
	    gen_files_print[test_params_p->gen_files],
	    test_params_p->keep_files,
	    test_params_p->enable_cache,
	    test_params_p->cache_size,
	    test_params_p->wait_time);

    if (test_type == BAND_SINGLE)
	debug_fprintf(stream, " %7d,%7d,%7d ",
		      region_count, region_size, region_spacing);
    else if (test_type == CHECK_SINGLE)
	debug_fprintf(stream, "%24s ", human_name[param_val]);
    else if (test_type == HUMAN)
	debug_fprintf(stream, "%24s ", human_name[param_val]);
    else if (test_type == DEFINED)
	debug_fprintf(stream, "%24s ", defined_name[param_val]);
    else
	debug_fprintf(stream, "%24d", param_val);
    
    if (reps_completed != test_params_p->reps)
	debug_fprintf(stream, "(%d rep) |", reps_completed);
    else
	debug_fprintf(stream, "|");

    fprintf(test_params_p->output_results,
	    "%24Ld |"
	    "%24Ld |"
	    "%24f |"
	    "%24f |"
	    "%24f |"
	    "%24f |"
	    "%24f |"
	    "%24f |"
	    "%24f |"
	    "\n",
	    agg_total_bytes,
	    agg_total_extent,
	    final_time_arr[TIME_OPEN],
	    final_time_arr[TIME_IO],
	    final_time_arr[TIME_SYNC],
	    final_time_arr[TIME_CLOSE],
	    final_time_arr[TIME_TOTAL],
	    agg_total_bytes / 1024.0 / 1024.0 /
	    final_time_arr[TIME_IO],
	    agg_total_bytes / 1024.0 / 1024.0 / 
	    (final_time_arr[TIME_IO] +
	     final_time_arr[TIME_SYNC]));
    
    return 0;
}

/*
 * Local variables:
 *  mode: c
 *  c-indent-level: 4
 *  c-basic-offset: 4
 * End:
 *
 * vim: ft=c ts=8 sts=4 sw=4 expandtab
 */
