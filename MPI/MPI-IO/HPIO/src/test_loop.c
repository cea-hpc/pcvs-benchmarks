/* (C) Northwestern University
 * See COPYING in the top-level directory . */

#include "test.h"
#include "buf.h"
#include "output.h"

/* run_test: This function is responsible for running a particular
 * test for a certain number of repetitions, averaging them out and
 * printing out the results */
int run_test(int rw_type,
	     int test_type,
	     int noncontig_type,
	     int io_method_type,
	     int param_val,
	     int use_mode,
	     struct test_params_s *test_params_p)
{
    int ret, i, j = 0;
    struct timeval tp;
    struct timezone tzp;
    double **test_time_matrix = NULL;
    double final_time_arr[MAX_TIME];
    double begin_time; 
    char *buf = NULL;
    char filename[MAX_FILENAME_SIZE];
    int region_count, region_size, region_spacing;
    int memtype_size, filetype_size, base_dtype_sz;
    int memtype_ct = -1;
    int64_t buf_sz = -1;
    MPI_Aint memtype_ext = -1, memtype_lb = -1,
	filetype_ext, base_dtype_ext;
    MPI_Offset general_disp = 0, fileview_disp = 0;
    int count = -1;
    
    /* Noncontiguous I/O API parameters */
    MPI_Datatype memtype, filetype;
    MPI_Datatype base_dtype;
    
    /* Averaging parameters */
    int max_index, reps_completed = -1;
    double *numproc_final_time_arr = NULL;
    
    switch(use_mode)
    {
	case GENERATE_MODE:
	case TEST_MODE:
	    break;
	default:
	    debug_stderr("run_test: use_mode %d invalid\n", use_mode);
	    return -1;
    }

    /* Create and initialize test timing matrix */
    if ((test_time_matrix = (double **) 
	 calloc(test_params_p->reps, sizeof(double *))) == NULL)
    {
	debug_stderr("run_test: malloc test_time_matrix of size %d "
		     "failed\n", 
		     (int) (test_params_p->reps * sizeof(double *)));
	return -1;
    }
    for (i = 0; i < test_params_p->reps; i++)
    {
	if ((test_time_matrix[i] = (double *) 
	     calloc(MAX_TIME, sizeof(double))) == NULL)
	{
	    debug_stderr("run_test: malloc test_time_matrix of size %d "
			 "failed\n", (int) (MAX_TIME * sizeof(double)));
	    return -1;
	}
    }
    memset(final_time_arr, 0, MAX_TIME * sizeof(double));

    /* Set up current test parameters (region_count, region_size,
     * region_spacing, base dtype, and general disp) */
    create_test_parameters(test_type, param_val, test_params_p,
			   &region_count, &region_size, &region_spacing, 
			   &base_dtype, &general_disp);    
    fileview_disp = general_disp;

    /* Create appropriate memory types and file types */
    create_access_pattern(test_params_p->pattern_dtype,
			  test_params_p->myid,
			  test_params_p->numprocs,
			  noncontig_type,
			  region_count,
			  region_size,
			  region_spacing,
			  &base_dtype,
			  &memtype_ct,
			  &memtype,
			  &filetype,
			  &fileview_disp);

    MPI_Type_size(base_dtype, &base_dtype_sz);
    MPI_Type_size(memtype, &memtype_size);
    MPI_Type_size(filetype, &filetype_size);
    
    MPI_Type_extent(base_dtype, &base_dtype_ext);
    MPI_Type_extent(memtype, &memtype_ext);
    MPI_Type_extent(filetype, &filetype_ext);

    MPI_Type_lb(memtype, &memtype_lb);

    assert(memtype_size == filetype_size);

    /* Allocate and initialize the buffer */
    buf_sz = memtype_lb + ((int64_t) memtype_ct * memtype_ext);
    assert(buf_sz < INT_MAX);
    if ((buf = (char *) malloc(buf_sz)) == NULL)
    {
	debug_stderr("run_test: malloc buf of size %Ld failed\n", buf_sz);
	return -1;
    }

    for (i = 0; i < test_params_p->reps; i++)
    {
	MPI_Status status;

	/* Generate the correct filename (depends if we are testing
	 * the same file for multiple reps) */
	generate_filename(filename, MAX_FILENAME_SIZE, 
			  test_params_p->pattern_dtype,
			  test_type, noncontig_type, io_method_type, 
			  region_count, region_size, region_spacing, 
			  param_val, i, test_params_p);

	memset(buf, DEFAULT_CHAR, buf_sz * sizeof(char));
	if (rw_type == WRITE)
	    init_data_buf(buf, buf_sz, test_params_p->myid, 
			  test_params_p->numprocs, noncontig_type,
			  region_count, region_size, region_spacing,
			  base_dtype_sz, memtype_ct, &memtype);

	MPI_Barrier(MPI_COMM_WORLD);

	/* Remove the old files before writing to it (just the
	 * first time if were using the same file). */
	if (test_params_p->myid == 0 && rw_type == WRITE)
	{
	    if ((test_params_p->same_file == FALSE || i == 0) &&
		test_params_p->gen_files != GEN_FULL)
	    {
		if (test_params_p->keep_files == FALSE)
		    ret = MPI_File_delete(filename, MPI_INFO_NULL);
	    }
	}

	MPI_Barrier(MPI_COMM_WORLD);
	    
	gettimeofday(&tp,&tzp);
	begin_time = ((double) tp.tv_sec + (double) tp.tv_usec * 1.e-6);
	    
	/* Open the files and set the file view (just the first time
	 * if we are using the same file) */
	if (test_params_p->same_file == FALSE || i == 0)
	{
	    MPI_Offset file_sz = -1;
	    MPI_Aint base_dtype_lb = -1;

	    mask_stdout(MASK_IO, "Open: filename=%s (fsync_method=%s) "
			"on rep %d\n", filename, 
			fsync_method_name[test_params_p->fsync_method], i);
	    if ((io_method_type == COLLECTIVE) ||
		(test_params_p->fsync_method == COLLECTIVE))
	    {
		ret = MPI_File_open(MPI_COMM_WORLD, filename,
				    MPI_MODE_CREATE | MPI_MODE_RDWR,
				    *(test_params_p->info_p),
				    &(test_params_p->fh));
	    }
	    else
	    {
		ret = MPI_File_open(MPI_COMM_SELF, filename,
				    MPI_MODE_CREATE | MPI_MODE_RDWR,
				    *(test_params_p->info_p),
				    &(test_params_p->fh));
	    }
	    if (ret != MPI_SUCCESS)
	    {
		debug_stderr("run_test: MPI_File_open file %s failed\n",
			     filename);
		return -1;
	    }

	    /* Use MPI atomic mode */
	    ret = MPI_File_set_atomicity(test_params_p->fh, 
					 test_params_p->atomicity);
	    if (ret != MPI_SUCCESS)
	    {
		debug_stderr("run_test: MPI_File_set_atomicity of %d failed\n",
			     test_params_p->atomicity);
		return -1;
	    }

	    /* Make sure to set the file size correctly so that
	     * processes do not read beyond the end of file.  Any
	     * base_dtype which has a upper bound which is beyond the
	     * last byte to be written must also be checked too. */
	    MPI_Type_lb(base_dtype, &base_dtype_lb);
	    if (noncontig_type == C_C || noncontig_type == NC_C)
		file_sz = general_disp + 
		    ((int64_t) test_params_p->numprocs * region_count * 
		     region_size * base_dtype_sz);
	    else
		file_sz = general_disp + base_dtype_lb + 
		    ((test_params_p->numprocs * 
		      (((int64_t) region_size * base_dtype_ext) + 
		       region_spacing)) * region_count) - 
		    region_spacing;

	    /* Everyone must call this. */
	    ret = MPI_File_set_size(test_params_p->fh, file_sz);
	    
	    if (has_mask(MASK_IO))
		debug_stdout("run_test: MPI_File_set_size on file %s "
			     "of size %Ld\n", filename, file_sz);
	}

	ret = MPI_File_set_view(test_params_p->fh, 
				fileview_disp, MPI_CHAR, filetype,
				"native", *(test_params_p->info_p));
	if (ret != MPI_SUCCESS)
	{
	    debug_stderr("run_test: MPI_File_set_view on file %s failed\n",
			 filename);
	    return -1;
	}
	ret = MPI_File_seek(test_params_p->fh, 0, MPI_SEEK_SET);
	if (ret != MPI_SUCCESS)
	{
	    debug_stderr("run_test: MPI_File_seek on file %s failed\n",
			 filename);
	    return -1;
	}

	if (use_mode == TEST_MODE)
	    MPI_Barrier(MPI_COMM_WORLD);

	gettimeofday(&tp,&tzp);
	test_time_matrix[i][TIME_OPEN] = 
	    ((double) tp.tv_sec + (double) tp.tv_usec * 1.e-6);
	
	if (has_mask(MASK_MULTIPLE_PROCESSES))
	{
	    for (j = 0; j < test_params_p->numprocs; j++)
	    {
		if (test_params_p->myid == j)
		{
		    j++;
		    break;
		}
		MPI_Barrier(MPI_COMM_WORLD);
	    }
	}

	mask_stdout(MASK_MULTIPLE_PROCESSES, 
		    "proc %d: %s\n", test_params_p->myid, 
		    rw_name[rw_type]);
	
	/* Do I/O */
	if (rw_type == WRITE)
	{
	    mask_stdout(MASK_IO, 
			"Write: io_method=%s on rep %d\n", 
			io_method_name[io_method_type], i);
	    
	    if (io_method_type == COLLECTIVE)
		ret = MPI_File_write_all(test_params_p->fh, buf, 
					 memtype_ct, memtype, 
					 &status);
	    else
		ret = MPI_File_write(test_params_p->fh, buf, 
				     memtype_ct, memtype, &status);
	}
	else
	{
	    mask_stdout(MASK_IO, "Read: io_method=%s on rep %d\n", 
			io_method_name[io_method_type], i);
	    
	    if (io_method_type == COLLECTIVE)
		ret = MPI_File_read_all(test_params_p->fh, buf, 
					memtype_ct, memtype, 
					&status);
	    else
		ret = MPI_File_read(test_params_p->fh, buf, 
				    memtype_ct, memtype, &status);
	}
	if (ret != MPI_SUCCESS)
	{
	    debug_stderr("run_test: MPI_File_(read|write)(|_all) "
			 "on file %s failed\n", filename);
	    return -1;
	    
	}
	if (has_mask(MASK_MULTIPLE_PROCESSES))
	{
	    for (;j < test_params_p->numprocs; j++)
		MPI_Barrier(MPI_COMM_WORLD);
	}

	/* Check to see whether the correct amount was written/read */
        ret = check_io_status(&status, &memtype, memtype_ct,
                              count, region_count, 
			      region_size * base_dtype_sz,
                              test_params_p);
	
	gettimeofday(&tp,&tzp);
	test_time_matrix[i][TIME_IO] = 
	    ((double) tp.tv_sec + (double) tp.tv_usec * 1.e-6);

	/* Sync the files */
	if (rw_type == WRITE && test_params_p->enable_fsync == TRUE)
	{
	    mask_stdout(MASK_IO, "Sync: filename=%s (fsync_method=%s) "
			"on rep %d\n", filename, 
			fsync_method_name[test_params_p->fsync_method], i);

	    ret = MPI_File_sync(test_params_p->fh);
	    if (ret != MPI_SUCCESS)
	    {
		debug_stderr("run_test: MPI_File_sync on file %s failed\n",
			     filename);
		return -1;
	    }
	}
	
	if (use_mode == TEST_MODE)
	    MPI_Barrier(MPI_COMM_WORLD);	    

	gettimeofday(&tp,&tzp);
	test_time_matrix[i][TIME_SYNC] = 
	    ((double) tp.tv_sec + (double) tp.tv_usec * 1.e-6);

	/* Close the file (only on the last rep if we are using the
	 * same file) */
	if (test_params_p->same_file == FALSE || 
	    i == test_params_p->reps - 1)
	{
	    mask_stdout(MASK_IO, "Close: filename=%s on rep %d\n", 
			filename, i);

	    ret = MPI_File_close(&(test_params_p->fh));
	    if (ret != MPI_SUCCESS)
	    {
		debug_stderr("run_test: MPI_File_close on file %s failed\n",
			     filename);
		return -1;
	    }
	}
	
	gettimeofday(&tp,&tzp);
	test_time_matrix[i][TIME_CLOSE] = 
	    ((double) tp.tv_sec + (double) tp.tv_usec * 1.e-6);
	
	/* Delete the file after read (only on the last rep if we are
	 * using the same file) */
	if (test_params_p->same_file == FALSE || 
	    i == test_params_p->reps - 1)
	{
	    if (rw_type == READ)
		MPI_Barrier(MPI_COMM_WORLD);

	    /* Remove the files after reading it to clean up. */
	    if (test_params_p->myid == 0 && rw_type == READ &&
		test_params_p->keep_files == FALSE)
		MPI_File_delete(filename, MPI_INFO_NULL);
	}

	/* Do the time calculations */
	test_time_matrix[i][TIME_TOTAL]  = test_time_matrix[i][TIME_CLOSE] - 
	    begin_time;
	test_time_matrix[i][TIME_CLOSE] -= test_time_matrix[i][TIME_SYNC];
	test_time_matrix[i][TIME_SYNC]  -= test_time_matrix[i][TIME_IO];
	test_time_matrix[i][TIME_IO]    -= test_time_matrix[i][TIME_OPEN];
	test_time_matrix[i][TIME_OPEN]  -= begin_time;

	if (use_mode == TEST_MODE)
	{
	    double iosync = 0.0;

	    /* Let things settle a bit */
	    usleep(test_params_p->wait_time);	    
	
	    /* Decide on whether to do more repetitions */
	    iosync = test_time_matrix[i][TIME_IO] +
		test_time_matrix[i][TIME_SYNC];
	    MPI_Allreduce(&(iosync), 
			  &(test_time_matrix[i][TIME_AGG_MAX_IOSYNC]),
			  1, MPI_DOUBLE, MPI_MAX, MPI_COMM_WORLD);

	    if (test_time_matrix[i][TIME_AGG_MAX_IOSYNC] > 
		test_params_p->rep_max_time)
	    {
		/* Add one rep here because we start on zero.  Also,
		 * the finally j has one bonus added since it failed
		 * the comparison */
		i++;
		break;
	    }
	}

	MPI_Barrier(MPI_COMM_WORLD);
    }

    if (use_mode == TEST_MODE)
    {
	/* Figure out the average correctly based on the number of
	 * reps that completed.  These averages are based on the
	 * aggregate max times so that processes agree on which reps
	 * are the average. */
	reps_completed = i;

	if (has_mask(MASK_AVERAGE))
	    print_time_matrix(test_time_matrix, i);

	test_average(reps_completed,
		     test_params_p,
		     test_time_matrix,
		     final_time_arr);

	if (has_mask(MASK_AVERAGE))
	    print_time_arr(final_time_arr);

	/* An average rep has been places in final_time_arr.  Process
	 * 0 prints out the process's data  who took the longest. */
	if ((numproc_final_time_arr = (double *) 
	     malloc(test_params_p->numprocs * sizeof(double))) == NULL)
	{
	    debug_stderr("malloc numproc_final_time_arr of size %d failed\n",
			 (int) (test_params_p->numprocs * sizeof(double)));
	    return -1;
	}

	MPI_Allgather(&(final_time_arr[TIME_TOTAL]),
		      1, MPI_DOUBLE, numproc_final_time_arr, 1, 
		      MPI_DOUBLE, MPI_COMM_WORLD);
	max_index = 0;
	for (i = 0; i < test_params_p->numprocs; i++)
	{
	    if (numproc_final_time_arr[i] > numproc_final_time_arr[max_index])
		max_index = i;
	}
	free(numproc_final_time_arr);
	
	/* Reuse final_time_arr on process 0 to print out the final
	 * time for all the processes */
	if (test_params_p->myid == max_index && max_index != 0)
	    MPI_Send(final_time_arr, MAX_TIME, MPI_DOUBLE,
		     0, 0, MPI_COMM_WORLD);
	else if (test_params_p->myid == 0 && max_index != 0)
	    MPI_Recv(final_time_arr, MAX_TIME, MPI_DOUBLE,
		     max_index, 0, MPI_COMM_WORLD, MPI_STATUS_IGNORE);
    }

    /* Print out statistics*/
    if (test_params_p->myid == 0)
    {
	if (use_mode == TEST_MODE)
	{
	    print_min_timing(stdout, reps_completed, test_type,
			     region_count, region_size, region_spacing,
			     param_val, final_time_arr,
			     test_params_p);
	    print_min_timing(test_params_p->output_min_results, 
			     reps_completed, test_type,
			     region_count, region_size, region_spacing,
			     param_val, final_time_arr,
			     test_params_p);
	    print_full_timing(test_params_p->output_results, test_type,
			      noncontig_type, test_params_p->pattern_dtype, 
			      rw_type, io_method_type, reps_completed, 
			      general_disp, &base_dtype,
			      region_count, region_size, region_spacing,
			      param_val, final_time_arr, 
			      test_params_p);
	}
	else if (use_mode == GENERATE_MODE)
	{
	    debug_stdout_stream(test_params_p->output_info, 
				"Generated file %s\n", filename);
	}
    }

    if (rw_type == WRITE)
	free(buf);

    /* Verify the last repetition if you like for writing/reading */
    if (test_params_p->verify > VERIFY_NONE && use_mode == TEST_MODE)
    {
	ret = check_data(buf, buf_sz,
			 filename,
			 rw_type,
			 test_type,
			 noncontig_type,
			 io_method_type,
			 param_val,
			 general_disp,
			 region_count,
			 region_size,
			 region_spacing,
			 &base_dtype,
			 test_params_p);
	if (ret < 0)
	    return ret;
    }
    
    if (rw_type == READ)
	free(buf);

    if (base_dtype != MPI_CHAR)
	MPI_Type_free(&base_dtype);
    MPI_Type_free(&memtype);
    MPI_Type_free(&filetype);


    for (i = 0; i < test_params_p->reps; i++)
        free(test_time_matrix[i]);
    free(test_time_matrix);
    
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
