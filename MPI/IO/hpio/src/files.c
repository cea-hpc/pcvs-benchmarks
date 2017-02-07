#include "buf.h"
#include <time.h>
#include "test.h"
#include "files.h"
#include "cache_flush.h"
#include "tmpi/tmpi.h"

#define MAX_BUFFER_SIZE 16*1024*1024

int create_output_filenames(char *output_info, 
			    char *output_header, 
			    char *output_min_results,
			    char *output_results,
			    char *output_failed,
			    int max_filename_sz, 
			    struct test_params_s *test_params_p)
{
    time_t tp;
    struct timeval tv;
    int tmp_buf_sz = 0, tmp_buf_remain = 0, tmp_buf_used = 0;
	
    memset(output_info, 0, max_filename_sz);
    memset(output_header, 0, max_filename_sz);
    memset(output_min_results, 0, max_filename_sz);
    memset(output_results, 0, max_filename_sz);
    memset(output_failed, 0, max_filename_sz);

    /* Add the directory name if there is one */
    if (test_params_p->output_dir != NULL)
    {
	tmp_buf_remain = max_filename_sz - tmp_buf_sz;
	tmp_buf_used = snprintf(&output_info[tmp_buf_sz], 
				tmp_buf_remain, "%s/", 
				test_params_p->output_dir);
	if (tmp_buf_used < 0 || tmp_buf_used >= tmp_buf_remain)
	{
	    debug_stderr_stream(
		test_params_p->output_info,
		"** Error: Ran out of memory in creating filename\n");
	    return -1;
	}
	else
	    tmp_buf_sz += tmp_buf_used;
    }
    /* Add a date, time, and timezone */
    gettimeofday(&tv, 0);
    tp = tv.tv_sec;
    tmp_buf_remain = max_filename_sz - tmp_buf_sz;
    tmp_buf_used = strftime(&output_info[tmp_buf_sz], tmp_buf_remain,
			    "%Y.%m.%d.%H.%M.S.%Z", localtime(&tp));
    if (tmp_buf_used < 0 || tmp_buf_used >= tmp_buf_remain)
    {
	debug_stderr_stream(
	    test_params_p->output_info,
	    "** Error: Ran out of memory in creating filename\n");
	return -1;
    }
    else
	tmp_buf_sz += tmp_buf_used;
    /* Add the "info" */
    tmp_buf_remain = max_filename_sz - tmp_buf_sz;
    tmp_buf_used = snprintf(&output_info[tmp_buf_sz], 
			    tmp_buf_remain, ".info");
    if (tmp_buf_used < 0 || tmp_buf_used >= tmp_buf_remain)
    {
        debug_stderr_stream(
            test_params_p->output_info,
            "** Error: Ran out of memory in creating filename\n");
	return -1;
    }
    else
	tmp_buf_sz += tmp_buf_used;

    /* Now the "header" file */	
    tmp_buf_sz = 0;
    tmp_buf_remain = 0;
    tmp_buf_used = 0;

    /* Add the directory name if there is one */
    if (test_params_p->output_dir != NULL)
    {
	tmp_buf_remain = max_filename_sz - tmp_buf_sz;
	tmp_buf_used = snprintf(&output_header[tmp_buf_sz], 
				tmp_buf_remain, "%s/", 
				test_params_p->output_dir);
	if (tmp_buf_used < 0 || tmp_buf_used >= tmp_buf_remain)
	{
	    debug_stderr_stream(
		test_params_p->output_info,
		"** Error: Ran out of memory in creating filename\n");
	    return -1;
	}
	else
	    tmp_buf_sz += tmp_buf_used;
    }
    /* Add a date, time, and timezone */
    gettimeofday(&tv, 0);
    tp = tv.tv_sec;
    tmp_buf_remain = max_filename_sz - tmp_buf_sz;
    tmp_buf_used = strftime(&output_header[tmp_buf_sz], tmp_buf_remain,
			    "%Y.%m.%d.%H.%M.S.%Z", localtime(&tp));
    if (tmp_buf_used < 0 || tmp_buf_used >= tmp_buf_remain)
    {
        debug_stderr_stream(
            test_params_p->output_info,
            "** Error: Ran out of memory in creating filename\n");
	return -1;
    }
    else
	tmp_buf_sz += tmp_buf_used;
    /* Add the "header" */
    tmp_buf_remain = max_filename_sz - tmp_buf_sz;
    tmp_buf_used = snprintf(&output_header[tmp_buf_sz], 
			    tmp_buf_remain, ".header"); 
    if (tmp_buf_used < 0 || tmp_buf_used >= tmp_buf_remain)
    {
        debug_stderr_stream(
            test_params_p->output_info,
            "** Error: Ran out of memory in creating filename\n");
	return -1;
    }
    else
	tmp_buf_sz += tmp_buf_used;

    /* Now the "min_results" file */	
    tmp_buf_sz = 0;
    tmp_buf_remain = 0;
    tmp_buf_used = 0;

    /* Add the directory name if there is one */
    if (test_params_p->output_dir != NULL)
    {
	tmp_buf_remain = max_filename_sz - tmp_buf_sz;
	tmp_buf_used = snprintf(&output_min_results[tmp_buf_sz], 
				tmp_buf_remain, "%s/", 
				test_params_p->output_dir);
	if (tmp_buf_used < 0 || tmp_buf_used >= tmp_buf_remain)
	{
	    debug_stderr_stream(
		test_params_p->output_info,
		"** Error: Ran out of memory in creating filename\n");
	    return -1;
	}
	else
	    tmp_buf_sz += tmp_buf_used;
    }
    /* Add a date, time, and timezone */
    gettimeofday(&tv, 0);
    tp = tv.tv_sec;
    tmp_buf_remain = max_filename_sz - tmp_buf_sz;
    tmp_buf_used = strftime(&output_min_results[tmp_buf_sz], tmp_buf_remain,
			    "%Y.%m.%d.%H.%M.S.%Z", localtime(&tp));
    if (tmp_buf_used < 0 || tmp_buf_used >= tmp_buf_remain)
    {
        debug_stderr_stream(
            test_params_p->output_info,
            "** Error: Ran out of memory in creating filename\n");
	return -1;
    }
    else
	tmp_buf_sz += tmp_buf_used;
    /* Add the "min_results" */
    tmp_buf_remain = max_filename_sz - tmp_buf_sz;
    tmp_buf_used = snprintf(&output_min_results[tmp_buf_sz], 
			    tmp_buf_remain, ".min_results");
    if (tmp_buf_used < 0 || tmp_buf_used >= tmp_buf_remain)
    {
        debug_stderr_stream(
            test_params_p->output_info,
            "** Error: Ran out of memory in creating filename\n");
	return -1;
    }
    else
	tmp_buf_sz += tmp_buf_used;

    /* Now the "results" file */	
    tmp_buf_sz = 0;
    tmp_buf_remain = 0;
    tmp_buf_used = 0;

    /* Add the directory name if there is one */
    if (test_params_p->output_dir != NULL)
    {
	tmp_buf_remain = max_filename_sz - tmp_buf_sz;
	tmp_buf_used = snprintf(&output_results[tmp_buf_sz], 
				tmp_buf_remain, "%s/", 
				test_params_p->output_dir);
	if (tmp_buf_used < 0 || tmp_buf_used >= tmp_buf_remain)
	{
	    debug_stderr_stream(
		test_params_p->output_info,
		"** Error: Ran out of memory in creating filename\n");
	    return -1;
	}
	else
	    tmp_buf_sz += tmp_buf_used;
    }
    /* Add a date, time, and timezone */
    gettimeofday(&tv, 0);
    tp = tv.tv_sec;
    tmp_buf_remain = max_filename_sz - tmp_buf_sz;
    tmp_buf_used = strftime(&output_results[tmp_buf_sz], tmp_buf_remain,
			    "%Y.%m.%d.%H.%M.S.%Z", localtime(&tp));
    if (tmp_buf_used < 0 || tmp_buf_used >= tmp_buf_remain)
    {
        debug_stderr_stream(
            test_params_p->output_info,
            "** Error: Ran out of memory in creating filename\n");
	return -1;
    }
    else
	tmp_buf_sz += tmp_buf_used;
    /* Add the "results" */
    tmp_buf_remain = max_filename_sz - tmp_buf_sz;
    tmp_buf_used = snprintf(&output_results[tmp_buf_sz], 
			    tmp_buf_remain, ".results");
    if (tmp_buf_used < 0 || tmp_buf_used >= tmp_buf_remain)
    {
        debug_stderr_stream(
            test_params_p->output_info,
            "** Error: Ran out of memory in creating filename\n");
	return -1;
    }
    else
	tmp_buf_sz += tmp_buf_used;

    /* Now the "failed" file */	
    tmp_buf_sz = 0;
    tmp_buf_remain = 0;
    tmp_buf_used = 0;

    /* Add the directory name if there is one */
    if (test_params_p->output_dir != NULL)
    {
	tmp_buf_remain = max_filename_sz - tmp_buf_sz;
	tmp_buf_used = snprintf(&output_failed[tmp_buf_sz], 
				tmp_buf_remain, "%s/", 
				test_params_p->output_dir);
	if (tmp_buf_used < 0 || tmp_buf_used >= tmp_buf_remain)
	{
	    debug_stderr_stream(
		test_params_p->output_info,
		"** Error: Ran out of memory in creating filename\n");
	    return -1;
	}
	else
	    tmp_buf_sz += tmp_buf_used;
    }
    /* Add a date, time, and timezone */
    gettimeofday(&tv, 0);
    tp = tv.tv_sec;
    tmp_buf_remain = max_filename_sz - tmp_buf_sz;
    tmp_buf_used = strftime(&output_failed[tmp_buf_sz], tmp_buf_remain,
			    "%Y.%m.%d.%H.%M.S.%Z", localtime(&tp));
    if (tmp_buf_used < 0 || tmp_buf_used >= tmp_buf_remain)
    {
        debug_stderr_stream(
            test_params_p->output_info,
            "** Error: Ran out of memory in creating filename\n");
	return -1;
    }
    else
	tmp_buf_sz += tmp_buf_used;
    /* Add the "failed" */
    tmp_buf_remain = max_filename_sz - tmp_buf_sz;
    tmp_buf_used = snprintf(&output_failed[tmp_buf_sz], 
			    tmp_buf_remain, ".failed");
    if (tmp_buf_used < 0 || tmp_buf_used >= tmp_buf_remain)
    {
        debug_stderr_stream(
            test_params_p->output_info,
            "** Error: Ran out of memory in creating filename\n");
	return -1;
    }
    else
	tmp_buf_sz += tmp_buf_used;

    return 0;
}

int disp_io(MPI_File fh, char *buf, MPI_Offset disp, int rw_type)
{
    MPI_Status status;
    int ret = -1;

    if (disp > INT_MAX)
    {
	debug_stderr("disp_io: Error - disp is greater than INT_MAX\n.");
	return -1;
    }

    ret = tMPI_File_set_view(fh, 0, MPI_CHAR, MPI_CHAR, 
			     "native", MPI_INFO_NULL);
    if (ret != MPI_SUCCESS)
	return -1;
    
    ret = tMPI_File_seek(fh, 0, MPI_SEEK_SET);
    if (ret != MPI_SUCCESS)
	return -1;

    if (rw_type == WRITE)
	ret = tMPI_File_write(fh, buf, (int) disp, MPI_CHAR, &status);
    else
	ret = tMPI_File_read(fh, buf, (int) disp, MPI_CHAR, &status);
    if (ret != MPI_SUCCESS)
	return -1;
    return 0;
}

/* alloc_and_fill_io_buffers: Given a set of test parameters, allocate
 * and fill up io_buf and data_buf with written data or full up io_buf
 * and data buf with data to be written */
int alloc_and_fill_io_buffers(
    char *filename, int myid, int numprocs,
    int region_count, int region_size, int region_spacing,
    int noncontig_type, MPI_Datatype *base_dtype_p, 
    MPI_Offset fileview_disp, int rw_type,
    struct test_params_s *test_params_p, 
    char **io_buf_p, int64_t *io_buf_sz_p, 
    char **data_buf_p, int64_t *data_buf_sz_p)
{
    MPI_Offset disp = -1;
    int elements = 0, i, j, ret = -1;
    int64_t domain_sz = -1;
    int64_t global_test_dtype_blocknum = 0;
    MPI_File fh;
    MPI_Status status;
    int base_dtype_sz = -1, buf_pos = 0;
    MPI_Aint base_dtype_ext = -1, base_dtype_lb = -1, data_buf_dtype_ext = -1;
    MPI_Datatype data_buf_dtype;

    MPI_Type_size(*base_dtype_p, &base_dtype_sz);
    MPI_Type_extent(*base_dtype_p, &base_dtype_ext);
        
    /* If the pattern is noncontig is contig in file then MPI_LB = 0
     * and the buffer is simply the size of the domain size. */
    if (noncontig_type == C_C || noncontig_type == NC_C)
    {
	domain_sz = (int64_t) base_dtype_sz * region_size *
	    region_count;
    
	MPI_Type_contiguous(region_count * region_size * base_dtype_sz, 
			    MPI_CHAR, &data_buf_dtype);

	base_dtype_lb = 0;
	*io_buf_sz_p = (int64_t) domain_sz;
	*data_buf_sz_p = (int64_t) domain_sz;
    }
    else 
    {
	/* While the domain_sz is correct, buffers need to be bigger
	 * than the actual data to write since there may be an intial
	 * MPI_LB. */
	domain_sz = (((int64_t) base_dtype_ext * region_size) + 
		     region_spacing) * region_count;
    
	MPI_Type_hvector(region_count, region_size, 
			 (base_dtype_ext * region_size) + 
			 region_spacing,
			 *base_dtype_p, &data_buf_dtype);

	MPI_Type_lb(*base_dtype_p, &base_dtype_lb);    
	if (myid == numprocs - 1)
	    *io_buf_sz_p = base_dtype_lb + domain_sz - region_spacing;
	else
	    *io_buf_sz_p = base_dtype_lb + domain_sz;
	*data_buf_sz_p = (int64_t) region_count * region_size * base_dtype_sz;
    }

    MPI_Type_commit(&data_buf_dtype);
    MPI_Type_extent(data_buf_dtype, &data_buf_dtype_ext);

    assert((int64_t) data_buf_dtype_ext >= *data_buf_sz_p);    
    
    /* *data_buf_p is the buffer to handle data which is unpacked into
     * the *io_buf_p */
    if ((*io_buf_p = (char *) malloc(*io_buf_sz_p * sizeof(char))) 
	== NULL)
    {
	debug_stderr_stream(
	    test_params_p->output_info,
	    "alloc_and_fill_io_buffers: malloc *io_buf_p "
	    "of size %Ld failed\n", *io_buf_sz_p * sizeof(char));
	return -1;
    }
    if ((*data_buf_p = (char *) malloc(*data_buf_sz_p * sizeof(char))) 
	== NULL)
    {
	debug_stderr_stream(
	    test_params_p->output_info,
	    "alloc_and_fill_io_buffers: malloc *data_buf_p "
	    "of size %Ld failed\n", *data_buf_sz_p * sizeof(char));
	return -1;
    }

    disp = fileview_disp + base_dtype_lb + ((MPI_Offset) myid * domain_sz);

    /* Process 0 handles the disp and the MPI_LB bytes */
    if (myid == 0 && rw_type == WRITE)
    {
	int64_t disp_lb_sz = 0;
	char *disp_buf = NULL;

	ret = tMPI_File_open(MPI_COMM_SELF, filename, MPI_MODE_CREATE | 
			     MPI_MODE_WRONLY, MPI_INFO_NULL, &fh);
	if (ret != MPI_SUCCESS)
	    return -1;
	
	disp_lb_sz = fileview_disp + base_dtype_lb;

	if ((disp_buf = (char *) malloc(disp_lb_sz * sizeof(char))) == NULL)
	{        
	    debug_stderr_stream(
		test_params_p->output_info,
		"alloc_and_fill_io_buffers: malloc disp_buf of size "
		"%Ld failed\n", disp_lb_sz);
	    return -1;
	}
	memset(disp_buf, DEFAULT_DISP_CHAR, disp_lb_sz * sizeof(char));
	ret = disp_io(fh, disp_buf, disp_lb_sz, WRITE);
	if (ret < 0)
	    return -1;
	free(disp_buf);
	ret = tMPI_File_close(&fh);
	if (ret != MPI_SUCCESS)
	    return ret;
    }

    /* Everyone writes their necessary bytes */
    if (rw_type == WRITE)
	ret = tMPI_File_open(MPI_COMM_WORLD, filename, MPI_MODE_CREATE | 
			     MPI_MODE_WRONLY, MPI_INFO_NULL, &fh);
    else
	ret = tMPI_File_open(MPI_COMM_WORLD, filename, MPI_MODE_RDONLY, 
			     MPI_INFO_NULL, &fh);
    if (ret != MPI_SUCCESS)
	return -1;

    /* When writing data to file, default chars should be written to
     * ensure that writes are doing the correct process.  However, if
     * it is at all possible that a read may occur without a preceding
     * write then the data must be initialized to the correct write
     * data. */
    memset(*io_buf_p, (char) DEFAULT_CHAR, *io_buf_sz_p * sizeof(char));
    memset(*data_buf_p, (char) DEFAULT_CHAR, 
	   *data_buf_sz_p * sizeof(char));
    if (rw_type == WRITE && 
	((test_params_p->enable_resume == TRUE && 
	  test_params_p->rw[READ] == TRUE) ||
	 (test_params_p->rw[WRITE] == FALSE &&
	  test_params_p->rw[READ] == TRUE)))
    {
	global_test_dtype_blocknum = myid * region_count;
	for (i = 0; i < region_count; i++)
	{
	    for (j = 0; j < region_size * base_dtype_sz; j++)
	    {
		assert((i * region_size * base_dtype_sz) + j < *data_buf_sz_p);
		(*data_buf_p)[(i * region_size * base_dtype_sz) + j] = 
		    gen_file_char(global_test_dtype_blocknum + j);
	    }
	    global_test_dtype_blocknum++;
	}
	buf_pos = 0;
	MPI_Unpack(*data_buf_p, *data_buf_sz_p, &buf_pos,
		   &((*io_buf_p)[base_dtype_lb]), 1, data_buf_dtype, 
		   MPI_COMM_SELF);
    }

    ret = tMPI_File_set_view(fh, disp, MPI_CHAR, MPI_CHAR, 
			     "native", MPI_INFO_NULL);
    if (ret != MPI_SUCCESS)
	return -1;
    
    ret = tMPI_File_seek(fh, 0, MPI_SEEK_SET);
    if (ret != MPI_SUCCESS)
	return -1;

    assert(*io_buf_sz_p <= INT_MAX && (int) *io_buf_sz_p > 0);
    if (rw_type == WRITE)
	ret = tMPI_File_write(fh, &((*io_buf_p)[base_dtype_lb]), 
			      (int) (*io_buf_sz_p - base_dtype_lb), 
			      MPI_CHAR, &status);
    else
	ret = tMPI_File_read(fh, &((*io_buf_p)[base_dtype_lb]), 
			     (int) (*io_buf_sz_p - base_dtype_lb), 
			     MPI_CHAR, &status);
    if (ret != MPI_SUCCESS)
	return -1;
    
    MPI_Get_elements(&status, MPI_CHAR, &elements);
    if (elements != *io_buf_sz_p - base_dtype_lb)
    {
        debug_stdout_stream(
            test_params_p->output_info,
	    "alloc_and_fill_io_buffers: Warning - amount to write/read "
	    "(%Ld) != actual amount (%d)\n", 
	    *io_buf_sz_p - base_dtype_lb,
	    elements);
    }

    ret = tMPI_File_close(&fh);
    if (ret != MPI_SUCCESS)
	return ret;

    /* For the read case, *io_buf_p needs to be cleared out in the
     * data that was written so that the rest of the data can be
     * easily checked.  Also, data_buf needs to be filled in. */
    if (rw_type == READ)
    {
	int data_buf_pos = 0, def_data_buf_pos = 0;
	char *def_data_buf = NULL;

	MPI_Pack(*io_buf_p, 1, data_buf_dtype,
		 *data_buf_p, *data_buf_sz_p, &data_buf_pos, MPI_COMM_SELF);
	assert(data_buf_pos == *data_buf_sz_p);

	/* Filling the buffer back into with DEFAULT_INIT_CHAR
	 * provides a way to keep track of where everything is in
	 * file. */
	if ((def_data_buf = (char *) malloc(*data_buf_sz_p * sizeof(char))) 
	    == NULL)
	{
	    debug_stderr_stream(
		test_params_p->output_info,
		"alloc_and_fill_io_buffers: malloc def_data_buf "
		"of size %Ld failed\n", *data_buf_sz_p);
	    return -1;
	}

	memset(def_data_buf, DEFAULT_INIT_CHAR, *data_buf_sz_p);
	MPI_Unpack(def_data_buf, *data_buf_sz_p, &def_data_buf_pos,
		   *io_buf_p, 1, data_buf_dtype, 
		   MPI_COMM_SELF);

	free(def_data_buf);
    }

    MPI_Type_free(&data_buf_dtype);
    return 0;
}

/* gen_files: Use all processes to generate a file which can be either
 * blank or filled with the correct data for read testing. */
int gen_files(char *filename, int myid, int numprocs,
	      int region_count, int region_size, int region_spacing, 
	      int noncontig_type, MPI_Datatype *base_dtype_p, 
	      MPI_Offset fileview_disp,
	      struct test_params_s *test_params_p)
{
    char *io_buf = NULL, *data_buf = NULL;
    int64_t io_buf_sz = -1, data_buf_sz = -1;
    double time;
    
    time = MPI_Wtime();
    alloc_and_fill_io_buffers(
	filename, myid, numprocs,
	region_count, region_size, region_spacing,
	noncontig_type, base_dtype_p, fileview_disp, WRITE,
	test_params_p, &io_buf, &io_buf_sz, &data_buf, &data_buf_sz);
    free(io_buf);
    free(data_buf);

    if (myid == 0)
    {
	if ((test_params_p->enable_resume == TRUE &&
	     test_params_p->rw[READ] == TRUE) ||
	    (test_params_p->rw[WRITE] == FALSE &&
	     test_params_p->rw[READ] == TRUE))
	    debug_stdout_stream(test_params_p->output_info,
				"Wrote data-filled file %s\n", filename);
	else
	    debug_stdout_stream(test_params_p->output_info,
				"Wrote blank file %s\n", filename);
    }

    time = MPI_Wtime() - time;
    return 0;
}

int precreate_file(int test_type,
		   int noncontig_type,
		   int io_method_type,
		   int param_val,
		   struct test_params_s *test_params_p)
{
    int i, region_count, region_size, region_spacing;
    char filename[MAX_FILENAME_SIZE];
    MPI_Datatype base_dtype;
    MPI_Offset general_disp = 0;

    create_test_parameters(test_type, param_val, test_params_p,
			   &region_count, &region_size, &region_spacing,
			   &base_dtype, &general_disp);
    
    for (i = 0; i < test_params_p->reps; i++)
    {
	/* Generate the correct filename (depends if we are testing
	 * the same file for multiple reps) */
	generate_filename(filename, MAX_FILENAME_SIZE, 
			  test_params_p->pattern_dtype,
			  test_type, noncontig_type, io_method_type, 
			  region_count, region_size, region_spacing, 
			  param_val, i,
			  test_params_p);

	/* Remove the old files before writing to it (just the
	 * first time if were using the same file). */
	if (test_params_p->myid == 0 && 
	    (test_params_p->same_file == FALSE || 
	     i == 0))
	{
	    MPI_File_delete(filename, MPI_INFO_NULL);
	}
		    
	/* Now precreate the file! */
	gen_files(filename, test_params_p->myid, 
		  test_params_p->numprocs,
		  region_count, region_size, region_spacing, 
		  noncontig_type, &base_dtype, general_disp, 
		  test_params_p);
    }

    return 0;
}

/* Truth table for generating X (full files) and Y (partial files) */
/* gen_files == GEN_FULL && not resuming */
/*   W    R   W/R  */
/*  X|X  X|X  X|X  */
/* gen_files == GEN_FULL && resuming from WRITE */
/*   W    R   W/R  */
/*   |X  N/A  X|X  */
/* gen_files == GEN_FULL && resuming from READ */
/*   W    R   W/R  */
/*  N/A   |X   |X  */
/* gen_files == GEN_PARTIAL && not resuming */
/*   W    R   W/R  */
/*  _|_  Y|Y  _|_  */
/* gen_files == GEN_PARTIAL && resuming from WRITE */
/*   W    R   W/R  */
/*  _|_  N/A  Y|_  */
/* gen_files == GEN_PARTIAL && resuming from READ */
/*   W    R   W/R  */
/*  N/A   |Y   |Y  */
/* F = GEN_FULL, P = GEN_PARTIAL, w = write, r = read, wr =
 * write/read, ^R = not resuming, Rw = resuming from write, Rr =
 * resuming from read, ^ = not */
/* Case 1: Generate all possible files for either partial or full
 * mode.  This includes F*(^R) + F*Rw*wr + P(^R)*(^w)*r. */
/* Case 2: Generate files before the resume point.  This includes
 * P*Rw*wr. */ 
/* Case 3: Generate files after the resume point.  This includes
 * F*Rw*w*(^r) + F*Rr*((^w)*r+w*r) + P*Rr*((^w)*r+w*r), which should
 * equal F*Rw*w*(^r) + (F+P)*(Rr*r). */

int generate_files(struct test_params_s *test_params_p)
{
    int i, j, k, l, m;
    int k2 = FALSE, l2 = FALSE, m2 = FALSE, ret = -1;

    /* Nothing to do */
    if (test_params_p->gen_files == GEN_NONE)
	return 0;

    /* Case 1: See above */
    if ((test_params_p->gen_files     == GEN_FULL &&
	 test_params_p->enable_resume == FALSE) ||
	(test_params_p->gen_files         == GEN_FULL &&
	 test_params_p->enable_resume     == TRUE &&
	 test_params_p->resume_arr[VAR_RW] == WRITE &&
	 test_params_p->rw[WRITE]         == TRUE &&
	 test_params_p->rw[READ]          == TRUE) ||
	(test_params_p->gen_files     == GEN_PARTIAL &&
         test_params_p->enable_resume == FALSE &&
         test_params_p->rw[WRITE]     == FALSE &&
         test_params_p->rw[READ]      == TRUE))
    {
	if (test_params_p->myid == 0)
	    debug_stdout_stream(
		test_params_p->output_info,
		"** Generating all files (%s) before any "
		"writes or reads...\n", 
		gen_files_name[test_params_p->gen_files]);
	
	i = WRITE;
	for (j = 0; j < MAX_TEST; j++)
	{
	    if (test_params_p->test[j] == TRUE)
	    {
		for (k = 0; k < MAX_NONCONTIG; k++)
		{
		    if (test_params_p->noncontig[k] == TRUE)
		    {
			for (l = 0; l < MAX_IO_METHOD; l++)
			{
			    if (test_params_p->io_method[l] == TRUE)
			    {
				for (m = 0; m < params_count[j]; m++)
				{
				    if (test_params_p->gen_files == GEN_FULL)
					ret = precreate_file(
					    j, k, l, (params_arr_p[j])[m],
					    test_params_p);
				    else if (test_params_p->gen_files == 
					     GEN_PARTIAL)
					ret = run_test(i, j, k, l, 
						       (params_arr_p[j])[m],
						       GENERATE_MODE,
						       test_params_p);
				    
				    if (ret != 0)
					return -1;
				}
			    }
			}
		    }
		}
	    }
	}
	
	if (test_params_p->myid == 0)
	    debug_stdout_stream(
		test_params_p->output_info,
		"** End of complete file (%s) generation...\n\n",
		gen_files_name[test_params_p->gen_files]);
    } /* Case 2: See above */
    else if (test_params_p->gen_files          == GEN_PARTIAL &&
	     test_params_p->enable_resume      == TRUE &&
	     test_params_p->resume_arr[VAR_RW] == WRITE &&
	     test_params_p->rw[WRITE]          == TRUE &&
	     test_params_p->rw[READ]           == TRUE)
    {
	if (test_params_p->myid == 0)
	    debug_stdout_stream(
		test_params_p->output_info,
		"** Generating files (%s) before the resume point...\n", 
		gen_files_name[test_params_p->gen_files]);
	
	i = WRITE;
	for (j = 0; j < MAX_TEST; j++)
	{
	    if (test_params_p->test[j] == TRUE)
	    {
		if (j == test_params_p->resume_arr[VAR_TEST])
		    k2 = TRUE;
		for (k = 0; k < MAX_NONCONTIG; k++)
		{
		    if (test_params_p->noncontig[k] == TRUE)
		    {
			if (k == test_params_p->resume_arr[VAR_NONCONTIG] &&
			    k2 == TRUE)
			    l2 = TRUE;
			for (l = 0; l < MAX_IO_METHOD; l++)
			{
			    if (test_params_p->io_method[l] == TRUE)
			    {
				if (l == test_params_p->resume_arr[
					VAR_IO_METHOD] && l2 == TRUE) 
				    m2 = TRUE;
				for (m = 0; m < params_count[j]; m++)
				{
				    if (m == test_params_p->resume_arr[
					    VAR_PARAM_IDX] && m2 == TRUE)
					break;
				    
				    if (test_params_p->gen_files == GEN_FULL)
					ret = precreate_file(
					    j, k, l, (params_arr_p[j])[m],
					    test_params_p);
				    else if (test_params_p->gen_files == 
					     GEN_PARTIAL)
					ret = run_test(i, j, k, l, 
						       (params_arr_p[j])[m],
						       GENERATE_MODE,
						       test_params_p);
				    
				    if (ret != 0)
					return -1;
				}
			    }
			    if (l2 == TRUE)
				break;
			}
		    }
		    if (k2 == TRUE)
			break;
		}
	    }
	    if (j == test_params_p->resume_arr[VAR_TEST])
		break;
	}
	
	if (test_params_p->myid == 0)
	    debug_stdout_stream(
                test_params_p->output_info,
		"** End of prior to resume point file (%s) generation...\n\n",
		gen_files_name[test_params_p->gen_files]);
    } /* Case 3: See above */
    else if ((test_params_p->gen_files          == GEN_FULL &&
	      test_params_p->enable_resume      == TRUE &&
	      test_params_p->resume_arr[VAR_RW] == WRITE &&
	      test_params_p->rw[WRITE]          == TRUE &&
	      test_params_p->rw[READ]           == FALSE) ||
	     ((test_params_p->gen_files          == GEN_FULL ||
	       test_params_p->gen_files          == GEN_PARTIAL) &&
	      (test_params_p->enable_resume      == TRUE &&
	       test_params_p->resume_arr[VAR_RW] == READ &&
	       test_params_p->rw[READ]           == TRUE)))
    {
	if (test_params_p->myid == 0)
            debug_stdout_stream(
                test_params_p->output_info,
		"** Generating files (%s) after the resume point...\n", 
		gen_files_name[test_params_p->gen_files]);
	
	i = WRITE;
	j = test_params_p->resume_arr[VAR_TEST];
	for (; j < MAX_TEST; j++)
	{
	    if (test_params_p->test[j] == TRUE)
	    {
		if (j == test_params_p->resume_arr[VAR_TEST])
		    k = test_params_p->resume_arr[VAR_NONCONTIG];
		else
		    k = 0;
		for (; k < MAX_NONCONTIG; k++)
		{
		    if (test_params_p->noncontig[k] == TRUE)
		    {
			if (k == test_params_p->resume_arr[VAR_NONCONTIG])
			    l = test_params_p->resume_arr[VAR_IO_METHOD];
			else
			    l = 0;
			for (; l < MAX_IO_METHOD; l++)
			{
			    if (test_params_p->io_method[l] == TRUE)
			    {
				if (l == test_params_p->resume_arr[
					VAR_IO_METHOD])
				    m = test_params_p->resume_arr[
					VAR_PARAM_IDX];
				else
				    m = 0;
				for (; m < params_count[j]; m++)
				{
				    if (test_params_p->gen_files == GEN_FULL)
					ret = precreate_file(
					    j, k, l, (params_arr_p[j])[m],
					    test_params_p);
				    else if (test_params_p->gen_files == 
					     GEN_PARTIAL)
					ret = run_test(i, j, k, l, 
						       (params_arr_p[j])[m],
						       GENERATE_MODE,
						       test_params_p);
				    
				    if (ret != 0)
					return -1;
				}
			    }
			}
		    }
		}
	    }
	}
	
	if (test_params_p->myid == 0)
	    debug_stdout_stream(
                test_params_p->output_info,
		"** End of post resume point file (%s) generation...\n\n",
		gen_files_name[test_params_p->gen_files]);
    }

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
