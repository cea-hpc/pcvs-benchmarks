/* (C) Northwestern University
 * See COPYING in the top-level directory . */

#include "test.h"
#include "space_requirement.h"

static int calc_size_spacing(int test_type,
			     int noncontig_type,
			     int param_val,
			     struct test_params_s *test_params_p,
			     int64_t *tmp_file_size_p,
			     int64_t *tmp_file_space_p,
			     int64_t *tmp_buf_space_p)
{
    MPI_Datatype base_dtype;
    int base_dtype_sz = -1;
    MPI_Aint base_dtype_lb = -1, base_dtype_ext = -1;
    int region_count = -1, region_size = -1, region_spacing = -1;
    MPI_Offset fileview_disp = -1;

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
    
    /* File size and space */
    *tmp_file_size_p =  (int64_t) test_params_p->numprocs *
	region_count * region_size * base_dtype_sz;

    if (noncontig_type == C_C || noncontig_type == NC_C)
    {
	*tmp_file_space_p = fileview_disp + 
	    ((int64_t) test_params_p->numprocs *
	     region_count * region_size * base_dtype_sz);
    }
    else
    {
	*tmp_file_space_p = fileview_disp + base_dtype_lb +
	    (test_params_p->numprocs *
	     (region_count * (((int64_t) region_size * base_dtype_ext) +
			      region_spacing))) - region_spacing;
    }
    
    /* Max buffer space */
    if (noncontig_type == C_C || noncontig_type == C_NC)
    {
	*tmp_buf_space_p = 
	    (int64_t) region_count * region_size * base_dtype_sz;
    }
    else
    {
	 *tmp_buf_space_p =  base_dtype_lb +
	     (region_count * (((int64_t) region_size * base_dtype_ext) +
			      region_spacing)) - region_spacing;
    }

    if (base_dtype != MPI_CHAR)
	MPI_Type_free(&base_dtype);
    
    return 0;
}

int estimate_space(struct test_params_s *test_params_p)
{
    int j, k, l, m, param_val;
    int64_t total_file_space = 0, test_file_space = 0, tmp_file_space = 0;
    int64_t total_file_size  = 0, test_file_size = 0, tmp_file_size  = 0;
    int64_t total_max_buf_space = -1, test_max_buf_space = -1, 
	tmp_buf_space = -1;

    debug_stdout("estimate_space: These are *rough estimates* due "
		 "to metadata and other factors (i.e. block size).\n\n");

    /* Run tests */
    j = test_params_p->resume_arr[VAR_TEST];
    for (; j < MAX_TEST; j++)
    {
	if (test_params_p->test[j] == TRUE)
	{
	    test_file_size = 0;
	    test_file_space = 0;
	    test_max_buf_space = -1;
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
				if (j == CHECK_SINGLE)
				    param_val =
					test_params_p->
					def_check_test_param_val;
				else
				    param_val = (params_arr_p[j])[m];
					
				calc_size_spacing(j,
						  k,
						  param_val,
						  test_params_p,
						  &tmp_file_size,
						  &tmp_file_space,
						  &tmp_buf_space);

				if (tmp_buf_space > test_max_buf_space)
				    test_max_buf_space = tmp_buf_space;
				if (tmp_buf_space > total_max_buf_space)
				    total_max_buf_space = tmp_buf_space;

				test_file_size += tmp_file_size;
				test_file_space += tmp_file_space;
				total_file_size += tmp_file_size;
				total_file_space += tmp_file_space;
			    }
			}
		    }
		}
	    }
	    debug_stdout(
		"Test: %s\n"
		"total file(s) size                = "
		"%Ld Bytes (%.4f MBytes)\n"
		"total file(s) space               = "
		"%Ld Bytes (%.4f MBytes)\n"
		"maximum process buffer space used = "
		"%Ld Bytes (%.4f MBytes)\n\n",
		test_type_name[j], 
		test_file_size, test_file_size / 1024.0 / 1024.0,
		test_file_space, test_file_space / 1024.0 / 1024.0,
		test_max_buf_space, test_max_buf_space / 1024.0 / 1024.0);
	}
    }
        
    debug_stdout(
	"Summary:\n"
	"total file(s) size                = "
	 "%Ld Bytes (%.4f MBytes)\n"
	"total file(s) space               = "
	"%Ld Bytes (%.4f MBytes)\n"
	"maximum process buffer space used = "
	"%Ld Bytes (%.4f MBytes)\n",
	total_file_size, total_file_size / 1024.0 / 1024.0,
	total_file_space, total_file_space / 1024.0 / 1024.0,
	total_max_buf_space, total_max_buf_space / 1024.0 / 1024.0);

    if (test_params_p->enable_cache)
    {
        int64_t cache_size_bytes = ((int64_t) test_params_p->cache_size)
            * 1024 * 1024;
        debug_stdout(
	    "cache flush amount                = %Ld Bytes (%.4f MBytes)\n",
		     cache_size_bytes, (double) test_params_p->cache_size);
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
