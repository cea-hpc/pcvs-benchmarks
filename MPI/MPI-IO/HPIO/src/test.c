/* (C) Northwestern University
 * See COPYING in the top-level directory . */

#include "files.h"
#include "test.h"
#include "test_human/test_human.h"
#include "test_defined/test_defined.h"
#include "buf.h"
#include "tmpi/tmpi.h"

int generate_filename(char *filename, int max_filename_sz, int pattern_dtype,
		      int test_type, int noncontig_type, int io_method_type,
		      int region_count, int region_size, int region_spacing,
		      int param_val, int rep, 
		      struct test_params_s *test_params_p)
{
    int tmp_buf_sz = 0, tmp_buf_used = 0, tmp_buf_remain = 0;

    memset(filename, 0, max_filename_sz);
    /* Add the directory name if there is one */
    if (test_params_p->dir != NULL)
    {
	tmp_buf_remain = max_filename_sz - tmp_buf_sz;
	tmp_buf_used = snprintf(&filename[tmp_buf_sz], tmp_buf_remain,
				"%s/", test_params_p->dir);
	if (tmp_buf_used < 0 || tmp_buf_used >= tmp_buf_remain)
	{
	    debug_stderr_stream(
		test_params_p->output_info,
		"** Error: Ran out of memory in creating "
		"filename\n");
	    return -1;
	}
	else
	    tmp_buf_sz += tmp_buf_used;
    }
    /* Add the filename prefix, pattern dtype, test type,
     * contig method, io method */
    tmp_buf_remain = max_filename_sz - tmp_buf_sz;
    tmp_buf_used = snprintf(&filename[tmp_buf_sz], tmp_buf_remain,
			    "%s.%s.%s.%s.%s.", 
			    test_params_p->filename_prefix,
			    pattern_dtype_name[pattern_dtype],
			    test_type_name[test_type],
			    noncontig_type_name[noncontig_type],
			    io_method_name[io_method_type]);
    if (tmp_buf_used < 0 || tmp_buf_used >= tmp_buf_remain)
    {
	debug_stderr_stream(
	    test_params_p->output_info,
	    "** Error: Ran out of memory in creating filename\n");
	return -1;
    }
    else
	tmp_buf_sz += tmp_buf_used;
    /* Add the necessary parameter information */
    if (test_type == BAND_SINGLE)
    {
	tmp_buf_remain = max_filename_sz - tmp_buf_sz;
	tmp_buf_used = snprintf(&filename[tmp_buf_sz], tmp_buf_remain,
				"ct.%d.sz.%d.sp.%d", region_count,
				region_size,
				region_spacing);
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
    else if (test_type == CHECK_SINGLE)
    {
	tmp_buf_remain = max_filename_sz - tmp_buf_sz;
	tmp_buf_used = snprintf(&filename[tmp_buf_sz], tmp_buf_remain,
				"ct.%d.sz.%d.sp.%d.%s", region_count,
				region_size,
				region_spacing,
				test_params_p->def_check_test);
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
    else if (test_type == HUMAN)
    {
	tmp_buf_remain = max_filename_sz - tmp_buf_sz;
	tmp_buf_used = snprintf(&filename[tmp_buf_sz], tmp_buf_remain,
				"%s", human_name[param_val]);
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
    else if (test_type == DEFINED)
    {
	tmp_buf_remain = max_filename_sz - tmp_buf_sz;
	tmp_buf_used = snprintf(&filename[tmp_buf_sz], tmp_buf_remain,
				"%s", defined_name[param_val]);
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
    else if (test_type == REGION_COUNT || 
	     test_type == REGION_SIZE || 
	     test_type == REGION_SPACING)
    {
	tmp_buf_remain = max_filename_sz - tmp_buf_sz;
	tmp_buf_used = snprintf(&filename[tmp_buf_sz], tmp_buf_remain,
				"%d", param_val);
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
    else
    {
	debug_stderr_stream(
	    test_params_p->output_info,
	    "** Error: Test type %d invalid in creating filename\n",
	    test_type);
    }
	     
    /* Add rep information */ 
    if (test_params_p->same_file != TRUE)
    {
	tmp_buf_remain = max_filename_sz - tmp_buf_sz;
	tmp_buf_used = snprintf(&filename[tmp_buf_sz], tmp_buf_remain,
				".r%d", rep);
	if (tmp_buf_used < 0 || tmp_buf_used >= tmp_buf_remain)
	{
            debug_stderr_stream(
                test_params_p->output_info,
                "** Error: Ran out of memory in creating filename\n");
	}
	else
	    tmp_buf_sz += tmp_buf_used;
	
    }
    
    mask_stdout(MASK_FILENAMES, 
		"Used %d characters of %d characters available for "
		"the filename %s\n", tmp_buf_sz, max_filename_sz, 
		filename);
    
    return 0;
}

#if 0
int set_hint_info(int hpio_hint, 
		  struct test_params_s *test_params_p, 
		  MPI_Info *info_p)
{
    char pfr_size_str[HINT_STR_MAX];
    char cb_node_str[HINT_STR_MAX];
    	    
    switch(hpio_hint)
    {
	case POSIX:
	    MPI_Info_set(*(info_p), "romio_pvfs2_io_method_write", "posix");
	    MPI_Info_set(*(info_p), "romio_cb_write", "disable");
	    MPI_Info_set(*(info_p), "romio_ds_write", "disable");
	    MPI_Info_set(*(info_p), "romio_pvfs2_io_method_read", "posix");
	    MPI_Info_set(*(info_p), "romio_cb_read", "disable");
	    MPI_Info_set(*(info_p), "romio_ds_read", "disable");
	    break;
	case LIST:
	    MPI_Info_set(*(info_p), "romio_pvfs2_io_method_write", "list");
	    MPI_Info_set(*(info_p), "romio_cb_write", "disable");
	    MPI_Info_set(*(info_p), "romio_ds_write", "disable");
	    MPI_Info_set(*(info_p), "romio_pvfs2_io_method_read", "list");
	    MPI_Info_set(*(info_p), "romio_cb_read", "disable");
	    MPI_Info_set(*(info_p), "romio_ds_read", "disable");
	    break;
	case DATASIEVE: 
	    /* Not supported in some filesystems */
	    MPI_Info_set(*(info_p), "romio_pvfs2_io_method_write", 
			 "datasieve");
	    MPI_Info_set(*(info_p), "romio_cb_write", "disable");
	    MPI_Info_set(*(info_p), "romio_ds_write", "enable");
	    MPI_Info_set(*(info_p), "romio_pvfs2_io_method_read", 
			 "datasieve");
	    MPI_Info_set(*(info_p), "romio_cb_read", "disable");
	    MPI_Info_set(*(info_p), "romio_ds_read", "enable");
	    break;
	case TWO_PHASE:
	    /* Setup char strings for MPI-IO hints */
	    assert(test_params_p->agg_count < HINT_NUM_MAX);
	    memset(cb_node_str, 0, HINT_STR_MAX);
	    sprintf(cb_node_str, "%d", test_params_p->agg_count);
	    
	    MPI_Info_set(*(info_p), "romio_cb_write", "enable");
	    MPI_Info_set(*(info_p), "romio_cb_read", "enable");
	    if (test_params_p->agg_method == 1)
		MPI_Info_set(*(info_p), "cb_config_list", "*:*");
	    if (test_params_p->agg_count != 0)
		MPI_Info_set(*(info_p), "cb_nodes", cb_node_str);
	    break;
	case TWO_PHASE_PFR:
	    /* Setup char strings for MPI-IO hints */
	    assert(test_params_p->agg_count < HINT_NUM_MAX);
	    memset(cb_node_str, 0, HINT_STR_MAX);
	    sprintf(cb_node_str, "%d", test_params_p->agg_count);
	    assert(test_params_p->pfr_size < HINT_NUM_MAX);
	    memset(pfr_size_str, 0, HINT_STR_MAX);
	    sprintf(pfr_size_str, "%d", test_params_p->pfr_size);
	    
	    MPI_Info_set(*(info_p), "romio_cb_write", "enable");
	    MPI_Info_set(*(info_p), "romio_cb_read", "enable");
	    MPI_Info_set(*(info_p), "persistent_filedomains", "enable");
	    MPI_Info_set(*(info_p), "pfd_stripe_size", pfr_size_str);
	    if (test_params_p->agg_method == 1)
		MPI_Info_set(*(info_p), "cb_config_list", "*:*");
	    if (test_params_p->agg_count != 0)
		MPI_Info_set(*(info_p), "cb_nodes", cb_node_str);
	    break;
	case DATATYPE:
	    MPI_Info_set(*(info_p), "romio_pvfs2_io_method_write", "dtype");
	    MPI_Info_set(*(info_p), "romio_cb_write", "disable");
	    MPI_Info_set(*(info_p), "romio_ds_write", "disable");
	    MPI_Info_set(*(info_p), "romio_pvfs2_io_method_read", "dtype");
	    MPI_Info_set(*(info_p), "romio_cb_read", "disable");
	    MPI_Info_set(*(info_p), "romio_ds_read", "disable");
	    break;
	default:
	    debug_stderr_stream(
		test_params_p->output_info,
		"set_hint_info: I/O method %d failed\n", hpio_hint);
	    break;
    }

    return 0;
}
#endif

int create_test_parameters(int test_type, 
			   int param_val,
			   struct test_params_s *test_params_p,
			   int *region_count_p,
			   int *region_size_p,
			   int *region_spacing_p,
			   MPI_Datatype *base_dtype_p,
			   MPI_Offset *fileview_disp_p)
{
    *fileview_disp_p = 0;

    switch(test_type)
    {
	case REGION_COUNT:
	    *region_count_p = param_val;
	    *region_size_p = test_params_p->def_region_params[REGION_SIZE];
	    *region_spacing_p = 
		test_params_p->def_region_params[REGION_SPACING];
	    *base_dtype_p = MPI_CHAR;
	    break;
	case REGION_SIZE:
	    *region_count_p = test_params_p->def_region_params[REGION_COUNT];
	    *region_size_p = param_val;
	    *region_spacing_p = 
		test_params_p->def_region_params[REGION_SPACING];
	    *base_dtype_p = MPI_CHAR;
	    break;
	case REGION_SPACING:
	    *region_count_p = test_params_p->def_region_params[REGION_COUNT];
	    *region_size_p = test_params_p->def_region_params[REGION_SIZE];
	    *region_spacing_p = param_val;
	    *base_dtype_p = MPI_CHAR;
	    break;
	case BAND_SINGLE:
	    *region_count_p = test_params_p->def_region_params[REGION_COUNT];
	    *region_size_p = test_params_p->def_region_params[REGION_SIZE];
	    *region_spacing_p = 
		test_params_p->def_region_params[REGION_SPACING];
	    *base_dtype_p = MPI_CHAR;
	    break;
	case HUMAN:
	    *region_count_p = test_params_p->def_region_params[REGION_COUNT];
            *region_size_p = test_params_p->def_region_params[REGION_SIZE];
	    *region_spacing_p =
		test_params_p->def_region_params[REGION_SPACING];
	    init_human_func_arr[param_val](base_dtype_p,
					   fileview_disp_p);
	    break;
	case DEFINED:
	    *region_count_p = test_params_p->def_region_params[REGION_COUNT];
            *region_size_p = test_params_p->def_region_params[REGION_SIZE];
	    *region_spacing_p =
		test_params_p->def_region_params[REGION_SPACING];
	    init_defined_func_arr[param_val](base_dtype_p,
					     fileview_disp_p);
	    break;
	case CHECK_SINGLE:
	    *region_count_p = test_params_p->def_region_params[REGION_COUNT];
	    *region_size_p = test_params_p->def_region_params[REGION_SIZE];
	    *region_spacing_p = 
		test_params_p->def_region_params[REGION_SPACING];
	    if (test_params_p->def_check_test_num == HUMAN)
		init_human_func_arr[test_params_p->def_check_test_param_val]
		    (base_dtype_p,
		     fileview_disp_p);
	    else if (test_params_p->def_check_test_num == DEFINED)
		init_defined_func_arr[test_params_p->def_check_test_param_val]
		    (base_dtype_p,
		     fileview_disp_p);
	    else
		debug_stderr_stream(
		    test_params_p->output_info,
		    "create_test_params: Check single mode has invalid "
		    "check test\n");
	    break;
	default:
	    debug_stderr_stream(
		test_params_p->output_info,
		"create_test_params: Impossible test_type %d\n", test_type);
	    return -1;
    }

    return 0;
}

#define STRUCT_CT 2
int create_access_pattern(int pattern_dtype,
			  int myid,
			  int numprocs,
			  int noncontig_type,
			  int region_count,
			  int region_size,
			  int region_spacing,
			  MPI_Datatype *base_dtype_p,
			  int *memtype_count_p,
			  MPI_Datatype *memtype_p,
			  MPI_Datatype *filetype_p,
			  MPI_Offset *fileview_disp_p)
{
    int len_arr[STRUCT_CT], base_dtype_sz = -1;
    MPI_Aint base_dtype_ext = -1, disp_arr[STRUCT_CT];
    MPI_Datatype dtype_arr[STRUCT_CT];

    MPI_Type_size(*base_dtype_p, &base_dtype_sz);    
    MPI_Type_extent(*base_dtype_p, &base_dtype_ext);

    len_arr[0] = region_size;
    len_arr[1] = 1;
    disp_arr[0] = 0;
    dtype_arr[0] = *base_dtype_p;
    dtype_arr[1] = MPI_UB;

    /* Set the appropriate memory type */
    switch(noncontig_type)
    {
	case C_C:
	case C_NC:
	    switch(pattern_dtype)
	    {
		case VECTOR:
		    MPI_Type_contiguous(region_count * region_size * 
					base_dtype_sz, MPI_CHAR,
					memtype_p);
		    break;
		case STRUCT:
		    MPI_Type_contiguous(region_size * base_dtype_sz, 
					MPI_CHAR, memtype_p);
		    break;
		default:
		    debug_stderr(
			"create_access_pattern: Invalid pattern "
			"dtype %d\n", pattern_dtype);
	    }
	    break;
	case NC_C:
	case NC_NC:
	    switch(pattern_dtype)
	    {
		case VECTOR:
		    MPI_Type_hvector(region_count, region_size, 
				     (region_size * base_dtype_ext) + 
				     region_spacing,
				     *base_dtype_p, memtype_p);
		    break;
		case STRUCT:
		    disp_arr[1] =
			(region_size * base_dtype_ext) + 
			region_spacing;
		    MPI_Type_struct(STRUCT_CT, 
				    len_arr, disp_arr, dtype_arr,
				    memtype_p);
		    break;
		default:
		    debug_stderr(
			"create_access_pattern: Invalid pattern "
			"dtype %d\n", pattern_dtype);
	    }
	    break;
	default:
	    debug_stderr(
		"create_access_pattern: Invalid "
		"noncontig_type %d\n", noncontig_type);
    }
    /* Set the appropriate file type */
    switch(noncontig_type)
    {
	case C_C:
	case NC_C:
	    switch(pattern_dtype)
	    {
		case VECTOR:
		    MPI_Type_contiguous(region_count * region_size *
					base_dtype_sz, MPI_CHAR,
					filetype_p);
		    break;
		case STRUCT:
		    MPI_Type_contiguous(region_size * base_dtype_sz,
					MPI_CHAR, filetype_p);
		    break;
		default:
		    debug_stderr(
			"create_access_pattern: Invalid pattern "
			"dtype %d\n", pattern_dtype);
	    }
	    break;
	case C_NC:
	case NC_NC:
	    switch(pattern_dtype)
	    {
		case VECTOR:
		    MPI_Type_hvector(region_count, region_size, 
				     ((region_size * base_dtype_ext) + 
				      region_spacing) * numprocs,
				     *base_dtype_p, filetype_p);
		    break;
		case STRUCT:
		    disp_arr[1] =   ((region_size * base_dtype_ext) + 
				     region_spacing) * numprocs;
		    MPI_Type_struct(STRUCT_CT,
                                    len_arr, disp_arr, dtype_arr,
                                    filetype_p);
		    break;
		default:
		    debug_stderr(
			"create_access_pattern: Invalid pattern "
			"dtype %d\n", pattern_dtype);
    }
	    break;
	default:
	    debug_stderr(
		"create_access_pattern: Invalid "
		"noncontig_type %d\n", noncontig_type);
    }    
    
    MPI_Type_commit(memtype_p);
    MPI_Type_commit(filetype_p);

    if (noncontig_type == C_C || noncontig_type == NC_C)
    {
	*fileview_disp_p += (MPI_Offset) region_count * 
	    region_size * base_dtype_sz * myid;
    }
    else
    {
	*fileview_disp_p += (((MPI_Offset) region_size * base_dtype_ext) + 
			     region_spacing) * myid;
    }

    /* If the pattern dtype is using struct, the count should
     * be region count (the access pattern was split) */
    if (pattern_dtype == STRUCT)
	*memtype_count_p = region_count;
    else
	*memtype_count_p = 1;
    
    return 0;
}

void print_time_arr(double *time_arr)
{
    int i;

    debug_stdout("printing time_arr...\n");
    for (i = 0; i < MAX_TIME; i++)
	debug_stdout("%7.3f |", time_arr[i]);
    debug_stdout("\n\n");
}

void print_time_matrix(double **test_time_matrix, int reps)
{
    int i, j;

    debug_stdout("printing time_matrix...\n");
    for (i = 0; i < reps; i++)
	for (j = 0; j < MAX_TIME; j++)
	{
	    debug_stdout("%7.3f |", test_time_matrix[i][j]);
	    if (j == MAX_TIME - 1)
		debug_stdout("\n");
	}
    
    debug_stdout("\n");
}

/* Take the matrix of test times and use an average method to fill in
 * final_time_arr */
int test_average(int reps_completed,
		 struct test_params_s *test_params_p,
		 double **test_time_matrix,
		 double *final_time_arr)
{
    int i, j;
    int tmp_max_index = 0, tmp_min_index = 0;
    int average_method = -1;

    assert(reps_completed <= test_params_p->reps);
    assert(test_params_p->average_method >= 0 && 
	   test_params_p->average_method < MAX_AVE);

    /* If not all reps finished, change the average methods in some
     * cases. */
    if (reps_completed < test_params_p->reps && 
	reps_completed < 3 &&
	test_params_p->average_method == AVE_NOMIN_NOMAX)
    {
	debug_stdout_stream(
	    test_params_p->output_info, 
	    "test_average: Completed only %d of %d attempted "
	    "reps and switching from average method %s to %s.\n",
	    reps_completed, test_params_p->reps,
	    average_name[AVE_NOMIN_NOMAX], average_name[AVE_NORMAL]);
		
	average_method = AVE_NORMAL;
    }
    else
	average_method = test_params_p->average_method;

    /* Eliminate some timings */
    switch (average_method)
    {
	case AVE_NORMAL:
	    break;
	case AVE_NOMIN_NOMAX:
	    /* Get rid of the aggregate high and low times, not the
	     * local ones */
	    assert(reps_completed > 2);
	    tmp_max_index = 0;
	    tmp_min_index = 0;
	    for (i = 0; i < reps_completed; i++)
	    {
		if (test_time_matrix[i][TIME_AGG_MAX_IOSYNC] > 
		    test_time_matrix[tmp_max_index][TIME_AGG_MAX_IOSYNC])
		    tmp_max_index = i;
		if (test_time_matrix[i][TIME_AGG_MAX_IOSYNC] < 
		    test_time_matrix[tmp_min_index][TIME_AGG_MAX_IOSYNC])
		    tmp_min_index = i;
	    }	    
	    break;
	case AVE_BEST:
	    break;
	default:
	    debug_stderr_stream(test_params_p->output_info,
				"test_average: Invalid average method %d\n",
				average_method);
	    return -1;
    }
    
    /* Do averages and store in the final_time_arr */
    switch (average_method)
    {
	case AVE_NORMAL:
	    for (i = 0; i < reps_completed; i++)
	    {
		for (j = 0; j < MAX_TIME; j++)
		    final_time_arr[j] += test_time_matrix[i][j];
	    }
	    for (i = 0; i < MAX_TIME; i++)
		final_time_arr[i] /= reps_completed;

	    break;
	case AVE_NOMIN_NOMAX:
	    for (i = 0; i < reps_completed; i++)
	    {
		if (i == tmp_max_index || i == tmp_min_index)
		    continue;
		
		for (j = 0; j < MAX_TIME; j++)
                    final_time_arr[j] += test_time_matrix[i][j];
		
	    } 
	    for (i = 0; i < MAX_TIME; i++)
		final_time_arr[i] /= (reps_completed - 2);
	    break;
	case AVE_BEST:
	    tmp_min_index = 0;
	    for (i = 0; i < reps_completed; i++)
	    {
                if (test_time_matrix[i][TIME_AGG_MAX_IOSYNC] <
                    test_time_matrix[tmp_min_index][TIME_AGG_MAX_IOSYNC])
                    tmp_min_index = i;
	    }
	    memcpy(final_time_arr, test_time_matrix[tmp_min_index],
		   MAX_TIME * sizeof(double));
	    break;
	default:
	    debug_stderr_stream(
		test_params_p->output_info,
		"test_average: Invalid average method %d\n",
		average_method);
	    return -1;
    }

    return 0;
}

/* check_data_buf - The data from the buffer has been packed into a
 * contiguous buffer and will be verified for correctness in this
 * function. */
static int check_data_buf(int noncontig_type, 
			  int region_size, 
			  int region_count, 
			  int region_spacing,
			  char *data_buf,
			  int64_t data_buf_sz,
			  MPI_Datatype *base_dtype_p,
			  struct test_params_s *test_params_p)
{
    int base_dtype_sz = -1, test_err = 0;
    int64_t i, j, global_test_dtype_blocknum = -1;
    int64_t bytes_checked = 0, agg_bytes_checked = -1, idx_calc = -1;

    MPI_Type_size(*base_dtype_p, &base_dtype_sz);

    if (noncontig_type == C_C || noncontig_type == NC_C)
	global_test_dtype_blocknum = test_params_p->myid * region_count;
    else
	global_test_dtype_blocknum = test_params_p->myid;

    for (i = 0; i < region_count; i++)
    {
	for (j = 0; j < region_size * base_dtype_sz; j++)
	{
	    idx_calc = ((int64_t) i * region_size * base_dtype_sz) + j;
	    assert(idx_calc < data_buf_sz);
	    if (data_buf[idx_calc] != 
		gen_file_char(global_test_dtype_blocknum + j))
	    {
		debug_stdout_stream(
		    test_params_p->output_info,
		    "Error: Byte number %Ld written by process %d "
		    "= %c (int %d), should = %c\n",
		    idx_calc, test_params_p->myid,
		    data_buf[idx_calc], (int) data_buf[idx_calc], 
		    gen_file_char(global_test_dtype_blocknum + j));
		test_err = 1;
	    }
	    else
	    {
		mask_stdout(
		    MASK_CORRECT,
		    "Correct: Byte number %Ld written by process %d "
		    "= %c (int %d), should = %c\n",
		    idx_calc, test_params_p->myid,
		    data_buf[idx_calc], (int) data_buf[idx_calc],
		    gen_file_char(global_test_dtype_blocknum + j));
	    }
	    bytes_checked++;
	}
	if (noncontig_type == C_C || noncontig_type == NC_C)
	    global_test_dtype_blocknum++;
	else
	    global_test_dtype_blocknum += test_params_p->numprocs;
    }

    MPI_Reduce(&bytes_checked, &agg_bytes_checked, 1, MPI_LONG_LONG,
	       MPI_SUM, 0, MPI_COMM_WORLD);
    if (test_params_p->myid == 0)
    {
	if (agg_bytes_checked != (int64_t) test_params_p->numprocs * 
	    region_count * region_size * base_dtype_sz)
	{
	    debug_stdout_stream(
		test_params_p->output_info,
		"Error: %Ld of %Ld bytes (data-only) are correct\n",
		agg_bytes_checked, (int64_t) 
		test_params_p->numprocs * 
		region_count * region_size * base_dtype_sz);
	    test_err = 1;
	}
	else
	{
	    mask_stdout(MASK_CORRECT_INFO,
			"Correct: %Ld of %Ld bytes (data-only) are correct\n",
			agg_bytes_checked, (int64_t) 
			test_params_p->numprocs *
			region_count * region_size * base_dtype_sz);
	}
    }
    return test_err;
}

static int check_manual_buf(int rw_type,
			    int noncontig_type,
			    int io_method_type,
			    int region_size,
			    int region_count,
			    int region_spacing,
			    int param_val,
			    char *filename,
			    char *data_buf,
			    int64_t data_buf_sz,
			    char *io_buf,
			    int64_t io_buf_sz,
			    MPI_Datatype *base_dtype_p,
			    MPI_Offset general_disp,
			    struct test_params_s *test_params_p)
{
    char check_char = 0;
    int *check_buf = NULL, ret = -1;
    int64_t check_buf_sz = -1, check_buf_data_sz = -1;
    int64_t check_buf_idx = 0, check_buf_repeat = 0;
    int64_t initial_skip = -1, block_off = 0, block_end = -1;
    int64_t data_buf_off = -1;

    MPI_Aint base_dtype_ext = -1, base_dtype_lb = -1;
    int base_dtype_sz = -1, test_err = 0;
    int64_t i, global_test_dtype_blocknum = -1;
    int64_t bytes_checked = 0, agg_bytes_checked = -1;
    
    MPI_Type_lb(*base_dtype_p, &base_dtype_lb);
    MPI_Type_size(*base_dtype_p, &base_dtype_sz);
    MPI_Type_extent(*base_dtype_p, &base_dtype_ext);

    alloc_buf_func_arr[param_val](rw_type,
				  noncontig_type,
				  &check_buf,
				  &check_buf_sz,
				  &check_buf_data_sz,
				  &initial_skip);

    /* The global blocks are laid out differently if writes are
     * being tested or if reads are being tested. */
    if (rw_type == WRITE)
	global_test_dtype_blocknum = test_params_p->myid * 
	    region_count;
    else
    {
	if (noncontig_type == C_C || noncontig_type == NC_C)
	    global_test_dtype_blocknum = 
		test_params_p->myid * region_count;
	else
	    global_test_dtype_blocknum = test_params_p->myid;
    }

    if (rw_type == WRITE)
    {
	if (noncontig_type == C_C || noncontig_type == NC_C)
	    block_end = region_size * base_dtype_sz;
	else
	    block_end = (region_size * base_dtype_ext) + 
		region_spacing;
    }
    else
    {
	if (noncontig_type == C_C || noncontig_type == C_NC)
	    block_end = region_size * base_dtype_sz;
	else
	    block_end = (region_size * base_dtype_ext) + 
		region_spacing;
    }

    /* Handle the fileview disp (if this is a write and process 0). */
    if (test_params_p->myid == 0 && rw_type == WRITE)
    {
	MPI_File fh_disp;
	char *disp_buf = NULL;
	if ((disp_buf = (char *) malloc(general_disp * sizeof(char))) 
	    == NULL)
	{
	    debug_stderr_stream(
		test_params_p->output_info,
		"check_data: malloc disp_buf of size %Ld "
		"failed", general_disp * sizeof(char));
	    return -1;
	}
	memset(disp_buf, DEFAULT_EMPTY_CHAR, 
	       general_disp * sizeof(char));
		
	ret = tMPI_File_open(MPI_COMM_SELF, filename, MPI_MODE_RDONLY,
			     MPI_INFO_NULL, &fh_disp);
	if (ret != MPI_SUCCESS)
	    return ret;

	ret = tMPI_File_read(fh_disp, disp_buf, 
			     general_disp, 
			     MPI_CHAR, MPI_STATUS_IGNORE);
	if (ret != MPI_SUCCESS)
	    return ret;
		
	ret = tMPI_File_close(&fh_disp);
	if (ret != MPI_SUCCESS)
	    return ret;

	for (i = 0; i < general_disp; i++)
	{
	    if (disp_buf[i] == (char) DEFAULT_DISP_CHAR ||
		disp_buf[i] == (char) DEFAULT_EMPTY_CHAR)
	    {
		mask_stdout(
		    MASK_CORRECT,
		    "Correct: (disp) Offset %Ld on process %d "
		    "= %c (int %d), defined as (int %d or %d)\n",
		    i, test_params_p->myid,
		    disp_buf[i], (int) disp_buf[i], DEFAULT_DISP_CHAR,
		    DEFAULT_EMPTY_CHAR);
		bytes_checked++;
	    }
	    else
	    {
		debug_stdout_stream(
                    test_params_p->output_info,
		    "Error: (disp) Offset %Ld on process %d "
		    "= %c (int %d), defined as (int %d or %d)\n",
		    i, test_params_p->myid,
		    disp_buf[i], (int) disp_buf[i], DEFAULT_DISP_CHAR,
		    DEFAULT_EMPTY_CHAR);
		test_err = 1;
	    }
	}
	free(disp_buf);
    }
	    
    /* Each process may have to skip the lb if it is noncontig in
     * memory and a read or noncontig in file and a write. */
    i = 0;
    if ((rw_type == WRITE &&
	 (noncontig_type == C_NC || noncontig_type == NC_NC)) ||
	(rw_type == READ && 
	 (noncontig_type == NC_C || noncontig_type == NC_NC)))
    {
	for (i = 0; i < base_dtype_lb; i++)
	{
	    if (io_buf[i] == (char) DEFAULT_CHAR ||
		io_buf[i] == (char) DEFAULT_EMPTY_CHAR ||
		(io_buf[i] == (char) DEFAULT_DATASIEVE_CHAR && 
		 (io_method_type == COLLECTIVE)))
	    {
		mask_stdout(
		    MASK_CORRECT,
		    "Correct: (skip) Offset %Ld on process %d "
		    "= %c (int %d), defined as (int %d, %d, or %d)\n",
		    i, test_params_p->myid,
		    io_buf[i], (int) io_buf[i], DEFAULT_CHAR, 
		    DEFAULT_EMPTY_CHAR, DEFAULT_DATASIEVE_CHAR);
		bytes_checked++;
	    }
	    else
	    {
		debug_stdout_stream(
                    test_params_p->output_info,
		    "Error: (skip) Offset %Ld on process %d "
		    "= %c (int %d), defined as %c\n",
		    i, test_params_p->myid,
		    io_buf[i], (int) io_buf[i], DEFAULT_CHAR, 
		    DEFAULT_EMPTY_CHAR, DEFAULT_DATASIEVE_CHAR);
		test_err = 1;
	    }
	}
    }

    block_off = 0;
    check_buf_idx = 0;
    data_buf_off = 0;
    for (; i < io_buf_sz; i++)
    {
	/* The user provided check_buf has a finite size and
	 * when checking other blocks, it is looped over and
	 * incremented to match the particular block number. */
	check_char = 
	    gen_file_char((check_buf_repeat * check_buf_data_sz) +
			  check_buf[check_buf_idx] + 
			  global_test_dtype_blocknum);

	mask_stdout(MASK_HUMAN, "check_data: check_char %c (int %d) "
		    "from:\n  check_buf_idx = %Ld check_buf_repeat "
		    "= %Ld check_buf_data_sz = %Ld\n  "
		    "global_test_dtype_blocknum = %Ld "
		    "gen_file_char input = %Ld\n",
		    check_char, (int) check_char, check_buf_idx, 
		    check_buf_repeat, check_buf_data_sz,
		    global_test_dtype_blocknum, 
		    check_buf[check_buf_idx] +
		    (check_buf_repeat * check_buf_data_sz) +
		    global_test_dtype_blocknum);

	if (((check_buf_idx + 1) % check_buf_sz) == 0) 
	{
	    check_buf_idx = 0;
	    check_buf_repeat++;
	}   
	else
	    check_buf_idx++;

	if (io_buf[i] == (char) DEFAULT_INIT_CHAR)
	{
	    if (data_buf[data_buf_off] != check_char)
	    {
                debug_stdout_stream(
                    test_params_p->output_info,
		    "Error: Offset %Ld on process %d "
		    "= %c (int %d), defined as %c\n",
		    i, test_params_p->myid,
		    data_buf[data_buf_off], 
		    (int) data_buf[data_buf_off], 
		    check_char);
		test_err = 1;
	    }
	    else
	    {
		mask_stdout(
		    MASK_CORRECT,
		    "Correct: Offset %Ld on process %d "
		    "= %c (int %d), defined as %c\n",
		    i, test_params_p->myid,
		    data_buf[data_buf_off], 
		    (int) data_buf[data_buf_off], 
		    check_char);
		bytes_checked++;
	    }
		    
	    data_buf_off++;
		    
	}
	else if (io_buf[i] == (char) DEFAULT_CHAR ||
		 io_buf[i] == (char) DEFAULT_EMPTY_CHAR ||
		 (io_buf[i] == (char) DEFAULT_DATASIEVE_CHAR && 
		  (io_method_type == COLLECTIVE)))
	{
	    mask_stdout(
		MASK_CORRECT,
		"Correct: Offset %Ld on process %d "
		"= %c (int %d), defined as (int %d, %d, or %d)\n",
		i, test_params_p->myid,
		io_buf[i], (int) io_buf[i],
		DEFAULT_CHAR, DEFAULT_EMPTY_CHAR, 
		DEFAULT_DATASIEVE_CHAR);
	    bytes_checked++;
	}
	else
	{
	    debug_stdout_stream(
		test_params_p->output_info,
		"Error: Offset %Ld on process %d "
		"= %c (int %d), defined as (int %d, %d, or %d)\n",
		i, test_params_p->myid,
		io_buf[i], (int) io_buf[i],
		DEFAULT_CHAR, DEFAULT_EMPTY_CHAR, 
		DEFAULT_DATASIEVE_CHAR);
	    test_err = 1;
	}
		
	if (block_off + 1 == block_end)
	{
	    /* Restart the checking of the human buffer */
	    check_buf_idx = 0;
	    check_buf_repeat = 0;
	    block_off = 0;
	    /* The global blocks are laid out differently if
	     * writes are being tested or if reads are being
	     * tested. */
	    if (rw_type == WRITE)
		global_test_dtype_blocknum++;
	    else
	    {
		if (noncontig_type == C_C || noncontig_type == NC_C)
		    global_test_dtype_blocknum++;
		else
		    global_test_dtype_blocknum += 
			test_params_p->numprocs;
	    }
	}
	else
	{
	    block_off++;
	}

    }

    MPI_Reduce(&bytes_checked, &agg_bytes_checked, 1, MPI_LONG_LONG,
	       MPI_SUM, 0, MPI_COMM_WORLD);
    if (test_params_p->myid == 0)
    {
	int64_t agg_total_extent = 0;

	if ((rw_type == READ && (noncontig_type == C_C || 
				 noncontig_type == C_NC)) ||
	    (rw_type == WRITE && (noncontig_type == C_C || 
				  noncontig_type == NC_C)))
	{
	    agg_total_extent = ((int64_t) test_params_p->numprocs * 
				region_size * base_dtype_sz * region_count);

	}
	else
	{
	    if (rw_type == READ)
	    {
		if (test_params_p->pattern_dtype == VECTOR)
		    agg_total_extent = 
			(int64_t) test_params_p->numprocs *
			(base_dtype_lb + 
			 ((((int64_t) region_size * base_dtype_ext) + 
			   region_spacing) * region_count) - region_spacing);
		else
		    agg_total_extent = 
			(int64_t) test_params_p->numprocs *
			(base_dtype_lb + 
			 ((((int64_t) region_size * base_dtype_ext) + 
			   region_spacing) * region_count));
	    }
	    else
	    {
	    /* Subtract the lb_bytes from all but one process */
		agg_bytes_checked -= 
		    base_dtype_lb * (test_params_p->numprocs - 1);

		agg_total_extent = base_dtype_lb + 
		    (((int64_t) test_params_p->numprocs *
		      (((int64_t) region_size * base_dtype_ext) + 
		       region_spacing) * 
		      region_count) - region_spacing);	
	    }

	}
	if (rw_type == WRITE)
	    agg_total_extent += general_disp;

	if (agg_bytes_checked != agg_total_extent)
	{
	    debug_stdout_stream(
		test_params_p->output_info,
		"Error: %Ld of %Ld bytes (manually-checked) are "
		"correct\n", agg_bytes_checked, agg_total_extent);
	    test_err = 1;
	}
	else
	{
	    mask_stdout(MASK_CORRECT_INFO, 
			"Correct: %Ld of %Ld bytes (manually-checked) are "
			"correct\n", agg_bytes_checked, agg_total_extent);
	}
    }
	    
    free(check_buf);

    if (data_buf_off != (int64_t) region_count * region_size * base_dtype_sz)
    {
	debug_stdout_stream(
	    test_params_p->output_info,
	    "check_manual_buf: Error - Accessed %Ld data bytes instead of %Ld "
	    "data bytes\n", data_buf_off, (int64_t) region_count * 
	    region_size * base_dtype_sz);
	test_err = 1;
    }
    return test_err;
}

static int check_generated_buf(int rw_type,
			       int noncontig_type,
			       int io_method_type,
			       int region_size,
			       int region_count,
			       int region_spacing,
			       char *data_buf,
			       int64_t data_buf_sz,
			       char *io_buf,
			       int64_t io_buf_sz,
			       MPI_Datatype *base_dtype_p,
			       MPI_Offset general_disp,
			       struct test_params_s *test_params_p)
{
    int64_t block_off = 0;
    int64_t data_buf_off = -1;

    MPI_Aint base_dtype_ext = -1, base_dtype_lb = -1;
    int base_dtype_sz = -1, test_err = 0;
    int64_t i, global_test_dtype_blocknum = -1;
    int64_t bytes_checked = 0, agg_bytes_checked = -1;

    MPI_Type_lb(*base_dtype_p, &base_dtype_lb);
    MPI_Type_size(*base_dtype_p, &base_dtype_sz);
    MPI_Type_extent(*base_dtype_p, &base_dtype_ext);

    /* The global blocks are laid out differently if writes are
     * being tested or if reads are being tested. */
    if (rw_type == WRITE)
	global_test_dtype_blocknum = test_params_p->myid * 
	    region_count;
    else
    {
	if (noncontig_type == C_C || noncontig_type == NC_C)
	    global_test_dtype_blocknum = 
		test_params_p->myid * region_count;
	else
	    global_test_dtype_blocknum = test_params_p->myid;
    }

    block_off = 0;
    data_buf_off = 0;
    for (i = 0; i < io_buf_sz; i++)
    {
	if (io_buf[i] == (char) DEFAULT_INIT_CHAR)
	{
	    if (data_buf[data_buf_off] !=
		gen_file_char(global_test_dtype_blocknum + block_off))
	    {
                debug_stdout_stream(
                    test_params_p->output_info,
		    "Error: Offset %Ld on process %d "
		    "= %c (int %d), computed as %c\n",
		    i, test_params_p->myid,
		    data_buf[data_buf_off], 
		    (int) data_buf[data_buf_off], 
		    gen_file_char(global_test_dtype_blocknum + 
				  block_off));
		test_err = 1;
	    }
	    else
	    {
		mask_stdout(
		    MASK_CORRECT,
		    "Correct: Offset %Ld on process %d "
		    "= %c (int %d), computed as %c\n",
		    i, test_params_p->myid,
		    data_buf[data_buf_off], 
		    (int) data_buf[data_buf_off],
		    gen_file_char(global_test_dtype_blocknum +
				  block_off));
		bytes_checked++;
	    }
		    
	    data_buf_off++;
	    if (block_off + 1 == region_size * base_dtype_sz)
	    {
		block_off = 0;
		/* The global blocks are laid out differently if
		 * writes are being tested or if reads are being
		 * tested. */
		if (rw_type == WRITE)
		    global_test_dtype_blocknum++;
		else
		{
		    if (noncontig_type == C_C || 
			noncontig_type == NC_C)
			global_test_dtype_blocknum++;
		    else
			global_test_dtype_blocknum += 
			    test_params_p->numprocs;
		}
	    }
	    else
		block_off++;
	}
	else if (io_buf[i] == (char) DEFAULT_CHAR ||
		 io_buf[i] == (char) DEFAULT_EMPTY_CHAR ||
		 (io_buf[i] == (char) DEFAULT_DATASIEVE_CHAR && 
		  (io_method_type == COLLECTIVE)))
	{
	    mask_stdout(
		MASK_CORRECT,
		"Correct: Offset %Ld on process %d "
		"= %c (int %d), computed as (int %d, %d, or %d)\n",
		i, test_params_p->myid,
		io_buf[i], (int) io_buf[i],
		DEFAULT_CHAR, DEFAULT_EMPTY_CHAR, 
		DEFAULT_DATASIEVE_CHAR);
	    bytes_checked++;
	}
	else {
	    debug_stdout_stream(
		test_params_p->output_info,
		"Error: Offset %Ld on process %d "
		"= %c (int %d), computed as (int %d, %d, or %d)\n",
		i, test_params_p->myid,
		io_buf[i], (int) io_buf[i],
		DEFAULT_CHAR, DEFAULT_EMPTY_CHAR, 
		DEFAULT_DATASIEVE_CHAR);
	    test_err = 1;
	}
    }

    MPI_Reduce(&bytes_checked, &agg_bytes_checked, 1, MPI_LONG_LONG,
	       MPI_SUM, 0, MPI_COMM_WORLD);
    if (test_params_p->myid == 0)
    {
	int64_t agg_total_extent = 0;

	if ((rw_type == READ && (noncontig_type == C_C || 
				 noncontig_type == C_NC)) ||
	    (rw_type == WRITE && (noncontig_type == C_C || 
				  noncontig_type == NC_C)))
	{
	    agg_total_extent = ((int64_t) test_params_p->numprocs * 
				region_size * base_dtype_sz * region_count);

	}
	else
	{
	    /* There is a situation for READ.  Since the buf size is
	     * dictated by the actual datatype (STRUCT or VECTOR), we
	     * have to check the appropriate bytes for each case. */
	    if (rw_type == READ)
	    {
		if (test_params_p->pattern_dtype == VECTOR)
		    agg_total_extent = 
			(int64_t) test_params_p->numprocs *
			(base_dtype_lb + 
			 ((((int64_t) region_size * base_dtype_ext) + 
			   region_spacing) * region_count) - region_spacing);
		else
		    agg_total_extent = 
			(int64_t) test_params_p->numprocs *
			(base_dtype_lb + 
			 ((((int64_t) region_size * base_dtype_ext) + 
			   region_spacing) * region_count));
	    }
	    else
	    {
		/* Subtract the lb_bytes from all but one process */
		agg_bytes_checked -= 
		    base_dtype_lb * (test_params_p->numprocs - 1);

		agg_total_extent = base_dtype_lb + 
		    (((int64_t) test_params_p->numprocs *
		      (((int64_t) region_size * base_dtype_ext) + 
		       region_spacing) * 
		      region_count) - region_spacing);
	    }
	}

	if (agg_bytes_checked != agg_total_extent)
	{
	    if (rw_type == READ)
                debug_stdout_stream(
                    test_params_p->output_info,
		    "Error: %Ld of %Ld (generated) bytes are "
		    "correct\n", agg_bytes_checked, agg_total_extent);
	    else
		debug_stdout_stream(
                    test_params_p->output_info,
		    "Error: %Ld of %Ld (generated) bytes are "
		    "correct - ignoring %Ld disp bytes\n",
		    agg_bytes_checked, agg_total_extent, general_disp);
		
	    test_err = 1;
	}
	else
	{
	    if (rw_type == READ)
		mask_stdout(MASK_CORRECT_INFO,
			    "Correct: %Ld of %Ld (generated) bytes are "
			    "correct\n",
			    agg_bytes_checked, agg_total_extent);
	    else
		mask_stdout(MASK_CORRECT_INFO,
			    "Correct: %Ld of %Ld (generated) bytes are "
			    "correct - ignoring %Ld disp bytes\n",
			    agg_bytes_checked, agg_total_extent,
			    general_disp);
	}
    }

    assert(data_buf_off == region_count * region_size * base_dtype_sz);
    return test_err;
}

/* check_data: This function should provide 2 levels of verifcation for
 * checking I/O operations.  (1) - Check only the data written or
 * read.  (2) - Check all data in the file (written) or all data in
 * the buffer (read) */
int check_data(char *buf, int64_t buf_sz,
	       char *filename,
	       int rw_type,
	       int test_type,
	       int noncontig_type,
	       int io_method_type,
	       int param_val,
	       MPI_Offset general_disp,
	       int region_count,
	       int region_size,
	       int region_spacing,
	       MPI_Datatype *base_dtype_p,
	       struct test_params_s *test_params_p)
{
    int ret = -1, test_err = 0, agg_test_err = -1;

    char *data_buf = NULL;
    int64_t data_buf_sz = -1;
    int base_dtype_sz = -1;
    MPI_Aint base_dtype_ext = -1, base_dtype_lb = -1;
    MPI_Datatype data_buf_dtype;

    MPI_Type_lb(*base_dtype_p, &base_dtype_lb);
    MPI_Type_size(*base_dtype_p, &base_dtype_sz);
    MPI_Type_extent(*base_dtype_p, &base_dtype_ext);

    /* Create a temporary buffer for reading the written data or
     * packed the read data. */
    data_buf_sz = (int64_t) region_count * region_size * base_dtype_sz;
    if ((data_buf = (char *) malloc(data_buf_sz * sizeof(char))) == NULL)
    {
	debug_stderr_stream(
	    test_params_p->output_info,
	    "run_test: malloc data_buf of size %Ld failed\n",
	    data_buf_sz);
	return -1;
    }
    memset(data_buf, DEFAULT_EMPTY_CHAR, data_buf_sz);

    MPI_Barrier(MPI_COMM_WORLD);
    if (rw_type == WRITE)
    {
	int wr_pattern_dtype = -1;
	MPI_Datatype wr_memtype, wr_filetype;    
	MPI_Offset wr_fileview_disp = general_disp;
	MPI_File wr_fh;
	int wr_memtype_count = -1;
	MPI_Status wr_status;
	MPI_Info wr_info;
	MPI_Info_create(&wr_info);

	/* When reading the data back, use a different I/O method and
	 * different datatype.  The wr_memtype is ignored since all
	 * the data goes into a contiguous data buffer. */
	if (io_method_type == COLLECTIVE)
	{
	    MPI_Info_set(wr_info, "romio_cb_read", "enable");
	    mask_stdout(MASK_CORRECT, 
			"run_test: Using POSIX for verification\n");
	}
	else
	    mask_stdout(MASK_CORRECT,
			"run_test: Using TWO_PHASE for verification\n");

	if (test_params_p->pattern_dtype == STRUCT)
	{
	    wr_pattern_dtype = STRUCT;
	    mask_stdout(MASK_CORRECT,
			"run_test: Using STRUCT for verification\n");
	}
	else
	{
	    wr_pattern_dtype = VECTOR;
	    mask_stdout(MASK_CORRECT,
			"run_test: Using VECTOR for verification\n");
	}

	create_access_pattern(wr_pattern_dtype,
			      test_params_p->myid,
			      test_params_p->numprocs,
			      noncontig_type,
			      region_count,
			      region_size,
			      region_spacing,
			      base_dtype_p,
			      &wr_memtype_count,
			      &wr_memtype,
			      &wr_filetype,
			      &wr_fileview_disp);

	ret = tMPI_File_open(MPI_COMM_WORLD, filename, MPI_MODE_RDONLY,
			     wr_info, &wr_fh);
	if (ret != MPI_SUCCESS)
	    return ret;

	ret = tMPI_File_set_view(wr_fh, wr_fileview_disp, MPI_CHAR, 
				 wr_filetype, "native", wr_info); 
	if (ret != MPI_SUCCESS)
	    return ret;

	ret = tMPI_File_seek(wr_fh, 0, MPI_SEEK_SET);
	if (ret != MPI_SUCCESS)
	    return ret;

	if (io_method_type == COLLECTIVE)
	    ret = tMPI_File_read(wr_fh, data_buf, data_buf_sz, 
				 MPI_CHAR, &wr_status);
	else
	    ret = tMPI_File_read_all(wr_fh, data_buf, data_buf_sz, 
				     MPI_CHAR, &wr_status);
	if (ret != MPI_SUCCESS)
	    return ret;

	ret = tMPI_File_close(&wr_fh);
	if (ret != MPI_SUCCESS)
	    return ret;

	MPI_Info_free(&wr_info);
	MPI_Type_free(&wr_memtype);
	MPI_Type_free(&wr_filetype);
    }
    else /* In the read case, pack data_buf. */
    {
	int data_buf_pos = 0;

	if (noncontig_type == NC_C || noncontig_type == NC_NC)	    
	{
	    MPI_Type_hvector(region_count, region_size, 
			     (base_dtype_ext * region_size) + 
			     region_spacing,
			     *base_dtype_p, &data_buf_dtype);
	}
	else
	    MPI_Type_contiguous(region_count * region_size * base_dtype_sz,
				MPI_CHAR, &data_buf_dtype);
	
	MPI_Type_commit(&data_buf_dtype);
	MPI_Pack(buf, 1, data_buf_dtype, data_buf, 
		 region_count * region_size * base_dtype_sz, &data_buf_pos,
		 MPI_COMM_SELF);
	assert(data_buf_pos == data_buf_sz);
    }

    /* Check the data buffer (file locations not given) */    
    if (test_params_p->verify == VERIFY_DATA)
    {
	test_err = check_data_buf(noncontig_type, 
				  region_size, 
				  region_count, 
				  region_spacing,
				  data_buf,
				  data_buf_sz,
				  base_dtype_p,
				  test_params_p);
    }

    /* For further verification for writes, read the entire file into
     * the buffer, set the data that was supposedly written to an
     * unused char, and then check all bytes for the default chars or
     * the unused char to find the actual data in the data_buf.  Begin
     * with process 0 checking the disp up to the MPI_LB if it is a
     * write case. */
    if (test_params_p->verify == VERIFY_FILE || test_type == HUMAN || 
	(test_type == CHECK_SINGLE &&
	 test_params_p->def_check_test_num == HUMAN))
    {
	char *io_buf = NULL;
	int64_t io_buf_sz = -1;

	if (rw_type == WRITE)
	{
	    free(data_buf);
	    ret = alloc_and_fill_io_buffers(
		filename, test_params_p->myid, test_params_p->numprocs, 
		region_count, region_size, region_spacing, 
		noncontig_type, base_dtype_p, general_disp, 
		READ, test_params_p,
		&io_buf, &io_buf_sz, &data_buf, &data_buf_sz);
	    if (ret < 0)
		return ret;

	}
	else
	{
	    int def_data_buf_pos = 0;
	    char *def_data_buf = NULL;
	    
	    if ((def_data_buf = (char *) malloc(data_buf_sz * sizeof(char))) 
		== NULL)
	    {
		debug_stderr_stream(
		    test_params_p->output_info,
		    "check_io: malloc def_data_buf of size %Ld "
		    "failed\n", data_buf_sz * sizeof(char));
		return -1;
	    }
	    memset(def_data_buf, DEFAULT_INIT_CHAR, data_buf_sz);
	    MPI_Unpack(def_data_buf, data_buf_sz, &def_data_buf_pos,
		       buf, 1, data_buf_dtype, MPI_COMM_SELF);

	    free(def_data_buf);
	    io_buf = buf;
	    io_buf_sz = buf_sz;
	}

	/* Human verified tests will check manual data against file data */
	if (test_type == HUMAN || (test_type == CHECK_SINGLE && 
		test_params_p->def_check_test_num == HUMAN))
	{
	    ret = check_manual_buf(rw_type,
				   noncontig_type,
				   io_method_type,
				   region_size,
				   region_count,
				   region_spacing,
				   param_val,
				   filename,
				   data_buf,
				   data_buf_sz,
				   io_buf,
				   io_buf_sz,
				   base_dtype_p,
				   general_disp,
				   test_params_p);
	    if (ret == 1 && test_err == 0)
		test_err = 1;
	}

	/* File verified tests will check generated data against file
	 * data */
	if (test_params_p->verify == VERIFY_FILE)
	{
	    ret = check_generated_buf(rw_type,
				      noncontig_type,
				      io_method_type,
				      region_size,
				      region_count,
				      region_spacing,
				      data_buf,
				      data_buf_sz,
				      io_buf,
				      io_buf_sz,
				      base_dtype_p,
				      general_disp,
				      test_params_p);
	    if (ret == 1 && test_err == 0)
		test_err = 1;
	}
	

	if (rw_type == WRITE)
	    free(io_buf);
    }

    ret = MPI_Allreduce(&test_err, &agg_test_err, 1, MPI_INT, 
			MPI_LOR, MPI_COMM_WORLD);
    if (ret != MPI_SUCCESS)
	return -1;
    
    if (agg_test_err == 1)
    {
	test_params_p->tests_failed++;
	if (test_params_p->myid == 0)
	{
	    debug_stdout_stream(
		test_params_p->output_failed,
		"Test rw=%s, type=%s, noncontig=%s, I/O method=%s, ",
		rw_name[rw_type],
		test_type_name[test_type],
		noncontig_type_name[noncontig_type],
		io_method_name[io_method_type]);
	    switch(test_type)
	    {
		case REGION_COUNT:
		case REGION_SIZE:
		case REGION_SPACING:
		    debug_stdout_stream(
			test_params_p->output_failed,
			"param=%d failed\n.",
			param_val);
		    break;
		case BAND_SINGLE:
		    debug_stdout_stream(
			test_params_p->output_failed,
			"param=ct.%d.sz.%d.sp.%d failed\n.",
			region_count, region_size, region_spacing);
		    break;
		case HUMAN:
		    debug_stdout_stream(
			test_params_p->output_failed,
			"param=%s failed\n.",
			human_name[param_val]);
		    break;
		case DEFINED:
		    debug_stdout_stream(
			test_params_p->output_failed,
			"param=%s failed\n.",
			defined_name[param_val]);
		    break;
		case CHECK_SINGLE:
		    debug_stdout_stream(
			test_params_p->output_failed,
			"param=%s failed\n.",
			test_params_p->def_check_test);
		    break;
		default:
		    debug_stderr_stream(
			test_params_p->output_failed,
			"Error: Test type %d invalid\n",
			test_type);
		    return -1;
	    }
	}
    }
    else
	test_params_p->tests_passed++;

    if (rw_type == READ)
	MPI_Type_free(&data_buf_dtype);
    free(data_buf);
    return 0;
}

int check_io_status(MPI_Status *status_p,
		    MPI_Datatype *memtype_p,
		    int memtype_count,
		    int count,
		    int region_count,
		    int region_size,
		    struct test_params_s *test_params_p)
{
    int elements = -1;

    MPI_Get_count(status_p, *memtype_p, &count);
    if (test_params_p->pattern_dtype != STRUCT)
	MPI_Get_elements(status_p, *memtype_p, &elements);

    switch(test_params_p->pattern_dtype)
    {
	case VECTOR:
	    if (count != memtype_count)
	    {
		debug_stderr_stream(
		    test_params_p->output_info,
		    "run_test: Error - Count (%d) "
		    "returned by MPI_Get_count should equal "
		    "region_count (%d)", count, region_count);
		return -1;
	    }
	    if (elements != region_count * region_size)
	    {
		debug_stderr_stream(
		    test_params_p->output_info,
		    "run_test: Error - Elements (%d) "
		    "should equal region_count (%d) * region_size "
		    "(%d)\n", elements, region_count, region_size);
		return -1;
	    }
	    break;
	case STRUCT:
	    if (count != memtype_count)
	    {
		debug_stderr_stream(
		    test_params_p->output_info,
		    "run_test: Error - Count (%d) "
		    "returned by MPI_Get_count should equal "
		    "region_count (%d)", count, region_count);
		return -1;
	    }
	    break;
	default:
	    debug_stderr_stream(
		test_params_p->output_info,
		"run_test: pattern dtype %d invalid\n",
		test_params_p->pattern_dtype);
	    return -1;
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
