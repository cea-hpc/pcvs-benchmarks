/* (C) Northwestern University
 * See COPYING in the top-level directory . */

#include "test.h"
#include "buf.h"

/* Generate memory data such that file data is written to according to
 * position which allows for easier checking.  Put the correct data
 * into tmp_buf and then unpack it based on the access pattern.  */
int init_data_buf(char *buf, int64_t buf_sz, int myid, int numprocs,
		  int noncontig_type, int region_count, 
		  int region_size, int region_spacing, 
		  int base_dtype_sz, int mpiio_count, 
		  MPI_Datatype *memtype_p)
{
    int i, j, buf_pos = 0;
    int64_t global_test_dtype_block = 0, tmp_buf_sz = -1;
    char *tmp_buf = NULL;

    tmp_buf_sz = (int64_t) region_count * region_size * base_dtype_sz;
    if ((tmp_buf = malloc(tmp_buf_sz * sizeof(char))) == NULL)
    {
	fprintf(stderr, "init_data_buf: malloc tmp_buf of size %Ld failed\n",
		tmp_buf_sz * sizeof(char));
	return -1;
    }

    if (noncontig_type == C_C || noncontig_type == NC_C)
	global_test_dtype_block = myid * region_count;
    else
	global_test_dtype_block = myid;

    for (i = 0; i < region_count; i++)
    {
	for (j = 0; j < region_size * base_dtype_sz; j++)
	{
	    tmp_buf[(i * region_size * base_dtype_sz) + j] =
		gen_file_char(global_test_dtype_block + j);
	}
	
	if (noncontig_type == C_C || noncontig_type == NC_C)
	    global_test_dtype_block++;
	else
	    global_test_dtype_block += numprocs;
    }
    buf_pos = 0;

    MPI_Unpack(tmp_buf, tmp_buf_sz, &buf_pos, 
	       buf, mpiio_count, *memtype_p, MPI_COMM_SELF);

    assert(buf_pos = tmp_buf_sz);
    free(tmp_buf);

    return 0;
}


/* Generate memory data such that file data is written to according to
 * position which allows for easier checking */

void init_global_buf(char *buf, int64_t len, int myid, int numprocs,
		     int noncontig_type, int region_count, 
		     int region_size, int region_spacing)
{
    int i, local_blk_num, global_blk_num, myid_blk_off;
    
    switch (noncontig_type)
    {
	case C_C:
	    for (i = 0; i < len; i++)
	    {
		local_blk_num = i / region_size;
		myid_blk_off = myid * region_count;
		global_blk_num = myid_blk_off + local_blk_num;
		buf[i] = gen_file_char(
		    i - (local_blk_num * region_size) + global_blk_num);
	    }
	    break;
	case NC_C:
	    for (i = 0; i < len; i++)
	    {
		/* Falls within a block */
		if (i % (region_size + region_spacing) < region_size)
		{
		    local_blk_num = i / (region_size + region_spacing);
		    myid_blk_off = myid * region_count;
		    global_blk_num = myid_blk_off + local_blk_num;
		    buf[i] = gen_file_char(
			i - (local_blk_num * (region_size + region_spacing)) +
			global_blk_num);
		}
		else
		    buf[i] = (char) DEFAULT_CHAR;
	    }
	    break;
	case C_NC:
	    for (i = 0; i < len; i++) {
		local_blk_num = i / region_size;
		global_blk_num = numprocs * local_blk_num + myid;
		buf[i] = gen_file_char(i - (local_blk_num * region_size) + 
				       global_blk_num);
	    }
	    break;
	case NC_NC:
	    for (i = 0; i < len; i++)
	    {
		/* Falls within a block */
		if (i % (region_size + region_spacing) < region_size)
		{
		    local_blk_num = i / (region_size + region_spacing);
		    global_blk_num = (local_blk_num * numprocs) + myid;
		    buf[i] = gen_file_char(
			i - (local_blk_num * (region_size + region_spacing)) + 
			global_blk_num);
		}
		else
		    buf[i] = (char) DEFAULT_CHAR;
	    }
	    break;
	default:
	    fprintf(stderr, "init_global_buf: Invalid noncontig_type %d\n",
		    noncontig_type);
    }
}

void init_buf(char *buf, int64_t len, int rw_type, int64_t start_byte)
{
    int i;
    if (rw_type == WRITE)
    {
	for (i = 0; i < len; i++)
	{
#if 0
	    /* numbers 0 - 9 */
	    buf[i] = gen_char(i + start_byte, 10, 48);
	    /* letters A -J */
	    buf[i] = gen_char(i + start_byte, 10, 65);
#else
	    /* assorted letters and some symbols A - Z - symbols a - z*/
	    buf[i] = gen_char(i+start_byte, 58, 65);
#endif
	}
    }
    else /* READ */
    {
	memset(buf, start_byte, len);
    }
}

void print_char_buf(char *buf_name, char *buf, int64_t buf_len)
{
    int i;

    debug_stdout("print_char_buf: %s\n", buf_name);
    for (i = 0; i < buf_len; i++)
    {
	debug_stdout("%c ", buf[i]);
	if (((i + 1) % 10) == 0)
	    debug_stdout("\n");	    
	else if (((i + 1) % 5) == 0)
	    debug_stdout("  ");
    }
    debug_stdout("\n");
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
