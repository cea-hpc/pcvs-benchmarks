#include "human.h"
#include "../test_params.h"

int human_cpy_buf(int rw_type,
		  int noncontig_type,
		  int *contig_buf,
		  int64_t contig_buf_sz,
		  int *noncontig_buf,
		  int64_t noncontig_buf_sz,
		  int **check_buf_p,
		  int64_t *check_buf_sz_p)
{
    if (rw_type == WRITE)
    {
	if (noncontig_type == C_C || noncontig_type == NC_C)
	{
	    *check_buf_sz_p = contig_buf_sz;
	    if ((*check_buf_p = (int *) malloc(*check_buf_sz_p * sizeof(int))) 
		== NULL)
	    {
		debug_stderr("dup_alloc_check_bufs: malloc *check_buf_p of "
			     "size %d failed\n", 
			     *check_buf_sz_p * sizeof(int));
		return -1;
	    }

	    memcpy(*check_buf_p, contig_buf, *check_buf_sz_p * sizeof(int));
	}
	else
	{
	    *check_buf_sz_p = noncontig_buf_sz;
	    if ((*check_buf_p = (int *) malloc(*check_buf_sz_p * sizeof(int))) 
		== NULL)
	    {
		debug_stderr("dup_alloc_check_bufs: malloc *check_buf_p of "
			     "size %d failed\n", 
			     *check_buf_sz_p * sizeof(int));
		return -1;
	    }

	    memcpy(*check_buf_p, noncontig_buf, *check_buf_sz_p * sizeof(int));
	}
    }
    else
    {
	if (noncontig_type == C_C || noncontig_type == C_NC)
	{
	    *check_buf_sz_p = contig_buf_sz;
	    if ((*check_buf_p = (int *) malloc(*check_buf_sz_p * sizeof(int))) 
		== NULL)
	    {
		debug_stderr("dup_alloc_check_bufs: malloc *check_buf_p of "
			     "size %d failed\n", 
			     *check_buf_sz_p * sizeof(int));
		return -1;
	    }

	    memcpy(*check_buf_p, contig_buf, *check_buf_sz_p * sizeof(int));
	}
	else
	{
	    *check_buf_sz_p = noncontig_buf_sz;
	    if ((*check_buf_p = (int *) malloc(*check_buf_sz_p * sizeof(int))) 
		== NULL)
	    {
		debug_stderr("dup_alloc_check_bufs: malloc *check_buf_p of "
			     "size %d failed\n", 
			     *check_buf_sz_p * sizeof(int));
		return -1;
	    }

	    memcpy(*check_buf_p, noncontig_buf, *check_buf_sz_p * sizeof(int));
	}
    }

    return 0;
}
