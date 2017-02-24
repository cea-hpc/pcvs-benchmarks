/* (C) Northwestern University
 * See COPYING in the top-level directory . */

#include "test.h"
#include "test_params.h"
#include "cache_flush.h"

#define MAX_BUFFER_SIZE 16*1024*1024

/* cache_flush_all
 * input: myid - proc id
 *        numprocs - total # procs
 *        size - MBytes of memory size
 *        filename - temporary filename */

void cache_flush_all(int myid,
		     int numprocs,
		     int size,
		     char *filename)
{
    char *buf;
    MPI_File fh;
    double time;
    /* Calculate how much each processor must write */
    int64_t ind_size = ceil(size / numprocs);
    int64_t comp = 0;

    ind_size = ind_size * 1024 * 1024; /* ind_size converted to MBytes */
    assert(ind_size != 0);

    if ((buf = (char *) malloc(MAX_BUFFER_SIZE * sizeof(char))) == NULL)
    {
	fprintf(stderr, "cache_flush_all: malloc buf of size %d failed\n",
		    MAX_BUFFER_SIZE);
    }
    
    MPI_Barrier(MPI_COMM_WORLD);
    
    MPI_File_open(MPI_COMM_WORLD, filename,
		  MPI_MODE_CREATE | MPI_MODE_WRONLY, MPI_INFO_NULL, &fh);
    MPI_File_set_view(fh, ind_size * myid, MPI_BYTE, MPI_BYTE, 
		      "native", MPI_INFO_NULL);
    MPI_File_seek(fh, 0, MPI_SEEK_SET);

    time = MPI_Wtime();

    while (comp != ind_size)
    {
	if (ind_size - comp > MAX_BUFFER_SIZE)
	{
	    comp += MAX_BUFFER_SIZE;
	    MPI_File_write(fh, buf, MAX_BUFFER_SIZE, 
			   MPI_BYTE, MPI_STATUS_IGNORE);
	}
	else
	{
	    int tmp_bytes = ind_size - comp;
	    comp += ind_size - comp;
	    MPI_File_write(fh, buf, tmp_bytes,
			   MPI_BYTE, MPI_STATUS_IGNORE);
	}
	
    }

    free(buf);

    MPI_File_sync(fh);
    time = MPI_Wtime() - time;
    MPI_File_close(&fh);
    MPI_Barrier(MPI_COMM_WORLD);
#if 0
    if (myid == 0)
    {
	MPI_File_delete(filename, MPI_INFO_NULL);
	fprintf(stderr, 
		"cache_flush_all: File %s written/deleted of "
		"size %.1f MBytes\n"
		"Time: %f secs Bandwidth: %f MBytes / sec\n\n",
		filename, comp*numprocs/1024.0/1024.0,
		time, comp*numprocs/1024.0/1024.0/time);
    }
    MPI_Barrier(MPI_COMM_WORLD);
#endif
}

void cache_flush_ind_all(int myid,
			 int numprocs,
			 int size,
			 char *filename)
{
    char *buf;
    MPI_File fh;
    double time;
    /* Calculate how much each processor must write */
    int64_t ind_size = ceil(size / numprocs);
    int64_t comp = 0;
    char *ind_filename = NULL;
    int ind_filename_size = 0;

    /* We will assume that we are using less than 1,000,000 processors 
     * therefore add 1 for NULL char and 6 for each individual processor 
     * for 7 total */
    ind_filename_size += strlen(filename) + 7;
    
    if ((ind_filename = (char *) malloc(ind_filename_size)) == NULL)
    {
	fprintf(stderr, "cache_flush_ind_all: malloc ind_filename of size"
		"%d failed\n", ind_filename_size);
    }
    sprintf(ind_filename, "%s%d", filename, myid);

    ind_size = ind_size * 1024 * 1024; /* ind_size converted to MBytes */
    assert(ind_size != 0);

    if ((buf = (char *) malloc(MAX_BUFFER_SIZE * sizeof(char))) == NULL)
    {
	fprintf(stderr, "cache_flush_all: malloc buf of size %d failed\n",
		    MAX_BUFFER_SIZE);
    }
    
    MPI_Barrier(MPI_COMM_WORLD);
    
    MPI_File_open(MPI_COMM_SELF, ind_filename,
		  MPI_MODE_CREATE | MPI_MODE_WRONLY, MPI_INFO_NULL, &fh);
    MPI_File_set_view(fh, 0, MPI_BYTE, MPI_BYTE, 
		      "native", MPI_INFO_NULL);
    MPI_File_seek(fh, 0, MPI_SEEK_SET);

    time = MPI_Wtime();

    while (comp != ind_size)
    {
	if (ind_size - comp > MAX_BUFFER_SIZE)
	{
	    comp += MAX_BUFFER_SIZE;
	    MPI_File_write(fh, buf, MAX_BUFFER_SIZE, 
			   MPI_BYTE, MPI_STATUS_IGNORE);
	}
	else
	{
	    int tmp_bytes = ind_size - comp;
	    comp += ind_size - comp;
	    MPI_File_write(fh, buf, tmp_bytes, 
			   MPI_BYTE, MPI_STATUS_IGNORE);
	}
	
    }

    MPI_File_sync(fh);
    time = MPI_Wtime() - time;
    MPI_File_close(&fh);
    MPI_Barrier(MPI_COMM_WORLD);
#if 0
    MPI_File_delete(ind_filename, MPI_INFO_NULL);
#endif
    if (myid == 0)
    {
	fprintf(stderr, 
		"cache_flush_ind_all: File(s) written of "
		"size %.1f MBytes\n"
		"Time: %f secs Bandwidth: %f MBytes / sec\n\n",
		comp*numprocs/1024.0/1024.0,
		time, comp*numprocs/1024.0/1024.0/time);
    }
    MPI_Barrier(MPI_COMM_WORLD);
    free(ind_filename);
    free(buf);
}
		 
void cache_flush_ind(int myid,
		     int numprocs,
		     int size,
		     char *filename)
{
    char *buf;
    MPI_File fh;
    double time;
    int64_t comp = 0;

    assert(size != 0);

    if ((buf = (char *) malloc(MAX_BUFFER_SIZE * sizeof(char))) == NULL)
    {
	fprintf(stderr, "cache_flush_all: malloc buf of size %d failed\n",
		    MAX_BUFFER_SIZE);
    }
    
    MPI_File_open(MPI_COMM_SELF, filename,
		  MPI_MODE_CREATE | MPI_MODE_WRONLY, MPI_INFO_NULL, &fh);
    MPI_File_set_view(fh, 0, MPI_BYTE, MPI_BYTE, 
		      "native", MPI_INFO_NULL);
    MPI_File_seek(fh, 0, MPI_SEEK_SET);

    time = MPI_Wtime();

    while (comp != size)
    {
	if (size - comp > MAX_BUFFER_SIZE)
	{
	    comp += MAX_BUFFER_SIZE;
	    MPI_File_write(fh, buf, MAX_BUFFER_SIZE, 
			   MPI_BYTE, MPI_STATUS_IGNORE);
	}
	else
	{
	    int tmp_bytes = size - comp;
	    comp += size - comp;
	    MPI_File_write(fh, buf, tmp_bytes,
                           MPI_BYTE, MPI_STATUS_IGNORE);

	}
	
    }
    
    free(buf);

    MPI_File_sync(fh);
    time = MPI_Wtime() - time;
    MPI_File_close(&fh);
    MPI_File_delete(filename, MPI_INFO_NULL);
    fprintf(stderr, 
	    "proc %d:cache_flush_ind: File %s written/deleted of "
	    "size %.1f MBytes\n"
	    "Time: %f secs Bandwidth: %f MBytes / sec\n\n",
	    myid,
	    filename, comp*numprocs/1024.0/1024.0,
	    time, comp*numprocs/1024.0/1024.0 / time);
}

void cache_flush_ind_all_remove_files(int myid,
				      int numprocs,
				      char *filename)
{
    /* Calculate how much each processor must write */
    char *ind_filename = NULL;
    int i, ind_filename_size = 0;

    MPI_Barrier(MPI_COMM_WORLD);
    if (myid == 0)
    {
	/* We will assume that we are using less than 1,000,000 processors 
	 * therefore add 1 for NULL char and 6 for each individual processor 
	 * for 7 total */
	
	ind_filename_size += strlen(filename) + 7;
	if ((ind_filename = (char *) malloc(ind_filename_size)) == NULL)
	{
	    fprintf(stderr, "cache_flush_ind_all: malloc ind_filename of size"
		    "%d failed\n", ind_filename_size);
	}
    
	for (i = 0; i < numprocs; i++)
	{
	    sprintf(ind_filename, "%s%d", filename, i);
	    MPI_File_delete(ind_filename,  MPI_INFO_NULL);
	}

	free(ind_filename);
    }
    MPI_Barrier(MPI_COMM_WORLD);
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
