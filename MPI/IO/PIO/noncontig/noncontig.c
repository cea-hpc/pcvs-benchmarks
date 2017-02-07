#include <stdio.h>
#include <string.h>
#include <stdlib.h>

#include "mpi.h"

/* tests noncontiguous reads/writes using independent I/O */

/* default values */
#define FNAME    "noncontig_test_file.dat"
#define ELEMENTS 10
#define COUNT    1
#define SSIZE    -1
#define SFACT    0
#define DISPLS   0
#define OFFSET   0
#define RMFILE   0

#define MAX_FNAME_LEN 1024

#define NCMEM_NCFILE 1
#define CMEM_NCFILE  2
#define NCMEM_CFILE  4
#define CMEM_CFILE   8
#define RUN_TESTS    (NCMEM_NCFILE|CMEM_NCFILE|NCMEM_CFILE)

/* verification parameters */
#define USE_DEFAULT_BUFSIZES      1
#if !USE_DEFAULT_BUFSIZES
#define INDIVIDUAL_READ_BUFSIZE   "1209"
#define INDIVIDUAL_WRITE_BUFSIZE  "1107"
#define COLLECTIVE_BUFSIZE        "32"
#endif

#define MB (1024*1024)
#define MIN(a,b) (a) < (b) ? (a) : (b)
#define MAX(a,b) (a) > (b) ? (a) : (b)

int cmdline_get_int(int *argc, char **argv, int rflag, char *name, int *val);
int cmdline_get_double(int *argc, char **argv, int rflag, 
		char *name, double *val);
int cmdline_get_string(int *argc, char **argv, int rflag, 
		char *name, char *val, int vallen);

int cmdline_has_name(int *argc, char **argv, int rflag, char *name);

int do_timing = 0;
int do_sync = 1;
int do_coll = 0;
int be_verbose = 0;
int loops = 1;
int do_verify; 

int nprocs, myrank;

#define HELP "noncontig tests non-contiguous file access.\n"\
"Arguments:\n"\
" -fname     filename used for testing (mandatory)\n"\
" -fsize     filesize (MB)\n"\
" -ionodes   names of ionodes used (if applicable; comma-separated list)\n"\
" -elmtcount number of MPI_INTs per element\n"\
" -veclen    number of elements in vector datatype used for testing\n"\
" -veccount  number of concatenated vectors read or written\n"\
" -sfact     striping factor (bytes)\n"\
" -ssize     size of striping unit (bytes)\n"\
" -displs    file type displacement (bytes)\n"\
" -offset    offset of access in file (in etypes, which is MPI_INT) \n"\
" -timing    print performance values\n"\
" -nosync    do not sync after writes\n"\
" -v         be verbose\n"\
" -cbsize    buffer size for collective buffering (KB)\n"\
" -coll      perform collective file access\n"\
" -bufsize   size (KB) of memory buffer to use for access (multipe vector length)\n"\
" -loops     number of repeats of write/read operations\n"\
"\n"\
" Access type selection:\n"\
" -ncmem_ncfile    both memory data and file data are non-contiguous\n"\
" -cmem_ncfile     contiguous memory data and non-contiguous file data\n"\
" -ncmem_cfile     non-contiguous memory data and contiguous file data \n"\
" -cmem_cfile      both memory data and file data are contiguous\n"\
" -all             test all access types \n"


void handle_error(int errcode, char *str)
{
	char msg[MPI_MAX_ERROR_STRING];
	int resultlen;
	MPI_Error_string(errcode, msg, &resultlen);
	fprintf(stderr, "%s: %s\n", str, msg);
	MPI_Abort(MPI_COMM_WORLD, 1);
}

void print_timing (int is_read, MPI_Aint data_size, MPI_Aint file_size, double t)
{
    double t_max, t_min, *all_t, acc_bw;
    char read_bw[10] = "read ";
    char write_bw[10] = "write";
    char bw[10];
    int i;

    if (is_read)
	strcpy(bw, read_bw);
    else
	strcpy(bw, write_bw);

    all_t = (double *)malloc(nprocs*sizeof(double));
    MPI_Reduce (&t, &t_min, 1, MPI_DOUBLE, MPI_MIN, 0, MPI_COMM_WORLD);
    MPI_Reduce (&t, &t_max, 1, MPI_DOUBLE, MPI_MAX, 0, MPI_COMM_WORLD);
    MPI_Gather (&t, 1, MPI_DOUBLE, all_t, 1, MPI_DOUBLE, 0, MPI_COMM_WORLD);
    
    if (!myrank) {
	for (i = 0, acc_bw = 0.0; i < nprocs; i++)
	    acc_bw += data_size/(all_t[i]/loops);
	printf (" %s bandwidth (min/max/acc [MB/s]) : %6.3f / %6.3f / %6.3f\n", bw,
		(data_size/(t_max/loops))/MB, (data_size/(t_min/loops))/MB, acc_bw/MB);
	if (is_read)
	    printf (" file size: %dkB  size per process: %dkB\n",
		    file_size/1024, data_size/1024);
    }
    free (all_t);   
}


int noncontigmem_noncontigfile (char *filename, int *buf, MPI_Aint bufsize, 
		MPI_Datatype dtype, MPI_Offset offset, MPI_Offset displs, 
		MPI_Info finfo, int veclen, int elmtcount, int veccount)
{
    MPI_File fh;
    MPI_Status status;
    MPI_Offset fsize;
    MPI_Aint data_ext, data_size;
    int err_cnt, acc_count, get_count, all_count;
    int i, c, l ,elmt_align, mpi_error;    
    double t0, t_total;

    err_cnt = 0;

    MPI_Type_size (dtype, &i);
    data_size = i*veccount;
    MPI_Type_extent (dtype, &data_ext);
    /* unless overridden on the command line, 
     * bufsize = veccount*veclen*elmtcount*sizeof(int) */
    acc_count = bufsize/data_ext;

    if (!myrank) {
	if (do_coll)
	    printf("\n# testing noncontiguous in memory, noncontiguous in file using collective I/O\n");
	else
	    printf("\n# testing noncontiguous in memory, noncontiguous in file using independent I/O\n");
	printf ("# vector count = %d - access count = %d\n", veccount, acc_count);
	fflush (stdout);
	MPI_File_delete(filename, MPI_INFO_NULL);
    }
    MPI_Barrier(MPI_COMM_WORLD);
    
    mpi_error = MPI_File_open(MPI_COMM_WORLD, filename, 
		    MPI_MODE_CREATE | MPI_MODE_RDWR, finfo, &fh);
    if (mpi_error != MPI_SUCCESS) {
	    handle_error(mpi_error, "MPI_File_open");
    }

    MPI_File_set_view(fh, displs, MPI_INT, dtype, "native", finfo);
    
    if (nprocs == 2)
	elmt_align = myrank > 0 ? veclen%2 : 0;
    else 
	elmt_align = 0;

    for (i = 0; i < acc_count*veclen; i++) {
	buf[i] = i%veclen + myrank*veclen;
    }

    t_total = 0;
    for (l = 0; l < loops; l++) {
	MPI_File_seek(fh, offset, MPI_SEEK_SET);
	c = 0; all_count = 0;
	t0 = MPI_Wtime();
	while (c < veccount) {
	    int use_count = MIN(acc_count, veccount - c);
	    
	    mpi_error = do_coll ?
		MPI_File_write_all(fh, buf, use_count, dtype, &status) :
		MPI_File_write(fh, buf, use_count, dtype, &status);
	    MPI_Get_count (&status, dtype, &get_count);
	    all_count += get_count;
	    if (use_count != get_count || mpi_error != MPI_SUCCESS)
		goto write_exit;
	    c += acc_count;
	}
	if (do_sync)
  	    MPI_File_sync(fh);
	t0 = MPI_Wtime() - t0;
	t_total += t0;
    }
    /* for two processes, veccount = 1 and veclen = 10, the file now reads (sequentially):
       0, 11, 2, 13, 4, 15, 6, 17, 8, 19 */
 write_exit:
    if (mpi_error != MPI_SUCCESS) {
       char string[MPI_MAX_ERROR_STRING+1];
       int len;

       MPI_Error_string (mpi_error, string, &len);
       fprintf (stderr, "[%d] Error %d in MPI_File_write\n%s\n",
                       myrank, mpi_error, string);
    } else {
       if (all_count != veccount) {
          printf("[%d] write-count is %d should be %d\n",
                 myrank, get_count, all_count);
       }
    }

    if (do_timing) 	
	print_timing(0, data_size, fsize, t_total);

    MPI_Barrier(MPI_COMM_WORLD);
    MPI_File_get_size (fh, &fsize);

    for (i = 0; i < acc_count*veclen; i++) 
	buf[i] = -1;

    t_total = 0;
    for (l = 0; l < loops; l++) {
	c = 0; all_count = 0;
	MPI_File_seek(fh, offset, MPI_SEEK_SET);
	t0 = MPI_Wtime();
	while (c < veccount) {
	    int use_count = MIN(acc_count, veccount - c);
	    
	    mpi_error = do_coll ?
		MPI_File_read_all(fh, buf, use_count, dtype, &status) : 
		MPI_File_read(fh, buf, use_count, dtype, &status);
	    MPI_Get_count (&status, dtype, &get_count);
	    all_count += get_count;
	    if (use_count != get_count || mpi_error != MPI_SUCCESS)
		goto read_exit;
	    c += acc_count;
	}
	t0 = MPI_Wtime() - t0;
	t_total += t0;
    }

 read_exit:
    if (mpi_error != MPI_SUCCESS) {
       char string[MPI_MAX_ERROR_STRING+1];
       int len;

       MPI_Error_string (mpi_error, string, &len);
       fprintf (stderr, "[%d] Error %d in MPI_File_read\n%s\n",
		myrank, mpi_error, string);
    } else {
       if (all_count != veccount) {
          printf("[%d] write-count is %d should be %d\n",
                 myrank, get_count, all_count);
       }
    }

    if (do_verify)
	for (c = 0; c < veccount; c++) {
	    elmt_align = c % 2 != 0 && veclen % 2 != 0 ? 1 : 0;
	    
	    for (i = 0; i < veclen; i++) {
		int idx_align, idx, cval;
		
		idx = c*veclen+i;
		idx_align = idx + elmt_align;
		cval = i + myrank*veclen;
		if ((idx_align % nprocs != myrank) && (buf[idx] != -1)) {
		    /* these buffer positions should be untouched */
		    printf("[%d] *** error: buf[%d] is %d, should be -1\n", myrank, idx, buf[idx]);
		    err_cnt++;
		}
		if ((idx_align % nprocs == myrank) && (buf[idx] != cval)) {
		    /* these buffer positions must have been overwritten with  value from file */
		    printf("[%d] *** error: buf[%d] is %d, should be %d\n", myrank, idx, buf[idx], cval);
		    err_cnt++;
		}
	    }
	}

    if (do_timing) 	
	print_timing(1, data_size, fsize, t_total);
    
    MPI_File_close(&fh);

    return err_cnt;
}


int noncontigmem_contigfile (char *filename, int *buf, MPI_Aint bufsize, 
		MPI_Datatype dtype, MPI_Offset offset, MPI_Offset displs, 
		MPI_Info finfo, int veclen, int elmtcount, int veccount)
{
    MPI_File fh;
    MPI_Status status;
    MPI_Offset fsize;
    MPI_Aint data_ext, data_size;
    int err_cnt, acc_count, get_count, all_count;
    int i, c, l ,elmt_align, mpi_error;    
    double t0, t_total;

    err_cnt = 0;

    MPI_Type_size (dtype, &i);
    data_size = i*veccount;
    MPI_Type_extent (dtype, &data_ext);
    acc_count = bufsize/data_ext;
    
    if (!myrank) {
	if (do_coll)
	    printf("\n# testing noncontiguous in memory, contiguous in file using collective I/O\n");
	else
	    printf("\n# testing noncontiguous in memory, contiguous in file using independent I/O\n");
	printf ("# vector count = %d - access count = %d\n", veccount, acc_count);
	fflush (stdout);
	MPI_File_delete(filename, MPI_INFO_NULL);
    }
    MPI_Barrier(MPI_COMM_WORLD);

    mpi_error = MPI_File_open(MPI_COMM_WORLD, filename, 
		    MPI_MODE_CREATE|MPI_MODE_RDWR, finfo, &fh);
    if (mpi_error != MPI_SUCCESS) {
	    handle_error(mpi_error, "MPI_File_open");
    }

    for (i = 0; i < acc_count*veclen; i++) 
	buf[i] = i%veclen + myrank*veclen;

    if (nprocs == 2)
	elmt_align = myrank > 0 ? veclen%2 : 0;
    else 
	elmt_align = 0;
    offset = offset + veccount*(myrank*elmtcount*(veclen/nprocs)+elmt_align)*sizeof(int);

    t_total = 0;
    for (l = 0; l < loops; l++) {
	MPI_File_seek(fh, offset, MPI_SEEK_SET);
	c = 0; all_count = 0;
	t0 = MPI_Wtime();
	while (c < veccount) {
	    int use_count = MIN(acc_count, veccount - c);
	    
	    mpi_error = do_coll ?
		MPI_File_write_all(fh, buf, use_count, dtype, &status) :
		MPI_File_write(fh, buf, use_count, dtype, &status);
	    MPI_Get_count (&status, dtype, &get_count);
	    all_count += get_count;
	    if (use_count != get_count || mpi_error != MPI_SUCCESS)
		goto write_exit;
	    c += acc_count;
	}
	if (do_sync)
	    MPI_File_sync(fh);
	t0 = MPI_Wtime() - t0;
	t_total += t0;
    }
    /* for two processes, veccount = 1 and veclen = 10, the file now reads (sequentially):
       0, 2, 4, 6, 8, 11, 13, 15, 17, 19 */
 write_exit:
    if (mpi_error != MPI_SUCCESS) {
	char string[MPI_MAX_ERROR_STRING+1];
	int len;
	
	MPI_Error_string (mpi_error, string, &len);
	fprintf (stderr, "[%d] Error %d in MPI_File_write_at\n%s\n",
		 myrank, mpi_error, string);
    } else {
	if (all_count != veccount) {
	    printf("[%d] write_at: count is %d should be %d\n",
		   myrank, all_count, veccount);
	}
    }

    if (do_timing) 	
	print_timing(0, data_size, fsize, t_total);

    MPI_Barrier(MPI_COMM_WORLD);
    MPI_File_get_size (fh, &fsize);

    for (i = 0; i < acc_count*veclen; i++) 
	buf[i] = -1;

    t_total = 0;
    for (l = 0; l < loops; l++) {
	c = 0; all_count = 0;
	MPI_File_seek(fh, offset, MPI_SEEK_SET);
	t0 = MPI_Wtime();
	while (c < veccount) {
	    int use_count = MIN(acc_count, veccount - c);
	    
	    mpi_error = do_coll ?
		MPI_File_read_all(fh, buf, use_count, dtype, &status) : 
		MPI_File_read(fh, buf, use_count, dtype, &status);
	    MPI_Get_count (&status, dtype, &get_count);
	    all_count += get_count;
	    if (use_count != get_count || mpi_error != MPI_SUCCESS)
		goto read_exit;
	    c += acc_count;
	}
	t0 = MPI_Wtime() - t0;
	t_total += t0;
    }

 read_exit:
    if (mpi_error != MPI_SUCCESS) {
       char string[MPI_MAX_ERROR_STRING+1];
       int len;

       MPI_Error_string (mpi_error, string, &len);
       fprintf (stderr, "[%d] Error %d in MPI_File_read\n%s\n",
		myrank, mpi_error, string);
    } else {
	if (all_count != veccount) {
	    printf("[%d] read-count is %d should be %d\n",
		   myrank, all_count, veccount);
	}
    }

    if (do_verify)
	for (c = 0; c < veccount; c++) {
	    elmt_align = c % 2 != 0 && veclen % 2 != 0 ? 1 : 0;
	    for (i = 0; i < veclen; i++) {
		int idx_align, idx, cval;
		
		idx = c*veclen+i;
		idx_align = idx + elmt_align;
		cval = i + myrank*veclen;
		
		if ((idx_align % nprocs != myrank) && (buf[idx] != -1)) {
		    /* these buffer positions should be untouched */
		    printf("[%d] *** error: buf[%d] is %d, should be -1\n", myrank, idx, buf[idx]);
		    err_cnt++;
		}
		if ((idx_align % nprocs == myrank) && (buf[idx] != cval)) {
		    /* these buffer positions must have been overwritten with  value from file */
		    printf("[%d] *** error: buf[%d] is %d, should be %d\n", myrank, idx, buf[idx], cval);
		    err_cnt++;
		}
	    }
	}

    if (do_timing) 	
	print_timing(1, data_size, fsize, t_total);

    MPI_File_close(&fh);
    MPI_Barrier(MPI_COMM_WORLD);
    
    return err_cnt;
}


int contigmem_noncontigfile (char *filename, int *buf, MPI_Aint bufsize, 
		MPI_Datatype dtype, MPI_Offset offset, MPI_Offset displs, 
		MPI_Info finfo, int veclen, int elmtcount, int veccount)
{
    MPI_File fh;
    MPI_Status status;
    MPI_Offset fsize;
    MPI_Aint data_size;
    int err_cnt, acc_count, get_count, all_count;
    int i, c, l ,mpi_error;    
    double t0, t_total;

    err_cnt = 0;

    /* amount of data to write in total */
    data_size = veccount*veclen*elmtcount*sizeof(int);
    /* number of vectors to access in each I/O call */
    /* if no bufsize provided on command line, 
     * 	then acc_count == veccount: in getbuf() we have
     *    bufsize = veccount * veclen*elmtcount*sizeof(int)
     */
    acc_count = bufsize/(veclen*elmtcount*sizeof(int));

    if (!myrank) {
	if (do_coll)
	    printf("\n# testing contiguous in memory, noncontiguous in file using collective I/O\n");
	else
	    printf("\n# testing contiguous in memory, noncontiguous in file using independent I/O\n");
	printf ("# vector count = %d - access count = %d\n", veccount, acc_count);
	fflush (stdout);
	MPI_File_delete(filename, MPI_INFO_NULL);
    }
    MPI_Barrier(MPI_COMM_WORLD);

    mpi_error = MPI_File_open(MPI_COMM_WORLD, filename, 
		    MPI_MODE_CREATE | MPI_MODE_RDWR, finfo, &fh);
    if (mpi_error != MPI_SUCCESS) {
	    handle_error(mpi_error, "MPI_File_open");
    }

    MPI_File_set_view(fh, displs, MPI_INT, dtype, "native", finfo);

    for (i = 0; i < acc_count*veclen; i++) 
	buf[i] = i%veclen + myrank*veclen;

    t_total = 0;
    for (l = 0; l < loops; l++) {
	MPI_File_seek(fh, offset, MPI_SEEK_SET);
	c = 0; all_count = 0;
	t0 = MPI_Wtime();
	while (c < veccount) {
	    int use_count = MIN(acc_count, veccount - c);
	    use_count *= elmtcount*veclen;

	    mpi_error = do_coll ?
		MPI_File_write_all(fh, buf, use_count, MPI_INT, &status) :
		MPI_File_write(fh, buf, use_count, MPI_INT, &status);
	    MPI_Get_count (&status, MPI_INT, &get_count);
	    all_count += get_count;
	    if (use_count != get_count || mpi_error != MPI_SUCCESS)
		goto write_exit;
	    c += acc_count;
	}
	if (do_sync)
	    MPI_File_sync(fh);
	t0 = MPI_Wtime() - t0;
	t_total += t0;
    }
    /* for two processes, veccount = 1 and veclen = 10, the file now reads (sequentially):
       0, 11, 1, 12, 2, 13, 3, 14, 4, 15, 5, 16, 6, 17, 7, 18, 8, 19, 9, 20 */
 write_exit:
    if (mpi_error != MPI_SUCCESS) {
       char string[MPI_MAX_ERROR_STRING+1];
       int len;

       MPI_Error_string (mpi_error, string, &len);
       fprintf (stderr, "[%d] Error %d in MPI_File_write\n%s\n",
                       myrank, mpi_error, string);
    } else {
       if (all_count != elmtcount*veccount*veclen) {
          printf("[%d] write-count is %d should be %d\n",
                 myrank, all_count, elmtcount*veccount*veclen);
       }
    }

    if (do_timing) 	
	print_timing(0, data_size, fsize, t_total);

    MPI_Barrier (MPI_COMM_WORLD);
    MPI_File_get_size (fh, &fsize);

    for (i = 0; i < acc_count*veclen; i++) 
	buf[i] = -1;

    t_total = 0;
    for (l = 0; l < loops; l++) {
	c = 0; all_count = 0;
	MPI_File_seek(fh, offset, MPI_SEEK_SET);
	t0 = MPI_Wtime();
	while (c < veccount) {
	    int use_count = MIN(acc_count, veccount - c);
	    use_count *= elmtcount*veclen;

	    mpi_error = do_coll ?
		MPI_File_read_all(fh, buf, use_count, MPI_INT, &status) :
		MPI_File_read(fh, buf, use_count, MPI_INT, &status);
	    MPI_Get_count (&status, MPI_INT, &get_count);
	    all_count += get_count;
	    if (use_count != get_count || mpi_error != MPI_SUCCESS)
		goto read_exit;
	    c += acc_count;
	}
	t0 = MPI_Wtime() - t0;
	t_total += t0;
    }

 read_exit:
    if (mpi_error != MPI_SUCCESS) {
       char string[MPI_MAX_ERROR_STRING+1];
       int len;

       MPI_Error_string (mpi_error, string, &len);
       fprintf (stderr, "[%d] Error %d in MPI_File_read\n%s\n",
                       myrank, mpi_error, string);
    } else {
       if (all_count != veccount*elmtcount*veclen) {
          printf("[%d] read-count is %d should be %d\n",
                 myrank, all_count, veccount*elmtcount*veclen);
       }
    }

    if (do_verify)
	for (c = 0; c < veccount; c++) {
	    for (i = 0; i < veclen; i++) {
		int idx, cval;
		
		idx = c*veclen+i;
		cval = i + myrank*veclen;
		
		if (buf[idx] != cval) {
		    printf("[%d] *** error: buf[%d] is %d, should be %d\n", 
			   myrank, idx, buf[idx], cval);
		    err_cnt++;
		}
	    }
	}

    if (do_timing) 	
	print_timing(1, data_size, fsize, t_total);

    MPI_File_close(&fh);

    return err_cnt;
}


int contigmem_contigfile (char *filename, int *buf, MPI_Aint bufsize, 
		MPI_Datatype dtype, MPI_Offset offset, MPI_Offset displs, 
		MPI_Info finfo, int veclen, int elmtcount, int veccount)
{
    MPI_File fh;
    MPI_Status status;
    MPI_Offset fsize;
    MPI_Aint data_size;
    int err_cnt, acc_count, get_count, all_count;
    int i, c, l , mpi_error;    
    double t0, t_total;

    err_cnt = 0;

    /* amount of data to write in total */
    data_size = veccount*veclen*elmtcount*sizeof(int);
    /* number of vectors to access in each I/O call */
    acc_count = bufsize/(veclen*elmtcount*sizeof(int));

    if (!myrank) {
	if (do_coll)
	    printf("\n# testing contiguous in memory, contiguous in file using collective I/O\n");
	else
	    printf("\n# testing contiguous in memory, contiguous in file using independent I/O\n");
	printf ("# vector count = %d - access count = %d\n", veccount, acc_count);
	fflush (stdout);
	MPI_File_delete(filename, MPI_INFO_NULL);
    }
    MPI_Barrier(MPI_COMM_WORLD);

    mpi_error = MPI_File_open(MPI_COMM_WORLD, filename, 
		    MPI_MODE_CREATE | MPI_MODE_RDWR, finfo, &fh);

    if (mpi_error != MPI_SUCCESS) {
	    handle_error(mpi_error, "MPI_File_open");
    }

    offset += myrank*data_size;

    for (i = 0; i < acc_count*veclen; i++) 
	buf[i] = i%veclen + myrank*veclen;

    t_total = 0;
    for (l = 0; l < loops; l++) {
	MPI_File_seek(fh, offset, MPI_SEEK_SET);
	c = 0; all_count = 0;
	t0 = MPI_Wtime();
	while (c < veccount) {
	    int use_count = MIN(acc_count, veccount - c);
	    use_count *= elmtcount*veclen;

	    mpi_error = do_coll ?
		MPI_File_write_all(fh, buf, use_count, MPI_INT, &status) :
		MPI_File_write(fh, buf, use_count, MPI_INT, &status);
	    MPI_Get_count (&status, MPI_INT, &get_count);
	    all_count += get_count;
	    if (use_count != get_count || mpi_error != MPI_SUCCESS)
		goto write_exit;
	    c += acc_count;
	}
	if (do_sync)
	    MPI_File_sync(fh);
	t0 = MPI_Wtime() - t0;
	t_total += t0;
    }
    /* for two processes, veccount = 1 and veclen = 10, the file now reads (sequentially):
       0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19 */
 write_exit:
    if (mpi_error != MPI_SUCCESS) {
       char string[MPI_MAX_ERROR_STRING+1];
       int len;

       MPI_Error_string (mpi_error, string, &len);
       fprintf (stderr, "[%d] Error %d in MPI_File_write\n%s\n",
                       myrank, mpi_error, string);
    } else {
       if (all_count != elmtcount*veccount*veclen) {
          printf("[%d] write-count is %d should be %d\n",
                 myrank, all_count, elmtcount*veccount*veclen);
       }
    }

    if (do_timing) 	
	print_timing(0, data_size, fsize, t_total);

    MPI_Barrier (MPI_COMM_WORLD);
    MPI_File_get_size (fh, &fsize);

    for (i = 0; i < acc_count*veclen; i++) 
	buf[i] = -1;

    t_total = 0;
    for (l = 0; l < loops; l++) {
	c = 0; all_count = 0;
	MPI_File_seek(fh, offset, MPI_SEEK_SET);
	t0 = MPI_Wtime();
	while (c < veccount) {
	    int use_count = MIN(acc_count, veccount - c);
	    use_count *= elmtcount*veclen;

	    mpi_error = do_coll ?
		MPI_File_read_all(fh, buf, use_count, MPI_INT, &status) :
		MPI_File_read(fh, buf, use_count, MPI_INT, &status);
	    MPI_Get_count (&status, MPI_INT, &get_count);
	    all_count += get_count;
	    if (use_count != get_count || mpi_error != MPI_SUCCESS)
		goto read_exit;
	    c += acc_count;
	}
	t0 = MPI_Wtime() - t0;
	t_total += t0;
    }

 read_exit:
    if (mpi_error != MPI_SUCCESS) {
       char string[MPI_MAX_ERROR_STRING+1];
       int len;

       MPI_Error_string (mpi_error, string, &len);
       fprintf (stderr, "[%d] Error %d in MPI_File_read\n%s\n",
                       myrank, mpi_error, string);
    } else {
       if (all_count != veccount*elmtcount*veclen) {
          printf("[%d] read-count is %d should be %d\n",
                 myrank, all_count, veccount*elmtcount*veclen);
       }
    }

    if (do_verify)
	for (c = 0; c < veccount; c++) {
	    for (i = 0; i < veclen; i++) {
		int idx, cval;
		
		idx = c*veclen+i;
		cval = i + myrank*veclen;
		
		if (buf[idx] != cval) {
		    printf("[%d] *** error: buf[%d] is %d, should be %d\n", 
			   myrank, idx, buf[idx], cval);
		    err_cnt++;
		}
	    }
	}
    
    if (do_timing) 
	print_timing(1, data_size, fsize, t_total);

    MPI_File_close(&fh);

    return err_cnt;
}


void print_help (int myrank)
{
    if (myrank == 0) {
	printf ("%s", HELP);
    }
    MPI_Finalize();
}


int *getbuf (MPI_Aint *bufsize, int veccount, int veclen, int elmtcount) 
{
    size_t len;
    int *buf;

    if (*bufsize != 0) {
	*bufsize = MIN(*bufsize, veccount*veclen*elmtcount*sizeof(int));
	len = veclen*elmtcount*sizeof(int);
	if (len < *bufsize) 
	    len = *bufsize/len * len;
    } else {
	len = veccount*veclen*elmtcount*sizeof(int);
    }
    *bufsize = (MPI_Aint)len;
    buf = (int *)malloc(len);
    if (!buf) {
	printf("\n*# out of memory\n"); MPI_Abort(MPI_COMM_WORLD, 1);
    }
    
    return buf;
}

int main(int argc, char **argv)
{
    int *buf, i, b[3], run_tests, cbsize;
    int nbr_errors, veclen, veccount, sfact, ssize, elmt_align;
    int elmtcount, fsize, stride;
    MPI_Aint d[3], bufsize;
    char *filename, *ionodes, tmp[256];
    MPI_Datatype elmttype, typevec, newtype, t[3];
    MPI_Info finfo;
    MPI_Offset displs, offset;

    MPI_Init(&argc,&argv);
    MPI_Comm_size(MPI_COMM_WORLD, &nprocs);
    MPI_Comm_rank(MPI_COMM_WORLD, &myrank);

    MPI_Info_create(&finfo);
    displs = DISPLS;
    offset = OFFSET;
    veclen = ELEMENTS;
    elmtcount = 1;
    veccount = 0;
    run_tests = 0;
    cbsize = 0;
    fsize = 0;

#if 0
    /* process 0 takes the file name as a command-line argument and 
       broadcasts it to other processes */
    /* XXX use this code if your MPI implementation does not pass
       cmdline parameters to all processes */
    if (!myrank) {
	i = 1;
	while ((i < argc) && strcmp("-fname", *argv)) {
	    i++;
	    argv++;
	}
	if (i >= argc) {
	    printf("\n*#  Usage: noncontig -fname filename\n\n");
	    MPI_Abort(MPI_COMM_WORLD, 1);
	}
	argv++;
	len = strlen(*argv);
	filename = (char *) malloc(len+1);
	strcpy(filename, *argv);
	MPI_Bcast(&len, 1, MPI_INT, 0, MPI_COMM_WORLD);
	MPI_Bcast(filename, len+1, MPI_CHAR, 0, MPI_COMM_WORLD);
    }
    else {
	MPI_Bcast(&len, 1, MPI_INT, 0, MPI_COMM_WORLD);
	filename = (char *) malloc(len+1);
	MPI_Bcast(filename, len+1, MPI_CHAR, 0, MPI_COMM_WORLD);
    }
#else
    sfact = SFACT;
    ssize = SSIZE;
    ionodes = (char *)malloc(MAX_FNAME_LEN);
    ionodes[0] = '\0';
    filename = (char *)malloc(MAX_FNAME_LEN);
    filename[0] = '\0';

    cmdline_get_string (&argc, argv, 1, "-fname", filename, MAX_FNAME_LEN);
    cmdline_get_string (&argc, argv, 1, "-ionodes", ionodes, MAX_FNAME_LEN);
    cmdline_get_int (&argc, argv, 1, "-fsize", &fsize);
    i = 0;
    cmdline_get_int (&argc, argv, 1, "-bufsize", &i);
    bufsize = i;
    cmdline_get_int (&argc, argv, 1, "-veclen", &veclen);
    cmdline_get_int (&argc, argv, 1, "-elmtcount", &elmtcount);
    cmdline_get_int (&argc, argv, 1, "-veccount", &veccount);
    cmdline_get_int (&argc, argv, 1, "-sfact", &sfact);
    cmdline_get_int (&argc, argv, 1, "-ssize", &ssize);
    cmdline_get_int (&argc, argv, 1, "-cbsize", &cbsize);
    cmdline_get_int (&argc, argv, 1, "-loops", &loops);
    i = DISPLS;
    cmdline_get_int (&argc, argv, 1, "-displs", &i);
    displs = (MPI_Offset)i;
    i = OFFSET;
    cmdline_get_int (&argc, argv, 1, "-offset", &i);
    offset = (MPI_Offset)i;
    if (cmdline_has_name (&argc, argv, 1, "-timing"))
	do_timing = 1;
    if (cmdline_has_name (&argc, argv, 1, "-nosync"))
        do_sync = 0;
    if (cmdline_has_name (&argc, argv, 1, "-coll"))
	do_coll = 1;
    if (cmdline_has_name (&argc, argv, 1, "-v"))
	be_verbose = 1;
    if (cmdline_has_name (&argc, argv, 1, "-help")) {
	print_help(myrank);
	return 0;
    }
    if (cmdline_has_name (&argc, argv, 1, "-ncmem_ncfile"))
	run_tests |= NCMEM_NCFILE;
    if (cmdline_has_name (&argc, argv, 1, "-cmem_ncfile"))
	run_tests |= CMEM_NCFILE;
    if (cmdline_has_name (&argc, argv, 1, "-ncmem_cfile"))
	run_tests |= NCMEM_CFILE;
    if (cmdline_has_name (&argc, argv, 1, "-cmem_cfile"))
	run_tests |= CMEM_CFILE;
    if (cmdline_has_name (&argc, argv, 1, "-all"))
	run_tests = NCMEM_NCFILE|NCMEM_CFILE|CMEM_NCFILE|CMEM_CFILE;

    if (myrank == 0) {

    printf("========= Parameter space dump =========\n");
    printf("filename: %s  ionodes %s\n", filename, ionodes);
    printf("file size (MB): %d buffer size %d \n", fsize, bufsize);
    printf("vector length: %d element count: %d vector count: %d\n", veclen, elmtcount, veccount);
    printf("striping factor: %d striping size: %d collective buffer size: %d\n",
		    sfact, ssize, cbsize);
    printf("loops: %d displacement %Ld\n", loops, displs);
    printf("========= Dump done            =========\n");
    }

    if (strlen(filename) == 0) {
	printf("\n*#  Usage: noncontig -fname filename\n");
	printf("*#         -help will give full list of parameters\n Aborting.\n");
	MPI_Abort(MPI_COMM_WORLD, 1);
    }

    if (nprocs > 2 && veclen % nprocs != 0) {
	if (myrank == 0) 
	    fprintf(stderr, "\n*# -veclen : must be a multiple of number of processes if using > 2 processes.\n Aborting.\n");
	MPI_Abort(MPI_COMM_WORLD, 1);
    }

    if (fsize > 0 && veccount > 0) {
	if (myrank == 0) 
	    fprintf(stderr, "\n*# Only specify one, either '-fsize' or '-veccount'.\n Aborting.\n");
	MPI_Abort(MPI_COMM_WORLD, 1);
    }

    bufsize *= 1024;

    /* set custom file parameters */
    if (ssize > 0) {
	sprintf (tmp, "%d", ssize);
	MPI_Info_set(finfo, "striping_unit", tmp);
    } 
    if (sfact > 0) {
	sprintf (tmp, "%d", sfact);
	MPI_Info_set(finfo, "striping_factor", tmp);
    }
    if (strlen (ionodes) > 0)
	MPI_Info_set(finfo, "io_node_list", ionodes);
#endif
    /* allow odd vector lengths for 2-process runs only */
    if (nprocs == 2)
	elmt_align = (myrank == 0 && veclen % 2 != 0) ? 1 : 0;
    else
	elmt_align = 0;

    do_verify = elmtcount == 1 && bufsize == 0 && nprocs > 1;
    if (!do_verify && !myrank)
	printf("#* no verification possible!\n");

    if (elmtcount > 1)
	MPI_Type_contiguous(elmtcount, MPI_INT, &elmttype);
    else 
	elmttype = MPI_INT;
    stride = nprocs > 1 ? nprocs : 2;
    MPI_Type_vector(veclen/nprocs + elmt_align, 1, stride, elmttype, &typevec);

    b[0] = b[1] = b[2] = 1;
    d[0] = 0;
    d[1] = myrank*elmtcount*sizeof(int);
    d[2] = (veclen + elmt_align)*elmtcount*sizeof(int);
    t[0] = MPI_LB;
    t[1] = typevec;
    t[2] = MPI_UB;

    MPI_Type_struct(3, b, d, t, &newtype);
    MPI_Type_commit(&newtype);
    if (elmtcount > 1)
	MPI_Type_free(&elmttype);
    MPI_Type_free(&typevec);

#if !USE_DEFAULT_BUFSIZES
    /* I am setting these info values for testing purposes only. It is
       better to use the default values in practice. */
    MPI_Info_set(finfo, "ind_rd_buffer_size", "32");
    MPI_Info_set(finfo, "ind_wr_buffer_size", "32");
#endif

    if (cbsize > 0) {
	char cbstring[20];

	sprintf(cbstring, "%d", cbsize*1024);
	MPI_Info_set(finfo, "cb_buffer_size", cbstring);
    }

    MPI_Info_set(finfo, "romio_pvfs_listio_read", "enable");
    MPI_Info_set(finfo, "romio_pvfs_listio_write", "enable");

    if (run_tests == 0)
	run_tests = RUN_TESTS;

    if (be_verbose && myrank == 0) {
	MPI_Aint vecext;
	int vecsize;

	printf ("# performance test for non-contiguous file access\n");
	printf ("# filename: %s - nbr of procs: %d - loops: %d\n", 
		filename, nprocs, loops);
	if (fsize > 0) 
	    printf ("# filesize: %dMB\n", fsize);
	if (bufsize > 0) 
	    printf ("# buffer size: %dKB\n", bufsize/1024);
	printf ("# vector: blocklen = %d bytes,  stride = %d bytes, elmts = %d\n", 
		elmtcount*sizeof(int), elmtcount*sizeof(int)*stride, veccount);
	MPI_Type_size (newtype, &vecsize);
	MPI_Type_extent (newtype, &vecext);
	printf ("#         extent = %d bytes - size = %d bytes\n", vecext, vecsize);
	if (cbsize > 0 && do_coll) 
	    printf ("# size of two-phase-buffer for collective access: %dKB\n", cbsize);
	fflush (stdout);
    }

    if (run_tests & NCMEM_NCFILE) {
	if (fsize > 0) 
	    veccount = (fsize*MB)/(veclen*elmtcount*sizeof(int));
	buf = getbuf(&bufsize, veccount, veclen, elmtcount);
	nbr_errors = noncontigmem_noncontigfile(filename, buf, bufsize, 
		newtype, offset, displs, finfo, veclen, elmtcount, veccount);
	if (nbr_errors > 0) 
	    printf("[%d] non-contiguous in memory, non-contiguous in file: %d errors!\n", myrank, nbr_errors);
	fflush (stdout);
	free(buf);
    }

    if (run_tests & NCMEM_CFILE) {
	if (fsize > 0) 
	    veccount = (fsize*MB)/(veclen*elmtcount*sizeof(int));
	buf = getbuf(&bufsize, veccount, veclen, elmtcount);
	nbr_errors = noncontigmem_contigfile(filename, buf, bufsize, newtype, 
			offset, displs, finfo, veclen, elmtcount, veccount);
	if (nbr_errors > 0) 
	    printf("[%d] non-contiguous in memory, contiguous in file: %d errors!\n", myrank, nbr_errors);
	fflush (stdout);
	free(buf);
    }
    if (run_tests & CMEM_NCFILE) {
	if (fsize > 0) 
	    veccount = (fsize*MB)/(nprocs*veclen*elmtcount*sizeof(int));
	buf = getbuf(&bufsize, veccount, veclen, elmtcount);
	nbr_errors = contigmem_noncontigfile(filename, buf, bufsize, newtype, 
		offset, displs, finfo, veclen/nprocs, elmtcount, veccount);
	if (nbr_errors > 0) 
	    printf("[%d] contiguous in memory, non-contiguous in file: %d errors!\n", myrank, nbr_errors);
	fflush (stdout);
	free (buf);
    }

    if (run_tests & CMEM_CFILE) {
	if (fsize > 0) 
	    veccount = (fsize*MB)/(stride*veclen*elmtcount*sizeof(int));
	buf = getbuf(&bufsize, veccount, veclen, elmtcount);
	nbr_errors = contigmem_contigfile(filename, buf, bufsize, newtype, 
			offset, displs, finfo, veclen, elmtcount, veccount);
	if (nbr_errors > 0) 
	    printf("[%d] contiguous in memory, contiguous in file: %d errors!\n", myrank, nbr_errors);
	fflush (stdout);
	free (buf);
    }

    MPI_Type_free(&newtype);
    MPI_Info_free(&finfo);
    free(filename);

    MPI_Finalize();
    return 0;
}
