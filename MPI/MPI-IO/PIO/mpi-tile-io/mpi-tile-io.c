/* mpi-tile-io.c
 *
 * Performs noncontiguous MPI I/O.  See README for details.
 *
 * Copyright (C) 2001 University of Chicago.  See COPYRIGHT notice in
 * top-level directory.
 */

#include <getopt.h>
#include <unistd.h>
#ifdef HAVE_GETOPT_LONG
#endif
#include <mpi.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/types.h>



/* default values for runtime parameters */
enum {
    DEFAULT_NR_TILES_X = 1,
    DEFAULT_NR_TILES_Y = 1,
    DEFAULT_SZ_TILE_X = 1024,
    DEFAULT_SZ_TILE_Y = 1024,
    DEFAULT_SZ_ELEMENT = 3,
    DEFAULT_NR_FILES = 1,
    DEFAULT_OVERLAP_X = 0,
    DEFAULT_OVERLAP_Y = 0,
    DEFAULT_SZ_COLL_BUF = -1,
    DEFAULT_SZ_INDEP_BUF = -1,
    DEFAULT_COLLECTIVE = 0,
    DEFAULT_HEADER_BYTES = 0,
    DEFAULT_WRITE_FILE = 0,
};

#ifdef HAVE_GETOPT_LONG
/* values returned by getopt_long */
enum {
    OPT_NR_TILES_X = 1,
    OPT_NR_TILES_Y,
    OPT_SZ_TILE_X,
    OPT_SZ_TILE_Y,
    OPT_SZ_ELEMENT,
    OPT_NR_FILES,
    OPT_OVERLAP_X,
    OPT_OVERLAP_Y,
    OPT_SZ_COLL_BUF,
    OPT_SZ_INDEP_BUF,
    OPT_COLLECTIVE,
    OPT_FILENAME,
    OPT_HEADER_BYTES,
    OPT_WRITE_FILE,
};
#endif

#ifdef HAVE_GETOPT_LONG
struct option longopts[] = {
    { "nr_tiles_x", 1, NULL, OPT_NR_TILES_X },
    { "nr_tiles_y", 1, NULL, OPT_NR_TILES_Y },
    { "sz_tile_x", 1, NULL, OPT_SZ_TILE_X },
    { "sz_tile_y", 1, NULL, OPT_SZ_TILE_Y },
    { "sz_element", 1, NULL, OPT_SZ_ELEMENT },
    { "nr_files", 1, NULL, OPT_NR_FILES },
    { "overlap_x", 1, NULL, OPT_OVERLAP_X },
    { "overlap_y", 1, NULL, OPT_OVERLAP_Y },
    { "sz_coll_buf", 1, NULL, OPT_SZ_COLL_BUF },
    { "sz_indep_buf", 1, NULL, OPT_SZ_INDEP_BUF },
    { "collective", 0, NULL, OPT_COLLECTIVE },
    { "filename", 1, NULL, OPT_FILENAME },
    { "header_bytes", 1, NULL, OPT_HEADER_BYTES },
    { "write_file", 0, NULL, OPT_WRITE_FILE },
    { NULL, 0, NULL, 0 },
};
#endif


/* globals holding configuration parameters */
int nr_tiles_x = DEFAULT_NR_TILES_X, 
    nr_tiles_y = DEFAULT_NR_TILES_Y;
int sz_tile_x = DEFAULT_SZ_TILE_X, 
    sz_tile_y = DEFAULT_SZ_TILE_Y;
int sz_element = DEFAULT_SZ_ELEMENT;
int nr_files = DEFAULT_NR_FILES;
int overlap_x = DEFAULT_OVERLAP_X, 
    overlap_y = DEFAULT_OVERLAP_Y;
int sz_coll_buf = DEFAULT_SZ_COLL_BUF, 
    sz_indep_buf = DEFAULT_SZ_INDEP_BUF;
int collective = DEFAULT_COLLECTIVE,
    write_file = DEFAULT_WRITE_FILE;
char *filename = NULL;
int header_bytes = DEFAULT_HEADER_BYTES;

#ifdef HAVE_GETOPT_LONG
void parse_args(int argc, 
		char **argv);
#endif
void parse_env(void);
void get_time_dist(MPI_Comm comm, 
		   double *time, 
		   double *min, 
		   double *max, 
		   double *mean, 
		   double *var);
void report_results(MPI_Comm comm,
		    double open_time,
		    double io_time,
		    double close_time,
		    int64_t total_size,
		    int print);
void report_parameters(int nr_procs, int nr_procs_used, int *sz_dataset);
void pass_parameters(void);

int main(int argc, 
	 char **argv)
{
    int i, ret, nr_procs, nr_procs_used, my_rank;
    int sz_dataset[2], sz_tile[2], off_tile[2], nr_tiles[2], 
	is_periodic[2], my_tile[2];
    char *buffer;

    MPI_Datatype el_type;
    MPI_Datatype tile_type;
    MPI_Comm cart_comm;
    MPI_File fh;
    MPI_Info info;
    MPI_Status status;

    /* values used for performance gathering/reporting */
    double open_time = 0.0, io_time = 0.0, close_time = 0.0, 
	start_time, end_time;
    int64_t total_sz;

    /* mpi init */
    MPI_Init(&argc, &argv);
    MPI_Comm_size(MPI_COMM_WORLD, &nr_procs);
    MPI_Comm_rank(MPI_COMM_WORLD, &my_rank);

    /* get command line parameters on process 0 */
    if (my_rank == 0) {
	parse_env();
#ifdef HAVE_GETOPT_LONG
	parse_args(argc, argv);
#endif
	if (filename == NULL) fprintf(stderr, 
			              "no filename specified...exiting\n");
    }

    /* do general checking of command line parameters */

    /* pass out command line parameters to everyone */
    pass_parameters();
    
    if (filename == NULL) {
	MPI_Finalize();
	return 1;
    }
    
    if (nr_procs < nr_tiles_x * nr_tiles_y) {
	fprintf(stderr, "too few processes (%d) for number of tiles (%d)\n",
		nr_procs, nr_tiles_x * nr_tiles_y);
	MPI_Finalize();
	return 1;
    }

    /* basically we're going to drop any extra processes here, then we're
     * going to use MPI_Cart_create() to allow MPI to dole out the tiles.
     * theoretically the MPI implementation can place our processes in some 
     * more optimal way than we can by just splitting on rank.
     *
     * anyway, it's an excuse for me to try out the function :).
     */

    /* note that the order is reversed; this seems to be the right thing */
    nr_tiles[1] = nr_tiles_x;
    nr_tiles[0] = nr_tiles_y;
    is_periodic[1] = 0;
    is_periodic[0] = 0;

    if (nr_procs > nr_tiles_x * nr_tiles_y) {
	MPI_Comm tmp_comm;
	nr_procs_used = nr_tiles_x * nr_tiles_y;

	/* get the unused procs out of my communicator */
	if (my_rank < nr_procs_used) {
	    MPI_Comm_split(MPI_COMM_WORLD, 0, my_rank, &tmp_comm);
	}
	else {
	    /* get rid of the extras */
	    MPI_Comm_split(MPI_COMM_WORLD, 1, my_rank, &tmp_comm);
	    MPI_Finalize();
	    return 0;
	}

	MPI_Cart_create(tmp_comm, 2, nr_tiles, is_periodic, 1, &cart_comm);
	MPI_Comm_free(&tmp_comm);
    }
    else {
	/* just the right number of processes */
	nr_procs_used = nr_procs;
	MPI_Cart_create(MPI_COMM_WORLD, 2, nr_tiles, is_periodic, 1, 
			&cart_comm);
    }

    /* get my coordinates */
    MPI_Cart_get(cart_comm, 2, nr_tiles, is_periodic, my_tile);

    sz_dataset[1] = ((nr_tiles_x * sz_tile_x) - ((nr_tiles_x-1) * overlap_x));
    sz_dataset[0] = ((nr_tiles_y * sz_tile_y) - ((nr_tiles_y-1) * overlap_y));

    /* check that our file(s) are big enough, that everyone can get to them */

    /* print out summary of parameters */
    if (my_rank == 0) report_parameters(nr_procs, nr_procs_used, sz_dataset);

    /* get a buffer */
    buffer = malloc(sz_tile_x * sz_tile_y * sz_element);
    if (buffer == NULL) {
	MPI_Finalize();
	return 1;
    }
    
    /* touch all the pages */
    for (i = 0; i < sz_tile_x * sz_tile_y * sz_element; i += 4096) {
	buffer[i] = (char) i;
    }

    /* Q: should we align our buffer? */

    /* build datatypes */
    MPI_Type_contiguous(sz_element, MPI_BYTE, &el_type);
    MPI_Type_commit(&el_type);

    sz_tile[1] = sz_tile_x;
    sz_tile[0] = sz_tile_y;

    if (my_tile[1] == 0) off_tile[1] = 0;
    else off_tile[1] = my_tile[1] * (sz_tile_x - overlap_x);

    if (my_tile[0] == 0) off_tile[0] = 0;
    else off_tile[0] = my_tile[0] * (sz_tile_y - overlap_y);

#if 0
    printf("my rank = %d, my coords = (%d,%d), my offset = (%d,%d)\n", 
	   my_rank, my_tile[1], my_tile[0], off_tile[1], off_tile[0]);
#endif

    MPI_Type_create_subarray(2, sz_dataset, sz_tile, off_tile, MPI_ORDER_C,
			     el_type, &tile_type);
    MPI_Type_commit(&tile_type);

    /* build info */
    MPI_Info_create(&info);

    /* foreach file */
    for (i=0; i < nr_files; i++) {
	int mode;

	if (write_file) {
	    /* create a new file */
	    if (my_rank == 0) MPI_File_delete(filename, MPI_INFO_NULL);
	    mode = MPI_MODE_WRONLY | MPI_MODE_CREATE;
	}
	else mode = MPI_MODE_RDONLY;

	/* barrier to try to sync processes */
	MPI_Barrier(cart_comm);

        /* open file */
	start_time = MPI_Wtime();
	if (collective) {
	    ret = MPI_File_open(cart_comm, filename, mode, info, &fh);
	}
	else {
	    ret = MPI_File_open(MPI_COMM_SELF, filename, mode, info, &fh);
	}
	if (ret != MPI_SUCCESS) {
	    fprintf(stderr, "failed during MPI_File_open\n");
	    MPI_Finalize();
	    return 1;
	}

	end_time = MPI_Wtime();
	open_time += end_time - start_time;

	if (ret != 0) {
	    MPI_Finalize();
	    return 1;
	}
	
	/* set view */
	MPI_File_set_view(fh, header_bytes, el_type, tile_type, "native", 
			  MPI_INFO_NULL);

	/* do I/O */
	/* barrier to try to sync processes */
	MPI_Barrier(cart_comm);

	start_time = MPI_Wtime();
	if (write_file) {
	    if (collective) {
		ret = MPI_File_write_all(fh, buffer, sz_tile_x * sz_tile_y, 
					 el_type, &status);
	    }
	    else {
		ret = MPI_File_write(fh, buffer, sz_tile_x * sz_tile_y, 
				     el_type, &status);
	    }
	}
	else {
	    /* read */
	    if (collective) {
		ret = MPI_File_read_all(fh, buffer, sz_tile_x * sz_tile_y, 
					el_type, &status);
	    }
	    else {
		ret = MPI_File_read(fh, buffer, sz_tile_x * sz_tile_y, 
				    el_type, &status);
	    }
	}
	end_time = MPI_Wtime();
	if (ret != MPI_SUCCESS) {
	    fprintf(stderr, "failed during MPI_File_(read or write)\n");
	    /* MUST close before finalize according to MPI specs */
	    MPI_File_close(&fh);
	    MPI_Finalize();
	    return 1;
	}
	io_time += end_time - start_time;

	/* check return value */

	if (write_file) {
	    /* sync - not needed for read tests */
	    MPI_File_sync(fh);
	}

	/* close file */
	/* barrier to try to sync processes */
	MPI_Barrier(cart_comm);

	start_time = MPI_Wtime();
	MPI_File_close(&fh);
	end_time = MPI_Wtime();
	close_time += end_time - start_time;
    }

    /* report stats (last parameter indicates process should print) */
    total_sz = (int64_t) nr_files * (int64_t) sz_tile_x *
	       (int64_t) sz_tile_y * (int64_t) nr_tiles_x *
	       (int64_t) nr_tiles_y * (int64_t) sz_element;

    if (my_rank == 0) 
	report_results(cart_comm, open_time, io_time, close_time, total_sz, 1);
    else
	report_results(cart_comm, open_time, io_time, close_time, total_sz, 0);

    MPI_Finalize();
    return 0;
}

#ifdef HAVE_GETOPT_LONG
/* parse_args()
 */
void parse_args(int argc, 
		char **argv)
{
    int ret, len;

  MPIO_lock_shared();
  
  optarg = NULL;
  optind = 0;
  opterr = 0;
  optopt = 0;

    while ((ret = getopt_long(argc, argv, "", longopts, NULL)) >= 0)
    {
	switch (ret) {
	case OPT_NR_TILES_X:
	    nr_tiles_x = atoi(optarg);
	    break;
	case OPT_NR_TILES_Y:
	    nr_tiles_y = atoi(optarg);
	    break;
	case OPT_SZ_TILE_X:
	    sz_tile_x = atoi(optarg);
	    break;
	case OPT_SZ_TILE_Y:
	    sz_tile_y = atoi(optarg);
	    break;
	case OPT_SZ_ELEMENT:
	    sz_element = atoi(optarg);
	    break;
	case OPT_NR_FILES:
	    fprintf(stderr, "warning: nr_files option not enabled\n");
	    nr_files = 1;
	    break;
	case OPT_OVERLAP_X:
	    overlap_x = atoi(optarg);
	    break;
	case OPT_OVERLAP_Y:
	    overlap_y = atoi(optarg);
	    break;
	case OPT_SZ_COLL_BUF:
	    fprintf(stderr, "warning: sz_coll_buf option not enabled\n");
	    sz_coll_buf = atoi(optarg);
	    break;
	case OPT_SZ_INDEP_BUF:
	    fprintf(stderr, "warning: sz_indep_buf option not enabled\n");
	    sz_indep_buf = atoi(optarg);
	    break;
	case OPT_COLLECTIVE:
	    collective = 1;
	    break;
	case OPT_WRITE_FILE:
	    write_file = 1;
	    break;
	case OPT_FILENAME:
	    if (filename != NULL) {
		/* was defined in an env. variable; over-riding */
		free(filename);
	    }
	    if (optarg == NULL) {
		fprintf(stderr, "error: no argument to filename option\n");
		break;
	    }
	    len = strlen(optarg);
	    filename = malloc(len + 1);
	    if (filename == NULL) {
		MPI_Finalize();
		exit(1);
	    }
	    strcpy(filename, optarg);
	    break;
	case OPT_HEADER_BYTES:
	    header_bytes = atoi(optarg);
	    break;
	default:
	    fprintf(stderr, "warning: invalid option ignored\n");
	    break;
	}
    }
    
    
    MPIO_unlock_shared();
}
#endif

/* parse_env() - pulls runtime parameters out of environment variables
 */
void parse_env(void)
{
    int len;
    char *envarg;
    
    if ((envarg = getenv("NR_TILES_X")) != NULL)
    	nr_tiles_x = atoi(envarg);
    if ((envarg = getenv("NR_TILES_Y")) != NULL)
    	nr_tiles_y = atoi(envarg);
    if ((envarg = getenv("SZ_TILE_X")) != NULL)
    	sz_tile_x = atoi(envarg);
    if ((envarg = getenv("SZ_TILE_Y")) != NULL)
    	sz_tile_y = atoi(envarg);
    if ((envarg = getenv("SZ_ELEMENT")) != NULL)
    	sz_element = atoi(envarg);
    if ((envarg = getenv("OVERLAP_X")) != NULL)
	overlap_x = atoi(envarg);
    if ((envarg = getenv("OVERLAP_Y")) != NULL)
	overlap_y = atoi(envarg);
    if ((envarg = getenv("SZ_COLL_BUF")) != NULL)
	sz_coll_buf = atoi(envarg);
    if ((envarg = getenv("SZ_INDEP_BUF")) != NULL)
	sz_indep_buf = atoi(envarg);
    if ((envarg = getenv("COLLECTIVE")) != NULL)
	collective = atoi(envarg);
    if ((envarg = getenv("HEADER_BYTES")) != NULL)
	header_bytes = atoi(envarg);
    if ((envarg = getenv("WRITE_FILE")) != NULL)
	write_file = atoi(envarg);
    if ((envarg = getenv("FILENAME")) != NULL) {
	len = strlen(envarg);
	filename = malloc(len + 1);
	if (filename == NULL) {
	    MPI_Finalize();
	    exit(1);
	}
	strcpy(filename, envarg);
    }
}

/* pass_parameters() - passes all important parameters from process 0 of
 *                     MPI_COMM_WORLD out to the rest of the processes
 */
void pass_parameters(void)
{
    int parameters[13];
    int my_rank;

    MPI_Comm_rank(MPI_COMM_WORLD, &my_rank);

    /* basically we're going to pack all the int parameters into an array for
     * easy broadcast.  we'll put the size of our string (filename) in there
     * too.  then we'll handle actually passing the string separately.
     */
    if (my_rank == 0) {
	parameters[0] = nr_tiles_x;
	parameters[1] = nr_tiles_y;
	parameters[2] = sz_tile_x;
	parameters[3] = sz_tile_y;
	parameters[4] = sz_element;
	parameters[5] = overlap_x;
	parameters[6] = overlap_y;
	parameters[7] = sz_coll_buf;
	parameters[8] = sz_indep_buf;
	parameters[9] = collective;
	parameters[10] = header_bytes;
	parameters[11] = write_file;
	if (filename) parameters[12] = strlen(filename) + 1;
	else parameters[12] = 0;
    }

    MPI_Bcast(parameters, 13, MPI_INT, 0, MPI_COMM_WORLD);
    
    /* everyone but process 0 needs to allocate a filename */
    if (my_rank != 0 && parameters[12]) filename = malloc(parameters[12]);

    /* exit now if we don't have a filename */
    if (filename == NULL) {
	MPI_Finalize();
	exit(1);
    }

    /* now that everyone knows the size, pass the filename */
    MPI_Bcast(filename, parameters[12], MPI_CHAR, 0, MPI_COMM_WORLD);

    if (my_rank != 0) {
	nr_tiles_x = parameters[0];
	nr_tiles_y = parameters[1];
	sz_tile_x = parameters[2];
	sz_tile_y = parameters[3];
	sz_element = parameters[4];
	overlap_x = parameters[5];
	overlap_y = parameters[6];
	sz_coll_buf = parameters[7];
	sz_indep_buf = parameters[8];
	collective = parameters[9];
	header_bytes = parameters[10];
	write_file = parameters[11];
    }
}

/* report_parameters() - prints a summary of parameters used in test
 */
void report_parameters(int nr_procs, 
		       int nr_procs_used, 
		       int *sz_dataset)
{
    int sz_procname;
    int64_t sz_file, sz_file_mb;
    char procname[MPI_MAX_PROCESSOR_NAME];

    sz_file = (int64_t) sz_dataset[0] * (int64_t) sz_dataset[1] 
	* (int64_t) sz_element;
    sz_file_mb = sz_file / (int64_t) 1048576;

    /* print out summary of app run parameters */
    MPI_Get_processor_name(procname, &sz_procname);
    printf("# mpi-tile-io run on %s\n", procname);
    printf("# %d process(es) available, %d used\n", nr_procs, nr_procs_used);
    printf("# filename: %s\n", filename);
    printf("# collective I/O %s\n", collective ? "on" : "off");
    printf("# %d byte header\n", header_bytes);
    printf("# %d x %d element dataset, %d bytes per element\n", 
	   sz_dataset[1], sz_dataset[0], sz_element);
    printf("# %d x %d tiles, each tile is %d x %d elements\n",
	   nr_tiles_x, nr_tiles_y, sz_tile_x, sz_tile_y);
    printf("# tiles overlap by %d elements in X, %d elements in Y\n",
	   overlap_x, overlap_y);
    if (sz_file_mb == 0) {
	printf("# total file size is %d bytes, %d file(s) total.\n",
	       (int) sz_file, nr_files);
    }
    else {
	printf("# total file size is ~%.2f Mbytes, %d file(s) total.\n",
	       (float) sz_file_mb, nr_files);
    }
}

/* report_times() - uses get_time_dist() to gather timing information,
 *                  then prints the information if the print parameter
 *                  is non-zero.
 *
 * NOTE: This is a collective operation across all members of the
 * communicator.
 */
void report_results(MPI_Comm comm,
		    double open_time,
		    double io_time,
		    double close_time,
		    int64_t total_size,
		    int print)
{
    double min_time, max_time, mean_time, var_time, io_bw;

    get_time_dist(comm, &open_time, &min_time, &max_time, &mean_time,
		  &var_time);
    if (print) {
	printf("# Times are total for all operations of the given type\n");
	printf("# Open: min_t = %f, max_t = %f, mean_t = %f, var_t = %f\n",
	       min_time, max_time, mean_time, var_time);
    }
    get_time_dist(comm, &io_time, &min_time, &max_time, &mean_time,
		  &var_time);
    if (print) {
	if (write_file) 
	    printf("# Write: min_t = %f, max_t = %f, mean_t = %f, var_t = %f\n",
		   min_time, max_time, mean_time, var_time);
	else
	    printf("# Read: min_t = %f, max_t = %f, mean_t = %f, var_t = %f\n",
		   min_time, max_time, mean_time, var_time);

	/* calculate the I/O bandwidth while we still have the max. time;
	 * avoid floating point exceptions
	 */
	total_size = total_size / (int64_t) 1048576;
	if (max_time != 0.0)
	    io_bw = ((double) total_size) / max_time;
	else
	    io_bw = 0.0;
    }
    get_time_dist(comm, &close_time, &min_time, &max_time, &mean_time,
		  &var_time);
    if (print) {
	printf("# Close: min_t = %f, max_t = %f, mean_t = %f, var_t = %f\n",
	       min_time, max_time, mean_time, var_time);
	printf("# Note: bandwidth values based on max_t (worst case)\n");

	if (write_file)
	    printf("Write Bandwidth = %.3f Mbytes/sec\n", (float) io_bw);
	else
    	    printf("Read Bandwidth = %.3f Mbytes/sec\n", (float) io_bw);
    }
}

/* get_time_dist() - perform allreduces necessary to gather statistics
 *                   on a single double value recorded across a comm.
 *
 */
void get_time_dist(MPI_Comm comm, 
		   double *time, 
		   double *min, 
		   double *max, 
		   double *mean, 
		   double *var)
{
    int nr_procs;
    double sq_time_part, sum, sum_sq;
    
    MPI_Comm_size(comm, &nr_procs);

    MPI_Allreduce(time, max, 1, MPI_DOUBLE, MPI_MAX, comm);
    MPI_Allreduce(time, min, 1, MPI_DOUBLE, MPI_MIN, comm);
    MPI_Allreduce(time, &sum, 1, MPI_DOUBLE, MPI_SUM, comm);

    *mean = sum / nr_procs;

    /* calculate our part of the variance */
    sq_time_part = *time - *mean;
    sq_time_part = sq_time_part * sq_time_part;
    MPI_Allreduce(&sq_time_part, &sum_sq, 1, MPI_DOUBLE, MPI_SUM, comm);

    if (nr_procs > 1)
	*var = sum_sq / ((double) nr_procs - 1.0);
    else 
	*var = 0.0;

    return;
}

