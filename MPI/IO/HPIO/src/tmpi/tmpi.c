#include <stdio.h>
#include <stdlib.h>
#include "mpi.h"
#include "tmpi.h"

int tMPI_Pack(void *inbuf, int incount, MPI_Datatype datatype,
	      void *outbuf, int outsize, int *position, MPI_Comm comm)
{
    int err = -1;
    err = MPI_Pack(inbuf, incount, datatype,
		   outbuf, outsize, position, comm);
    if (err != MPI_SUCCESS)
	fprintf(stderr, "MPI_Pack failed\n");
    return err;
}

int tMPI_Unpack(void *inbuf, int insize, int *position, void *outbuf,
		int outcount, MPI_Datatype datatype, MPI_Comm comm)
{
    int err = -1;
    err = MPI_Unpack(inbuf, insize, position, outbuf,
		     outcount, datatype, comm);
    if (err != MPI_SUCCESS)
	fprintf(stderr, "MPI_Unpack failed\n");
    return err;
}

int tMPI_File_delete(char *filename, MPI_Info info)
{
    int err = -1;
    err = MPI_File_delete(filename, info);
    if (err == MPI_ERR_NO_SUCH_FILE)
	fprintf(stderr, "MPI_File_delete: File %s does not exist\n",
		filename);
    else if (err != MPI_SUCCESS)
	fprintf(stderr, "MPI_File_delete failed\n");
    return err;
}

int tMPI_File_open(MPI_Comm comm, char *filename, int amode,
		   MPI_Info info, MPI_File *fh)
{
    int err = -1;
    err = MPI_File_open(comm, filename, amode, info, fh);
    if (err != MPI_SUCCESS)
	fprintf(stderr, "MPI_File_open failed\n");
    return err;
}

int tMPI_File_set_view(MPI_File fh, MPI_Offset disp, MPI_Datatype etype, 
		       MPI_Datatype filetype, char *datarep, MPI_Info info)
{
    int err = -1;
    err = MPI_File_set_view(fh, disp, etype, filetype, datarep, info);
    if (err != MPI_SUCCESS)
	fprintf(stderr, "MPI_File_set_view failed\n");
    return err;
}

int tMPI_File_read(MPI_File fh, void *buf, int count, 
                  MPI_Datatype datatype, MPI_Status *status)
{
    int err = -1;
    err = MPI_File_read(fh, buf, count, datatype, status);
    if (err != MPI_SUCCESS)
	fprintf(stderr, "MPI_File_read failed\n");
    return err;
}

int tMPI_File_read_all(MPI_File fh, void *buf, int count, 
		       MPI_Datatype datatype, MPI_Status *status)
{
    int err = -1;
    err = MPI_File_read_all(fh, buf, count, datatype, status);
    if (err != MPI_SUCCESS)
	fprintf(stderr, "MPI_File_read_all failed\n");
    return err;
}

int tMPI_File_write(MPI_File fh, void *buf, int count, 
		    MPI_Datatype datatype, MPI_Status *status)
{
    int err = -1;
    err = MPI_File_write(fh, buf, count, datatype, status);
    if (err != MPI_SUCCESS)
	fprintf(stderr, "MPI_File_write failed\n");
    return err;
}

int tMPI_File_write_all(MPI_File fh, void *buf, int count, 
			MPI_Datatype datatype, MPI_Status *status)
{
    int err = -1;
    err = MPI_File_write_all(fh, buf, count, datatype, status);
    if (err != MPI_SUCCESS)
	fprintf(stderr, "MPI_File_write_all failed\n");
    return err;
}

int tMPI_File_seek(MPI_File fh, MPI_Offset offset, int whence)
{
    int err = -1;
    err = MPI_File_seek(fh, offset, whence);
    if (err != MPI_SUCCESS)
	fprintf(stderr, "MPI_File_seek failed\n");
    return err;
}

int tMPI_File_sync(MPI_File fh)
{
    int err = -1;
    err = MPI_File_sync(fh);
    if (err != MPI_SUCCESS)
	fprintf(stderr, "MPI_File_sync failed\n");
    return err;
}

int tMPI_File_close(MPI_File *fh)
{
    int err = -1;
    err = MPI_File_close(fh);
    if (err != MPI_SUCCESS)
	fprintf(stderr, "MPI_File_close failed\n");
    return err;
}

