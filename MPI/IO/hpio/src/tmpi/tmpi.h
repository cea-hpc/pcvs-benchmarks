/* (C) Northwestern University
 * See COPYING in the top-level directory . */

#ifndef TMPI_H
#define TMPI_H

int tMPI_Pack(void *inbuf, int incount, MPI_Datatype datatype,
	      void *outbuf, int outsize, int *position, MPI_Comm comm);
int tMPI_Unpack(void *inbuf, int insize, int *position, void *outbuf,
		int outcount, MPI_Datatype datatype, MPI_Comm comm);
int tMPI_File_delete(char *filename, MPI_Info info);
int tMPI_File_open(MPI_Comm comm, char *filename, int amode,
		   MPI_Info info, MPI_File *fh);
int tMPI_File_set_view(MPI_File fh, MPI_Offset disp, MPI_Datatype etype, 
		       MPI_Datatype filetype, char *datarep, MPI_Info info);
int tMPI_File_read(MPI_File fh, void *buf, int count, 
		   MPI_Datatype datatype, MPI_Status *status);
int tMPI_File_read_all(MPI_File fh, void *buf, int count, 
		       MPI_Datatype datatype, MPI_Status *status);
int tMPI_File_write(MPI_File fh, void *buf, int count, 
		    MPI_Datatype datatype, MPI_Status *status);
int tMPI_File_write_all(MPI_File fh, void *buf, int count, 
			MPI_Datatype datatype, MPI_Status *status);
int tMPI_File_seek(MPI_File fh, MPI_Offset offset, int whence);
int tMPI_File_sync(MPI_File fh);
int tMPI_File_close(MPI_File *fh);

#endif

/*
 * Local variables:
 *  mode: c
 *  c-indent-level: 4
 *  c-basic-offset: 4
 * End:
 *
 * vim: ft=c ts=8 sts=4 sw=4 expandtab
 */
