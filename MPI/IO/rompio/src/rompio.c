/*************************************************************************
* Copyright (c) 2007, Los Alamos National Security, LLC
* All rights reserved.
*  Copyright 2007. Los Alamos National Security, LLC. This software was
*  produced under U.S. Government contract DE-AC52-06NA25396 for 
*  Los Alamos National Laboratory (LANL), which is operated by Los Alamos
*  National Security, LLC for the U.S. Department of Energy. The U.S. 
*  Government has rights to use, reproduce, and distribute this software. 
*  NEITHER THE GOVERNMENT NOR LOS ALAMOS NATIONAL SECURITY, LLC MAKES 
*  ANY WARRANTY, EXPRESS OR IMPLIED, OR ASSUMES ANY LIABILITY FOR THE
*  USE OF THIS SOFTWARE.  If software is modified to produce derivative
*  works, such modified software should be clearly marked, so as not to
*  confuse it with the version available from LANL.
* 
*  Additionally, redistribution and use in source and binary forms, 
*  with or without modification, are permitted provided that the 
*  following conditions are met:
*    o Redistributions of source code must retain the above 
*      copyright notice, this list of conditions and the 
*      following disclaimer. 
*    o Redistributions in binary form must reproduce the above 
*      copyright notice, this list of conditions and the following
*      disclaimer in the documentation and/or other materials provided
*      with the distribution. 
*    o Neither the name of Los Alamos National Security, LLC, 
*      Los Alamos National Laboratory, LANL, the U.S. Government, nor 
*      the names of its contributors may be used to endorse or 
*      promote products derived from this software without
*      specific prior written permission. 
* 
*     THIS SOFTWARE IS PROVIDED BY LOS ALAMOS NATIONAL SECURITY, LLC
*     AND CONTRIBUTORS "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES,
*     INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF
*     MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
*     DISCLAIMED. IN NO EVENT SHALL LOS ALAMOS NATIONAL SECURITY, LLC
*     OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
*     SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT
*     NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
*     LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER
*     CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT,
*     STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE)
*     ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF
*     ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
*************************************************************************/

/************************************************************/
/* Stephen W. Hodson, HPC-5, HPC Systems Integration Group, */
/* Los Alamos National Laboratory, February, 2007           */
/************************************************************/

#define _XOPEN_SOURCE 500
#define _LARGEFILE_SOURCE 1
#define _LARGEFILE64_SOURCE 1
#define _FILE_OFFSET_BITS 64
 
#include <unistd.h>
#include <stdio.h>
#include <stdlib.h>
#include <mpi.h>
#include <time.h>
#include <sys/fcntl.h>

/* For debugging in the deep depths of the romio software stack */
#include "rompio.h"

/* High-jacked from ADIOI_GEN_WriteStridedColl to pass dynamic value for write index */
/* This is global only for info purposes to rompio                                      */
int idx_write;

#define O_CONCURRENT_WRITE   020000000000

/*******************************************************************************/
/* Routine to test MPI collective, independent, and posix file i/o performance */
/* Stephen W. Hodson, CCN-9 HPC Systems Integration Group, LANL, October, 2005 */
/*******************************************************************************/

main(int argc,char *argv[])
{
  
  MPI_Info info      = MPI_INFO_NULL;      
  MPI_Info def_info  = MPI_INFO_NULL;  /* Default MPI_info struct     */
  MPI_Status status; 
  MPI_File fh = NULL;       
  MPIO_Request *io_request;
  MPI_Status   *io_status;
  char fnamew[MPI_MAX_PROCESSOR_NAME];
  char fnamer[MPI_MAX_PROCESSOR_NAME];
  char ctmp  [MPI_MAX_PROCESSOR_NAME];
  int open_flags, fd = -1;
  char key[200], value[200];
  
  int numpes, my_rank   = -1;        
  int i,j,k,flag,nkeys  = 0;
  int num_writes        = 0;
  int obj_size_min      = 0;     
  int obj_size_max      = 0;     
  int obj_size_inc      = 0;     
  int num_obj_sizes     = 0;
  int sync_write_flag   = 0;
  int sync_close_flag   = 0;
  int barr_write_flag   = 0;
  int allg_write_flag   = 0;
  int atoa_write_flag   = 0;
  int sleep_write_flag  = 0;
  int ared_write_flag   = 0;
  int cb_nodes          = 0;
  int cb_buffer_size    = 0;
  int posix_write_flag  = 0;
  int posix_read_flag   = 0;
  int coll_write_flag   = 0;
  int coll_read_flag    = 0;
  int mpio_write_flag   = 0;
  int mpio_read_flag    = 0;
  int aio_write_flag    = 0;
  int max_out_reqs      = 1;
  int nton_file_flag    = 0;
  int amode;
  int writing_flag;
  int reading_flag;
  int strided_io_flag   = 0;
  int read_check_flag   = 0;
  int read_check_failed = 0;
  int file_trunc_flag   = 0;
  int panfs_concurrent_write = 0;
  int element_size           = sizeof(double);
  MPI_Datatype mpi_data_type = MPI_DOUBLE;
  int *ivalue = NULL, *result = NULL;
  ssize_t  rc;
  unsigned long izero   = 0;
  char   *cbuf          = NULL;
  char   *wbuf          = NULL;
  unsigned char set_byte_value;
  unsigned char byte_value;
  double total_test_time_start;
  size_t        lengthb;
  int           lengthe;
  MPI_Offset   *length_by_write;
  MPI_Offset   *file_offset_by_write_my_rank;
  MPI_Offset    offset;
  MPI_Offset    woff;
  MPI_Offset    file_size_check;
  double        one_MB = (double)(1024*1024);
  unsigned long usleep_time = 0;
#define NTRYS 2
  int my_open_status, *open_status, open_trouble, ntrys;
  struct stat stat_buf;
    
  struct tm *ptr;
  time_t lt;

 /*****************************************************************/
 /* MPI Initialization                                            */
 /*****************************************************************/
  if (MPI_Init(&argc, &argv) != MPI_SUCCESS) {
    fprintf(stderr, "ERROR: Problem with MPI_Init.\n");
    return -1;
  }
  if (MPI_Comm_rank(MPI_COMM_WORLD, &my_rank) != MPI_SUCCESS) {
    fprintf(stderr, "[RANK %d] ERROR: Problem getting processor rank.\n",my_rank);
    return -1;
  }
  if (MPI_Comm_size( MPI_COMM_WORLD, &numpes) != MPI_SUCCESS){
    fprintf(stderr, "[RANK %d] ERROR: Problem getting number of processors.\n",my_rank);
    return -1;
  }
  
 /******************************************************************
 * Print routine identifier and date 
 ******************************************************************/
  if(my_rank == 0)
  {
    fprintf(stdout,"################ Execute Line ##########################\n");
    lt = time(NULL);
    ptr = localtime(&lt);
    fprintf(stdout,"###### %s V.1.0.0: %s\n", argv[0],asctime(ptr));
    i = 0; while ( i < argc) { fprintf(stdout,"%s ",argv[i++]); }
    fprintf(stdout,"\n\n");
    fflush(stdout);
  }
 
 /******************************************************************
 * Check number of command line arguments 
 ******************************************************************/
  if(argc < 2)
  {
    if (!my_rank)
    {
      fprintf(stderr,"%s:\n",argv[0]);
      usage();
    }
    MPI_Finalize();
    return -1;
  }

/***************************************************
* Parse the command line
**************************************************/
  i = 1;
  while ( i < argc)
  {
    if(!strcmp(argv[i], "-nw")){
      num_writes = atoi(argv[i+1]);
      i += 2;
    }
    else if(!strcmp(argv[i], "-size"))
    {
          obj_size_min =   atoi(argv[i+1]);
          obj_size_max =   atoi(argv[i+2]);
          obj_size_inc =   atoi(argv[i+3]);
          if (i+4 < argc)
          {
            if (          strncmp(argv[i+4],"-",1)) 
            {
              element_size = atoi(argv[i+4]);
              i += 1;
            }
          }
     if ( element_size == 1 ) mpi_data_type = MPI_BYTE;
     else
     if ( element_size == 4 ) mpi_data_type = MPI_FLOAT;
     else
     if ( element_size == 8 ) mpi_data_type = MPI_DOUBLE;
     else
     {
      if(!my_rank)
      {
        fprintf(stderr, "[RANK %d] ERROR: Element size <> 1,4,8 (%d)\n",my_rank, element_size);
      }
      MPI_Finalize();
      return -1;
     }
     if ( obj_size_min%element_size != 0 || obj_size_max%element_size != 0 || obj_size_inc%element_size != 0) 
     {
      if(!my_rank)
      {
        fprintf(stderr, "[RANK %d] ERROR: Object size min,max,incr (%ld, %ld, %ld) must be a multiple of element size %d.\n",
              my_rank, obj_size_min, obj_size_max, obj_size_inc, element_size);
      }
      MPI_Finalize();
      return -1;
     }
      num_obj_sizes = (obj_size_max - obj_size_min) / obj_size_inc + 1;
      i += 4;
    }
    else if(!strcmp(argv[i], "-cwrite")){ 
      coll_write_flag  = 1; 
      mpio_write_flag  = 0; 
      posix_write_flag = 0;
      i += 1; 
    }
    else if(!strcmp(argv[i], "-mwrite")){ 
      coll_write_flag  = 0; 
      mpio_write_flag  = 1; 
      posix_write_flag = 0;
      i += 1; 
    }
    else if(!strcmp(argv[i], "-iwrite"))
    {
      aio_write_flag  = 1;
      i += 1;
    }
    else if(!strcmp(argv[i], "-maxout"))
    {
      max_out_reqs  = atoi(argv[i+1]);
      max_out_reqs  = (max_out_reqs <           1) ?           1 : max_out_reqs;
      i += 2;
    }
    else if(!strcmp(argv[i], "-pwrite")){ 
      posix_write_flag = 1; 
      coll_write_flag  = 0; 
      mpio_write_flag  = 0; 
      i += 1; 
    }
    else if(!strcmp(argv[i], "-cread")){ 
      coll_read_flag  = 1; 
      mpio_read_flag  = 0; 
      posix_read_flag = 0; 
      i += 1; 
    }
    else if(!strcmp(argv[i], "-mread")){ 
      coll_read_flag  = 0; 
      mpio_read_flag  = 1; 
      posix_read_flag = 0; 
      i += 1; 
    }
    else if(!strcmp(argv[i], "-pread")){ 
      posix_read_flag = 1; 
      coll_read_flag  = 0; 
      mpio_read_flag  = 0; 
      i += 1; 
    }
    else if(!strcmp(argv[i], "-strided")){
      strided_io_flag = 1;
      i += 1;
    }
    else if(!strcmp(argv[i], "-nton")){
      nton_file_flag = 1;
      i += 1;
    }
    else if(!strcmp(argv[i], "-rcheck")){
      read_check_flag = 1;
      i += 1;
    }
    else if(!strcmp(argv[i], "-ftrunc")){
      file_trunc_flag = 1;
      i += 1;
    }
    else if(!strcmp(argv[i], "-wsync")){
      sync_write_flag = 1;
      i += 1;
    }
    else if(!strcmp(argv[i], "-csync")){
      sync_close_flag = 1;
      i += 1;
    }
    else if(!strcmp(argv[i], "-wbarr")){
      barr_write_flag = 1; 
      i += 1;
    }
    else if(!strcmp(argv[i], "-wared")){
      ared_write_flag = 1; 
      if( (ivalue = (int *)valloc(     1*sizeof(int))) == NULL)
      {
        printf("RANK %d - ERROR: Unable to allocate memory for Allreduce result buffer\n",my_rank);
        MPI_Finalize();
        return -1;
      }
      i += 1;
    }
    else if(!strcmp(argv[i], "-wallg")){
      allg_write_flag = 1; 
      if( (ivalue = (int *)valloc(     1*sizeof(int))) == NULL)
      {
        printf("RANK %d - ERROR: Unable to allocate memory for Allgather result buffer\n",my_rank);
        MPI_Finalize();
        return -1;
      }
      if( (result = (int *)valloc(numpes*sizeof(int))) == NULL)
      {
        printf("RANK %d - ERROR: Unable to allocate memory for Allgather result buffer\n",my_rank);
        MPI_Finalize();
        return -1;
      }
      i += 1;
    }
    else if(!strcmp(argv[i], "-watoa")){
      atoa_write_flag = 1;
      if( (ivalue = (int *)valloc(numpes*sizeof(int))) == NULL)
      {
        printf("RANK %d - ERROR: Unable to allocate memory for Alltoall result buffer\n",my_rank);
        MPI_Finalize();
        return -1;
      }
      if( (result = (int *)valloc(numpes*sizeof(int))) == NULL)
      {
        printf("RANK %d - ERROR: Unable to allocate memory for Alltoall result buffer\n",my_rank);
        MPI_Finalize();
        return -1;
      }
      i += 1;
    }
    else if(!strcmp(argv[i], "-wsleep"))
    {
      sleep_write_flag = 1;
      usleep_time   = atoi(argv[i+1]);
      i += 2;
    }
    else if(!strcmp(argv[i], "-fname")){
      strncpy(fnamew,argv[i+1],MPI_MAX_PROCESSOR_NAME-1);
      i += 2;
    }
    else if(!strcmp(argv[i], "-hints")){
      MPI_Info_create(&info);
      i++;
      
      while(strncmp(argv[i], "-", 1)){
	MPI_Info_set(info, argv[i], argv[i+1]);
        if (!strcmp(argv[i],"panfs_concurrent_write")) panfs_concurrent_write = atoi(argv[i+1]);
        if (!strcmp(argv[i],"cb_nodes"              )) cb_nodes               = atoi(argv[i+1]);
        if (!strcmp(argv[i],"cb_buffer_size"        )) cb_buffer_size         = atoi(argv[i+1]);
	i += 2;
      }
    }
    else{
      if(my_rank == 0)
	fprintf(stderr, "[RANK %d] Warning: Unrecognized input %s\n", my_rank, argv[i]);
      i++;
    }
  }
  /* ------------------------------------ */
  /* Over-ride input line inconsistencies */
  /* ------------------------------------ */
  if (!aio_write_flag) max_out_reqs    = 1;
  if ( nton_file_flag) strided_io_flag = 0;
  if (!my_rank && info != MPI_INFO_NULL)
  {
     fprintf(stdout,"XXXXXXXXXXXXXX MPI-IO Hints after input: ##################\n");
        MPI_Info_get_nkeys(         info, &nkeys);
        for(i=0; i < nkeys; i++)
        {
          MPI_Info_get_nthkey(info, i, key);
          MPI_Info_get(info, key, 200, value, &flag);
          fprintf(stdout,"%-32s = %-16s  ######\n", key, value); 
        }
        fflush(stdout);
  }


  writing_flag = posix_write_flag + mpio_write_flag + coll_write_flag + aio_write_flag;
  reading_flag = posix_read_flag  + mpio_read_flag  + coll_read_flag;
/*  if (writing_flag == 0) reading_flag == 0;  */

  /* ---------------------------------------------- */
  /* Build test parameters string for rompio_timing */
  /* ---------------------------------------------- */
  sprintf(test_params," %4d %7ld %9ld %9ld %9ld %2d %1d", 
     numpes,num_writes,obj_size_min,obj_size_max,obj_size_inc,num_obj_sizes,element_size);
  if (writing_flag)
  {
    if (posix_write_flag) strncpy(&write_params[0],"p",1);
    if (mpio_write_flag ) strncpy(&write_params[0],"m",1);
    if (coll_write_flag ) strncpy(&write_params[0],"c",1);
    if (aio_write_flag  ) strncpy(&write_params[0],"i",1);
    if ( strided_io_flag) strncpy(&write_params[1],"s",1);
    if (!strided_io_flag) strncpy(&write_params[1],"n",1);
    if (!nton_file_flag ) strncpy(&write_params[2],"1",1);
    if ( nton_file_flag ) strncpy(&write_params[2],"n",1);
    if (coll_write_flag ) sprintf(&write_params[3],"_c%02d_b%05d",cb_nodes,(cb_buffer_size/1024));
    if (aio_write_flag  ) sprintf(&write_params[3],"_a%02d",max_out_reqs);
    if (sync_write_flag ) strcat (write_params,"_wsync");
    if (sync_close_flag ) strcat (write_params,"_csync");
  }
    if (sleep_write_flag) strcat (write_params,"_wsleep_");
    if (sleep_write_flag) sprintf(&write_params[strlen(write_params)],"%lu",usleep_time);
    if (allg_write_flag ) strcat (write_params,"_wallg");
    if (atoa_write_flag ) strcat (write_params,"_watoa");
    if (barr_write_flag ) strcat (write_params,"_wbarr");
    if (ared_write_flag ) strcat (write_params,"_wared");
  if (reading_flag)
  {
    if (posix_read_flag ) strncpy(&read_params[0],"p",1);
    if (mpio_read_flag  ) strncpy(&read_params[0],"m",1);
    if (coll_read_flag  ) strncpy(&read_params[0],"c",1);
    if ( strided_io_flag) strncpy(&read_params[1],"s",1);
    if (!strided_io_flag) strncpy(&read_params[1],"n",1);
    if (!nton_file_flag ) strncpy(&read_params[2],"1",1);
    if ( nton_file_flag ) strncpy(&read_params[2],"n",1);
    if (coll_read_flag  ) sprintf(&read_params[3],"_c%02d_b%05d",cb_nodes,(cb_buffer_size/1024));
    if (read_check_flag ) strcat(read_params,"_rcheck");
  }
  if (writing_flag || reading_flag )
  {
    /* ----------------------- */
    /* Set file name if needed */
    /* ----------------------- */
    if (!strncmp(&fnamew[0],"\0",1)) 
    {
      strncpy(&fnamew[0],write_params,3);
      sprintf(&fnamew[3],".p%04d.s%07d-%09d.w%05d",numpes,obj_size_min,obj_size_max,num_writes);
    }
    strcpy(fnamer,fnamew);
    if (nton_file_flag) 
    {
      sprintf(&fnamew[strlen(fnamew)],".%04d", my_rank);
      sprintf(&fnamer[strlen(fnamer)],".%04d",(my_rank+1)%numpes);
    }
  }


  if( (length_by_write = (MPI_Offset *)malloc(numpes*num_obj_sizes * sizeof(MPI_Offset))) == NULL)
  {
    printf("RANK %d - ERROR: Unable to allocate memory for length_one_write\n",my_rank);
    MPI_Finalize();
    return -1;
  }
  /* -------------------------------------------------- */
  /* First determine length distribution for each write */
  /* so that each pe varies ea write and no pes ==      */
  /* Any distribution can be implemented here           */
  /* -------------------------------------------------- */
  for (j=0; j<numpes; j++)
  {
    for (i=0; i<num_obj_sizes; i++)
    {
      length_by_write[i+j*num_obj_sizes] = obj_size_min + obj_size_inc*(i % num_obj_sizes);
    }
  }
  /* --------------------------------------------------------------------------- */
  /* Next calculate the starting file offset for each rank, assuming the         */
  /* length for write [i] of rank [j] == length_by_write[(i+j) % num_obj_sizes]. */
  /* In this scheme, each pe  is shifted in the generic length_by_write          */
  /* table by its rank.                                                          */
  /* --------------------------------------------------------------------------- */

  if( (file_offset_by_write_my_rank = (MPI_Offset *)malloc(num_writes * sizeof(MPI_Offset))) == NULL){
    printf("RANK %d - ERROR: Unable to allocate memory for offset_by_rank\n",my_rank);
    MPI_Finalize();
    return -1;
  }
  memset((void*)file_offset_by_write_my_rank,0,num_writes*sizeof(MPI_Offset));
  offset  = 0;
  if ( nton_file_flag)
  { /* N to N, Non-strided */
      for (i=0; i<num_writes; i++)
      {
        file_offset_by_write_my_rank[i] = offset;
        offset += length_by_write[(i+my_rank)%num_obj_sizes];
      }
  }
  else
  if (!strided_io_flag)
  { /* N to 1, Non-strided */
    for (j=0; j<numpes; j++)
    {
      for (i=0; i<num_writes; i++)
      {
        if (j == my_rank ) file_offset_by_write_my_rank[i] = offset;
        offset += length_by_write[i%num_obj_sizes+j*num_obj_sizes];
      }
    }
  }
  else
  { /* N to 1, Strided */
    for (i=0; i<num_writes; i++)
    {
      for (j=0; j<numpes; j++)
      {
        if (j == my_rank) file_offset_by_write_my_rank[i] = offset;
        offset += length_by_write[i%num_obj_sizes+j*num_obj_sizes];
      }
    }
  }
  file_size_check                = offset;
  rompio_file_size_check = (double)offset;


#ifdef DBUG
  for (i=0; i<num_writes; i++)
  {
    lengthb = (size_t)length_by_write     [i%num_obj_sizes+my_rank*num_obj_sizes];
    offset  = file_offset_by_write_my_rank[i];
    fprintf(stdout,"R %03d write# %03d length %10ld offset %10Ld\n",my_rank,i,lengthb,offset);
  }
#endif
 
/*********************************************/
/* Allocate/set memory for the copy  buffer  */
/* Allocate memory for the write buffer      */
/*********************************************/
  if( (cbuf = (char *)valloc(obj_size_max             )) == NULL)
  {
    printf("RANK %d - ERROR: Unable to allocate memory for the write buffer\n",my_rank);
    MPI_Finalize();
    return -1;
  }
   for (j=0; j< obj_size_max; j++) cbuf[j] = (unsigned char)((((my_rank+             0)%numpes)+j)%254+1);
  if( (wbuf = (char *)valloc(obj_size_max*max_out_reqs)) == NULL)
  {
    printf("RANK %d - ERROR: Unable to allocate memory for the write buffer\n",my_rank);
    MPI_Finalize();
    return -1;
  }
 
  /* --------------------- */
  /* Print test parameters */
  /* --------------------- */
  if (!my_rank)
  {
    fprintf(stdout,"################ Test Parameters ##########################\n");
    if ( nton_file_flag ) fprintf(stdout,"%d Processes <-I/O-> %d Files                        ######\n",numpes,numpes);
    if (!nton_file_flag ) fprintf(stdout,"%d Processes <-I/O->  1 File                         ######\n",numpes);
    if ( strided_io_flag) fprintf(stdout,"Using Strided I/O                                    ######\n");
    if (!strided_io_flag) fprintf(stdout,"Using NonStrided I/O                                 ######\n");
    if (coll_write_flag ) fprintf(stdout,"Using MPI_File_write_at_all --- collective           ######\n");
    if (coll_read_flag  ) fprintf(stdout,"Using MPI_File_read_at_all  --- collective           ######\n");
    if (!strided_io_flag && (coll_write_flag || coll_read_flag)) 
    fprintf(stdout,"WARNING: Using NonStrided I/O with collective write/read. May NOT work if request domain too large ######\n");
    if (mpio_read_flag  ) fprintf(stdout,"Using MPI_File_read_at      --- independent          ######\n");
    if (mpio_write_flag ) fprintf(stdout,"Using MPI_File_write_at     --- independent          ######\n");
    if (aio_write_flag  ) fprintf(stdout,"Using MPI_File_iwrite_at    --- independent          ######\n");
    if (aio_write_flag  ) fprintf(stdout,"Using MPI_Wait with %d maximum outstanding requests  ######\n",max_out_reqs);
    if (posix_write_flag) fprintf(stdout,"Using POSIX pwrite64        --- independent          ######\n");
    if (posix_read_flag ) fprintf(stdout,"Using POSIX pread64         --- independent          ######\n");
    if (sync_write_flag ) fprintf(stdout,"Using sync after each write                          ######\n");
    if (barr_write_flag ) fprintf(stdout,"Using MPI_Barrier   before each write                ######\n");
    if (allg_write_flag ) fprintf(stdout,"Using MPI_Allgather before each write                ######\n");
    if (atoa_write_flag ) fprintf(stdout,"Using MPI_Alltoall  before each write                ######\n");
    if (read_check_flag ) fprintf(stdout,"Read check after each read                           ######\n");
    fprintf(stdout,"Number of MPI processes                %12ld  ######\n", numpes);
    fprintf(stdout,"Number of               writes / proc  %12ld  ######\n", num_writes);
    fprintf(stdout,"min,max,inc,num,size ea write / proc   %10ldB %10ldB, %10ldB, %4d, %4dB ######\n", 
                                               obj_size_min,obj_size_max,obj_size_inc,num_obj_sizes,element_size);
    fprintf(stdout,"sizeof(MPI_Offset)                     %12ldB ######\n", sizeof(MPI_Offset));
    fprintf(stdout,"sizeof(size_t)                         %12ldB ######\n", sizeof(size_t));
    fprintf(stdout,"Size of file                           %10.4lfMB ######\n",rompio_file_size_check/one_MB);
  }

  if ( MPI_Barrier(MPI_COMM_WORLD) != MPI_SUCCESS)
  {
    fprintf(stderr,"RANK %d - ERROR: MPI_Barrier at Location 0010\n",my_rank);
    MPI_Finalize();
    return -1;
  }

  total_test_time_start = MPI_Wtime();

 if (writing_flag || barr_write_flag || allg_write_flag || atoa_write_flag || ared_write_flag || sleep_write_flag)
 {

  if (writing_flag)
  {
    /* --------- */
    /* Open file */
    /* --------- */
 
    rompio_tims[WOPN] = MPI_Wtime();
    if (posix_write_flag)
    {
      open_flags = (panfs_concurrent_write == 1) ? (O_CREAT | O_RDWR | O_CONCURRENT_WRITE) : ( O_CREAT | O_RDWR );
      if ((fd = open(fnamew, open_flags, 0666)) == -1)
      {
        sprintf(ctmp,"RANK %d - ERROR: Unable to popen file %s for write\n",my_rank, fnamew);
        perror(ctmp);
        MPI_Finalize();
        return -1;
      }
      if (!my_rank) fprintf(stdout,"POSIX IO open for write with panfs_concurrent_write = %d ######\n", panfs_concurrent_write);
    }
    else
    if (mpio_write_flag || coll_write_flag || aio_write_flag )
    {
                          amode = MPI_MODE_WRONLY | MPI_MODE_CREATE;
      if (nton_file_flag) amode = amode |  MPI_MODE_UNIQUE_OPEN;
      if( MPI_File_open(MPI_COMM_WORLD, fnamew, amode,info, &fh) != MPI_SUCCESS) 
      { 
        sprintf(ctmp,"RANK %d - ERROR: Unable to MPI_File_open file %s for write\n",my_rank, fnamew);
        perror(ctmp);
        MPI_Finalize();
        return -1;
      }
      /* ----------- */
      /* Print hints */
      /* ----------- */
      if (!my_rank) 
      {
        fprintf(stdout,"XXXXXXXXXXXXXX MPI-IO Hints for write on file: ##################\n");
        fprintf(stdout,"%s ######\n", fnamew);
        MPI_Info_create   (    &def_info);
        MPI_File_get_info (fh, &def_info);
        MPI_Info_get_nkeys(     def_info, &nkeys);
        for(i=0; i < nkeys; i++)
        {
          MPI_Info_get_nthkey(def_info, i, key);
          MPI_Info_get(def_info, key, 200, value, &flag);
          fprintf(stdout,"%-32s = %-16s  ######\n", key, value);
        }
          MPI_Info_get(def_info, "filename", 200, value, &flag);
          fprintf(stdout,"filename = %-16s  ######\n", value);
        fflush(stdout);
        MPI_Info_free(&def_info);
        fprintf(stdout,"XXXXENDXXXXXXX MPI-IO Hints for write on file: ##################\n");
      }
    }
    else
    {
      printf("RANK %d - LOGIC ERROR: undefined write type for write open, should not have reached this point\n",my_rank);
      MPI_Finalize();
      return -1;
    }
    rompio_time[WOPN] += MPI_Wtime() - rompio_tims[WOPN];
  }
  if (aio_write_flag)
  {
    if( (io_request = (MPIO_Request *) malloc(max_out_reqs * sizeof( MPIO_Request))) == NULL)
    {
      printf("RANK %d - ERROR: Unable to allocate memory for io_request\n",my_rank);
      MPI_Finalize();
      return -1;
    }
    for (i=0;i<max_out_reqs;i++) io_request[i] = NULL;
    if( (io_status = (MPI_Status *) malloc(max_out_reqs * sizeof( MPI_Status))) == NULL)
    {
      printf("RANK %d - ERROR: Unable to allocate memory for io_status\n",my_rank);
      MPI_Finalize();
      return -1;
    }
  }

  /* =========== */
  /* Write test  */
  /* =========== */

  for(i=0; i < num_writes; i++)
  {
    idx_write = i;

    if (sleep_write_flag)
    {
      rompio_tims[WSLP] = MPI_Wtime();
      sleep_lite ( usleep_time );
      rompio_time[WSLP] += MPI_Wtime() - rompio_tims[WSLP];
    }

    if (barr_write_flag)
    {
      rompio_tims[WBAR] = MPI_Wtime();
      if ( MPI_Barrier(MPI_COMM_WORLD) != MPI_SUCCESS)
      {
        fprintf(stderr,"RANK %d - ERROR: MPI_Barrier at Loc 0015\n",my_rank);
        MPI_Finalize();
        return -1;
      }
      rompio_time[WBAR] += MPI_Wtime() - rompio_tims[WBAR];
    }
 
    if (allg_write_flag)
    {
      rompio_tims[WALG] = MPI_Wtime();
      ivalue[0] = i+my_rank;
      if ( MPI_Allgather(ivalue, 1, MPI_INTEGER4, result, 1, MPI_INTEGER4, MPI_COMM_WORLD) != MPI_SUCCESS )
      {
        fprintf(stderr,"RANK %d - ERROR: MPI_Allgather\n",my_rank);
        MPI_Finalize();
        return -1;
      }
      rompio_time[WALG] += MPI_Wtime() - rompio_tims[WALG];
    }
 
    if (atoa_write_flag)
    {
      rompio_tims[WA2A] = MPI_Wtime();
      for (j=0;j<numpes;j++) ivalue[j] = i+my_rank+j;
      if ( MPI_Alltoall(ivalue, 1, MPI_INTEGER4, result, 1, MPI_INTEGER4, MPI_COMM_WORLD) != MPI_SUCCESS )
      {
        fprintf(stderr,"RANK %d - ERROR: MPI_Allgather\n",my_rank);
        MPI_Finalize();
        return -1;
      }
      rompio_time[WA2A] += MPI_Wtime() - rompio_tims[WA2A];
    }

    if (ared_write_flag) 
    {      
      rompio_tims[WARD] = MPI_Wtime();
      ivalue[0] = i+my_rank;
      if ( MPI_Allreduce(ivalue, ivalue, 1, MPI_INTEGER4, MPI_SUM, MPI_COMM_WORLD) != MPI_SUCCESS )
      {
        fprintf(stderr,"RANK %d - ERROR: MPI_Allreduce\n",my_rank);
        MPI_Finalize();
        return -1;  
      }
      rompio_time[WARD] += MPI_Wtime() - rompio_tims[WARD];
    }

    if (writing_flag)
    {
      offset  = file_offset_by_write_my_rank[i];
      lengthb = (size_t)length_by_write[i%num_obj_sizes+my_rank*num_obj_sizes];
      lengthe = (int)lengthb/element_size;
      woff    = (i%max_out_reqs)*obj_size_max;
      rompio_time[WBYT] += (double)lengthb;

      if (aio_write_flag)
      {
        rompio_tims[WAIO] = MPI_Wtime();
        if( MPIO_Wait   (&io_request[i%max_out_reqs],&io_status[i%max_out_reqs]) != MPI_SUCCESS)
        {
          printf("RANK %d - ERROR: MPI_Wait: Unable to check status object %d with size %d.\n", my_rank, i+1, lengthb);
           MPI_Finalize();
          return;  
        }
        rompio_time[WAIO] += MPI_Wtime() - rompio_tims[WAIO];
      }    
      rompio_tims[WBUF] = MPI_Wtime();  
      memcpy(&wbuf[woff],&cbuf[0],lengthb);
      rompio_time[WBUF] += MPI_Wtime() - rompio_tims[WBUF];
         
      rompio_tims[WRAW] = MPI_Wtime();
      if (posix_write_flag)
      {
        if ((rc = pwrite64(fd, &wbuf[woff], lengthb, (off64_t) offset)) != lengthb) 
        {
          perror("pwrite64");
          printf("RANK %d - ERROR: Unable to write object %d, fd %d, rc %ld with size %d., offset %ld\n",
                        my_rank, i+1, fd, rc, lengthb, offset);
          MPI_Finalize();
          return -1;
        }
      }
      else
      if (mpio_write_flag)
      {
        if( MPI_File_write_at    (fh, offset, &wbuf[woff], lengthe, mpi_data_type, &status) != MPI_SUCCESS)
        {
          printf("RANK %d - ERROR: Unable to write object %d with size %d.\n", my_rank, i+1, lengthb);
          MPI_Finalize();
          return -1;
        }
      }
      else
      if (aio_write_flag)
      {
        if( MPI_File_iwrite_at   (fh, offset, &wbuf[woff], lengthe, mpi_data_type, &io_request[i%max_out_reqs]) != MPI_SUCCESS)
        {
          printf("RANK %d - ERROR: MPI_File_iwrite_at: Unable to write object %d with size %d.\n", my_rank, i+1, lengthb);
           MPI_Finalize();
          return;
        }
      }
      else
      if (coll_write_flag)
      {
        if( MPI_File_write_at_all(fh, offset, &wbuf[woff], lengthe, mpi_data_type, &status) != MPI_SUCCESS)
        {
          printf("RANK %d - ERROR: Unable to write object %d with size %d.\n", my_rank, i+1, lengthb);
          MPI_Finalize();
          return -1;
        }
      }
      else
      {
        printf("RANK %d - LOGIC ERROR: undefined write type for write, should not have reached this point\n",my_rank);
        MPI_Finalize();
        return -1;
      }
      rompio_time[WRAW] += MPI_Wtime() - rompio_tims[WRAW];

      if (sync_write_flag)
      {
        rompio_tims[WSYN] = MPI_Wtime();
        if (posix_write_flag && fd != -1)
        {
          if ((rc = fsync(fd))) 
          {
            printf("RANK %d - ERROR: after write %03d Unable to sync after offset %011Ld size %011ld.\n",
                   my_rank, i+1, offset, lengthb);
            MPI_Finalize();
            return -1;
          }
        }
        else
        if (mpio_write_flag || coll_write_flag || aio_write_flag)
        {
          if ( MPI_File_sync(fh) != MPI_SUCCESS )
          {
            printf("RANK %d - ERROR: after write %03d Unable to sync after offset %011Ld size %011ld.\n",
                   my_rank, i+1, offset, lengthb);
            MPI_Finalize();
            return -1;
          }
        }
        else
        {
          printf("RANK %d - LOGIC ERROR: undefined write type for fsync, should not have reached this point\n",my_rank);
          MPI_Finalize();
          return -1;
        }
        rompio_time[WSYN] += MPI_Wtime() - rompio_tims[WSYN];
      }
    }
  }
  /* ================= */
  /* End of write loop */
  /* ================= */
 
  if (writing_flag)
  {
    if (aio_write_flag)
    {
      rompio_tims[WAIO] = MPI_Wtime();
      for (i=0; i<max_out_reqs; i++)
      {
        if( MPIO_Wait   (&io_request[i],&io_status[i]) != MPI_SUCCESS)
        {
          printf("RANK %d - ERROR: MPI_Wait: Unable to check status object %d with size %d.\n", my_rank, i+1, lengthb);
          MPI_Finalize();
          return;
        }
      }
      rompio_time[WAIO] += MPI_Wtime() - rompio_tims[WAIO];
    }
  }

  if (writing_flag)
  {
    if (sync_close_flag)
    {
      rompio_tims[CSYN] = MPI_Wtime();
      if (posix_write_flag && fd != -1)
      {
        if ((rc = fsync(fd)))
        {
          printf("RANK %d - ERROR: after write %03d Unable to sync after offset %011Ld size %011ld.\n",
                 my_rank, i+1, offset, lengthb);
          MPI_Finalize();
          return -1;
        }
      }
      else
      if (mpio_write_flag || coll_write_flag || aio_write_flag)
      {
        if ( MPI_File_sync(fh) != MPI_SUCCESS )
        {
          printf("RANK %d - ERROR: after write %03d Unable to sync after offset %011Ld size %011ld.\n",
                 my_rank, i+1, offset, lengthb);
          MPI_Finalize();
          return -1;
        }
      }
      else
      {
        printf("RANK %d - LOGIC ERROR: undefined write type for fsync, should not have reached this point\n",my_rank);
        MPI_Finalize();
        return -1;
      }
      rompio_time[CSYN] += MPI_Wtime() - rompio_tims[CSYN];
    }
  }

  if (writing_flag)
  {
    /* ------------- */
    /* Truncate file */
    /* ------------- */
    if (file_trunc_flag)
    {
      rompio_tims[TRNC] = MPI_Wtime();
      if (posix_write_flag)
      {
        if (!my_rank)
        {
          if ((rc = ftruncate(fd, file_size_check)) == -1)
          {
            printf("RANK %d - ERROR: Unable to ftruncate file %s to size %ld.\n",my_rank, fnamew, rompio_file_size_check);
            MPI_Finalize();
            return -1;
          }
        }
        if ( MPI_Barrier(MPI_COMM_WORLD) != MPI_SUCCESS)
        {
          fprintf(stderr,"RANK %d - ERROR: MPI_Barrier after ftruncate\n",my_rank);
          MPI_Finalize();
          return -1;
        }
      }
      else
      if (mpio_write_flag || coll_write_flag || aio_write_flag)
      {
        if( MPI_File_set_size(fh, file_size_check ) != MPI_SUCCESS)
        {
          printf("RANK %d - ERROR: Unable to MPI_File_set_size file %s to size %ld.\n",my_rank, fnamew, file_size_check);
          /* dont quit - the debug print indicates error code not set for procs other than 0 */
          /* MPI_Finalize();
           return -1;  */
        }
      }
      else
      {
        printf("RANK %d - LOGIC ERROR: undefined write type for file set size, should not have reached this point\n",my_rank);
        MPI_Finalize();
        return -1;
      }
      rompio_time[TRNC] += MPI_Wtime() - rompio_tims[TRNC];
    }
  }
 
  if (writing_flag)
  {
    rompio_tims[WCLS] = MPI_Wtime();
    if (posix_write_flag)          close( fd);
    if (mpio_write_flag ||
        coll_write_flag ||
         aio_write_flag ) MPI_File_close(&fh);
    rompio_time[WCLS] += MPI_Wtime() - rompio_tims[WCLS];
  }

  rompio_tims[BBAR] = MPI_Wtime();
  if ( MPI_Barrier(MPI_COMM_WORLD) != MPI_SUCCESS)
  {
    fprintf(stderr,"RANK %d - ERROR: MPI_Barrier at Location 0041\n",my_rank);
    MPI_Finalize();
    return -1;
  }
  rompio_time[BBAR] += MPI_Wtime() - rompio_tims[BBAR];
 }


  if (reading_flag)
  {
    if( (open_status = (int *)malloc(numpes*sizeof(int))) == NULL)
    { 
      printf("RANK %d - ERROR: Unable to allocate memory for Allgather open_status buffer\n",my_rank);
      MPI_Finalize();
      return -1;
    }
   /* ======================== */
   /* Open file for read check */
   /* ======================== */
   rompio_tims[ROPN] = MPI_Wtime();
   ntrys        = 0;
   if (posix_read_flag)
   {
     open_flags = (panfs_concurrent_write == 1) ? (O_RDONLY | O_CONCURRENT_WRITE) : ( O_RDONLY );
     while ( (fd = open(fnamer, open_flags)) == -1 && ntrys < NTRYS )
     {
       sprintf(ctmp,"RANK %d - ERROR: Unable to POSIX open file for read, try stat: %s\n",my_rank, fnamer);
       perror(ctmp);
       if ( stat(fnamer, &stat_buf) == -1 )
       {
         sprintf(ctmp,"RANK %3d - ERROR: Unable to stat file for read: %s\n",my_rank, fnamer);
         perror(ctmp);
       }
       fprintf(stdout,"RANK %3d: Able to stat file [%s] for read, size %Ld ... sleep for 0s and try again ######\n",
                                                                        my_rank, fnamer, stat_buf.st_size);
                           my_open_status = -1;
       if ( MPI_Allgather(&my_open_status, 1, MPI_INTEGER4, open_status, 1, MPI_INTEGER4, MPI_COMM_WORLD) != MPI_SUCCESS )
       { 
         fprintf(stderr,"RANK %d - ERROR: MPI_Allgather open status\n",my_rank); 
         MPI_Finalize(); 
         return -1; 
       }
/*     sleep_lite ( 999999UL ); */
       ntrys++;
     }
     if (!my_rank) fprintf(stdout,"POSIX IO open for read  with panfs_concurrent_write = %d ######\n", panfs_concurrent_write);
   }
   else
   if (mpio_read_flag || coll_read_flag)
   {
                          amode =          MPI_MODE_RDONLY;
      if (nton_file_flag) amode = amode |  MPI_MODE_UNIQUE_OPEN;
     while( MPI_File_open(MPI_COMM_WORLD, fnamer, amode, info, &fh) != MPI_SUCCESS && ntrys < NTRYS ) 
     {
       sprintf(ctmp,"RANK %d - ERROR: Unable to MPI_File_open file %s for read\n",my_rank, fnamer);
       perror(ctmp);
       if ( stat(fnamer, &stat_buf) == -1 )
       {
         sprintf(ctmp,"RANK %3d - ERROR: Unable to stat file for read: %s\n",my_rank, fnamer);
         perror(ctmp);
       }
       fprintf(stdout,"RANK %3d: Able to stat file [%s] for read, size %Ld ... sleep for 0s and try again ######\n",
                                                                        my_rank, fnamer, stat_buf.st_size);
                           my_open_status = -1;
       if ( MPI_Allgather(&my_open_status, 1, MPI_INTEGER4, open_status, 1, MPI_INTEGER4, MPI_COMM_WORLD) != MPI_SUCCESS )
       {
         fprintf(stderr,"RANK %d - ERROR: MPI_Allgather open status\n",my_rank);
         MPI_Finalize();
         return -1;
       }
/*     sleep_lite ( 999999UL ); */
       ntrys++;
     }
     /* ----------- */
     /* Print hints */
     /* ----------- */
     if (!my_rank)
     {
       fprintf(stdout,"############ MPI-IO Hints for read on file: ##################\n");
       fprintf(stdout,"%s ######\n", fnamer);
       MPI_Info_create  (    &def_info);
       MPI_File_get_info(fh, &def_info);
       MPI_Info_get_nkeys(def_info, &nkeys);
       for(i=0; i < nkeys; i++)
       {
         MPI_Info_get_nthkey(def_info, i, key);
         MPI_Info_get(def_info, key, 200, value, &flag);
         fprintf(stdout,"%3d - %-32s = %-16s  ######\n", i, key, value);
       }
       fflush(stdout);
       MPI_Info_free(&def_info);
       fprintf(stdout,"##END####### MPI-IO Hints for read on file: ##################\n");
     }
   }
   else
   {
     printf("RANK %d - LOGIC ERROR: undefined read type in read open, should not have reached this point\n",my_rank);
     MPI_Finalize();
     return -1;
   }
   /* -------------------------------------- */
   /* debug to check all procs open status.  */
   /* all procs that successfully open file, */
   /* call allgather below. Procs that fail  */
   /* opens call allgathers above.           */
   /* -------------------------------------- */
          open_trouble = 1;
   while (open_trouble && ntrys < NTRYS)
   {
                         my_open_status = 0;
     if ( MPI_Allgather(&my_open_status, 1, MPI_INTEGER4, open_status, 1, MPI_INTEGER4, MPI_COMM_WORLD) != MPI_SUCCESS )
     {
       fprintf(stderr,"RANK %d - ERROR: MPI_Allgather\n",my_rank);
       MPI_Finalize();
       return -1;
     }
     open_trouble = 0;
     for (j=0; j< numpes;j++)
     {
       if (open_status[j]) 
       {
         if (!my_rank) fprintf(stderr,"RANK %3d had trouble opening file for read %s\n",j,fnamer);
         open_trouble++;
       }
     }
     ntrys++;
   }
   if (open_trouble) MPI_Abort(MPI_COMM_WORLD, open_trouble);
   free (open_status);

   /* -------------------------------------------------------- */
   /* set cbuf to expected read values specified by process id */
   /* -------------------------------------------------------- */
   for (j=0; j< obj_size_max; j++) cbuf[j] = (unsigned char)((((my_rank+nton_file_flag)%numpes)+j)%254+1);
   rompio_time[ROPN] += MPI_Wtime() - rompio_tims[ROPN];

   /* ====================== */
   /* Do reads, read checks  */
   /* ====================== */
   for(i=0; i < num_writes; i++)
   {
    offset  = file_offset_by_write_my_rank[i];
    lengthb = (size_t)length_by_write[i%num_obj_sizes+my_rank*num_obj_sizes];
    lengthe = (int)lengthb/element_size;
    rompio_time[RBYT] += (double)lengthb;

    if (read_check_flag)
    {
      rompio_tims[RCHK] = MPI_Wtime();
      memset(wbuf,255,lengthb);
      rompio_time[RCHK] += MPI_Wtime() - rompio_tims[RCHK];
    }

    rompio_tims[RRAW] = MPI_Wtime();
    if (posix_read_flag)
    {
      if ((rc = pread64(fd, wbuf, lengthb, offset)) != lengthb) 
      {
        printf("RANK %d - ERROR: Unable to read object %d with size %d.\n", my_rank, i+1, lengthb);
        MPI_Finalize();
        return -1;
      }
    }
    else
    if (coll_read_flag)
    {
      if( MPI_File_read_at_all(fh, offset, wbuf, lengthe, mpi_data_type, &status) != MPI_SUCCESS)
      {
        printf("RANK %d - ERROR: Unable to read object %d with size %d.\n", my_rank, i+1, lengthe);
        MPI_Finalize();
        return -1;
      }
    }
    else
    if (mpio_read_flag)
    {
      if( MPI_File_read_at    (fh, offset, wbuf, lengthe, mpi_data_type, &status) != MPI_SUCCESS)
      {
        printf("[RANK %d]- ERROR: Unable to read object %d with size %d.\n",my_rank, i+1,lengthe);
        MPI_Finalize();
        return -1;
      }
    }
    else
    {
      printf("RANK %d - LOGIC ERROR: undefined read type for read, should not have reached this point\n",my_rank);
      MPI_Finalize();
      return -1;
    }
    rompio_time[RRAW] += MPI_Wtime() - rompio_tims[RRAW];

    if (read_check_flag)
    {
      rompio_tims[RCHK] = MPI_Wtime();
      for (j=0; j< lengthb; j++) 
      {
       memcpy(&set_byte_value,&cbuf[j],1); 
       memcpy(    &byte_value,&wbuf[j],1); 
       if (memcmp(&byte_value,&set_byte_value,1))
       {
         if (read_check_failed < 16)
         {
           fprintf(stdout,"[%03d]: Read# %02d of %02d offset: %011Ld size: %08ld ERROR wbuf[%05d]=%03u <> %03u\n", 
              my_rank, i+1, num_writes, offset, lengthe,j,byte_value,set_byte_value);
         }
        read_check_failed++;
       }
      }
      rompio_time[RCHK] += MPI_Wtime() - rompio_tims[RCHK];
    }
   }

   rompio_tims[RCLS] = MPI_Wtime();
   if ( posix_read_flag)          close( fd);
   if (!posix_read_flag) MPI_File_close(&fh);
   rompio_time[RCLS] += MPI_Wtime() - rompio_tims[RCLS];
  }
  rompio_tims[EBAR] = MPI_Wtime();
  if ( MPI_Barrier(MPI_COMM_WORLD) != MPI_SUCCESS)
  {
    fprintf(stderr,"RANK %d - ERROR: MPI_Barrier at Location 0042\n",my_rank);
    MPI_Finalize();
    return -1;
  }
  rompio_time[EBAR] += MPI_Wtime() - rompio_tims[EBAR];

  rompio_test_time_check  = MPI_Wtime() - total_test_time_start;

/*  if (read_check_failed) MPI_Abort(MPI_COMM_WORLD, read_check_failed); */

  /* --------------------- */
  /* Print timing results  */
  /* --------------------- */
  (void)rompio_timing();

  /* ============================== */
  /* Make all procs wait for proc 0 */
  /* ============================== */
  if ( MPI_Barrier(MPI_COMM_WORLD) != MPI_SUCCESS)
  {
    fprintf(stderr,"RANK %d - ERROR: MPI_Barrier at Location 0999\n",my_rank);
    MPI_Finalize();
    return -1;
  }

  if (length_by_write)              free(length_by_write);
  if (file_offset_by_write_my_rank) free(file_offset_by_write_my_rank);
/*  if (wbuf)                         free(wbuf); */
  if (result)                       free(result);
  MPI_Finalize();
  return(0);

}


int usage(){
  fprintf(stderr,"-nw   #  \tNumber of writes for each processor\n");
  fprintf(stderr,"-size #  \tNumber of bytes (min, max, increment, [element size(1)]) for each write per processor\n");
  fprintf(stderr,"-strided \tUse Strided instead of Nonstrided I/O[default:Nonstrided]\n");
  fprintf(stderr,"-nton    \tUse N Processes to N Files I/O       [default:N Processes to 1 File]\n");
  fprintf(stderr,"-cwrite  \tUse MPI_File_write_at_all            --- collective\n");
  fprintf(stderr,"-cread   \tUse MPI_File_read_at_all             --- collective\n");
  fprintf(stderr,"-mwrite  \tUse MPI_File_write_at                --- independent\n");
  fprintf(stderr,"-iwrite  \tUse MPI_File_iwrite                  --- independent\n");
  fprintf(stderr,"-maxout #\tMaximum number of outstanding I/O requests for iwrite\n");
  fprintf(stderr,"-mread   \tUse MPI_File_read_at                 --- independent\n");
  fprintf(stderr,"-pwrite  \tUse POSIX pwrite                     --- independent\n");
  fprintf(stderr,"-pread   \tUse POSIX pread                      --- independent\n");
  fprintf(stderr,"-rcheck  \tCheck data after read                --- no read check[default]\n");
  fprintf(stderr,"-ftrunc  \tTruncate file to calculated size on close[no truncate]\n");
  fprintf(stderr,"-wsync   \tCall fsync (-pwrite) or MPI_File_sync (-[c,m]write) after each write\n");
  fprintf(stderr,"-csync   \tCall fsync (-pwrite) or MPI_File_sync (-[c,m]write) after end  write  test\n");
  fprintf(stderr,"-wbarr   \tCall MPI_Barrier   before each write\n");
  fprintf(stderr,"-wallg   \tCall MPI_Allgather before each write\n");
  fprintf(stderr,"-watoa   \tCall MPI_Alltoall  before each write\n");
  fprintf(stderr,"-wsleep #\tCall usleep for # micro-seconds  before each write\n");
  fprintf(stderr,"-fname   \%%s \tPath and file name to write to.\n");
  fprintf(stderr,"-hints   \%%s \%%s ... \%%s \%%s  \tString pairs in the form of hint name (key) and value.\n\t Hints are expected to be in pairs and can list as many pairs as you like.\n\t File will be open with these hints.\n");
  return 1;
}
void sleep_lite (
                 unsigned long usleep_time_total
                )
{
  /* ------------------ */
  /* Local Declarations */
  /* ------------------ */
  unsigned long
      usleep_time,
      usleep_time_rem = usleep_time_total;
 
  /* --------------------- */
  /* Executable Statements */
  /* --------------------- */
 
  while (usleep_time_rem > 0)
  {
           usleep_time = (usleep_time_rem > 999999) ? 999999 : usleep_time_rem;
    usleep(usleep_time);
    usleep_time_rem -= usleep_time;
  }
}
