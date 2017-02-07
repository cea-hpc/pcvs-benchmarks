/***************************************************************************
 * PROGRAM: tpf - Touch Parallel File 
 * AUTHOR:  James Nunez/Gary Grider (CCN-8)
 * DATE:    November 2002
 * LAST MODIFIED: February 18, 2003
 * PURPOSE: Program to get PFS atrributes for an existing file, default 
 *          attirbutes for a given PFS, or create (touch) a "parallel" file.
 *
 *      LOS ALAMOS NATIONAL LABORATORY
 *      An Affirmative Action/Equal Opportunity Employer
 *
 * Copyright (c) 2003
 * the Regents of the University of California.
 *
 * Unless otherwise indicated, this information has been authored by an
 * employee or employees of the University of California, operator of the Los
 * Alamos National Laboratory under Contract No. W-7405-ENG-36 with the U. S.
 * Department of Energy. The U. S. Government has rights to use, reproduce, and
 * distribute this information. The public may copy and use this information
 * without charge, provided that this Notice and any statement of authorship
 * are reproduced on all copies. Neither the Government nor the University
 * makes any warranty, express or implied, or assumes any liability or
 * responsibility for the use of this information.
***************************************************************************/

#ifndef _KMEMUSER
#define _KMEMUSER
#endif
#include <stdio.h>
#include <sys/ioctl.h>
#include <sys/stat.h>
#include <sys/mode.h>
#include <sys/errno.h>
#include <sys/types.h>
#include <sys/time.h>
#include <sys/param.h>
#include <fcntl.h>
#include <signal.h>
#include <errno.h>
#include <unistd.h>
#include <netdb.h>
#include <sys/fs/pfs/common.h>
#include <sys/fs/pfs/map.h>
#ifndef _S_IFREG
#define _S_IFREG S_IFREG
#endif
#ifndef _S_IFCHR
#define _S_IFCHR S_IFCHR
#endif
 
void print_help(){
  fprintf(stderr,"Touch Parallel File(tpf): Set or get PFS mapping attributes.\n\n");
  fprintf(stderr,"The tpf utility can be used to\n"
	  "(1) get the default settings for an existing PFS, \n"
	  "(2) get the PFS attributes for an exiting file, and \n"
	  "(3) set the PFS attributes for a new file. If the \"set\" option\n"
	  "is called with an existing file name, a warning message will be \n"
	  "printed and no action will take place. If the \"-force\" flag is\n"
	  "used with the \"set\" option and if the file exists, it will be\n"
	  "destroyed and recreated with the user provided PFS attributes. In\n"
	  "this case all data in the existing file will be lost.\n\n");

  fprintf(stderr,"Usage:\n");
  fprintf(stderr,"tpf default write_path\n");
  fprintf(stderr,"where write_path is the path to a directory where the user has\n"
	  "write privileges on a PFS. A file, with random characters will be\n"
	  "created and destroyed in this space.\n\n");

  fprintf(stderr,"tpf get file_name\n"
	  "where file_name is the full path and name of an existing file on a PFS.\n\n");

  fprintf(stderr,"tpf set file_name -force -stripe # -stride # -start_iodevice #\n");
  fprintf(stderr,"where all flags are optional and are defined as:\n"
	  "\"-force\" allows the user to destroy and recreate a file with\n"
	  "\t the specified input parameters.\n"
	  "\"-stripe\" is the number of component file systems or disks\n"
	  "\t to write your data across. The stripe may be any number\n"
	  "\t between 1 and the number of component file systems.\n"
	  "\"-stride\" is the number of bytes of data written to one disk\n"
	  "\t before writing to the next disk. The stride must be at least\n"
	  "\t 64 KB (65536 bytes).\n"
	  "\"-start_iodevice\" is the first disk to start writing data. The\n"
	  "\t range of valid disk numbers is 0 to one less than the number\n"
	  "\t of component file systems.\n");
}

int get_pfsmap(char *fname, struct pfsmap *in_pfsmap){
  int fd = 0;
  
  if( (fd = open(fname,O_RDONLY)) == -1){
    fprintf(stderr,"ERROR: Unable to open file %s.\n", fname);
    return 1;
  }
  
  if( ioctl(fd,PFSIO_GETMAP,in_pfsmap) == -1 ){
    fprintf(stderr,"ERROR: Unable to get the PFS parameters for file %s.\n", fname);
    return 1;
  }
  close(fd);
  
  return 0;
}

void print_params(char *fname, struct pfsmap pmap){
  fprintf(stdout,"PFS parameters for file %s:\n", fname);
  fprintf(stdout,"Starting I/O device: %d\n",
	  pmap.pfsmap_slice.ps_base);
  fprintf(stdout, "PFS Stride (bytes): %d\n",
	  pmap.pfsmap_stride);
  fprintf(stdout, "PFS Stripe: %d\n",
	  pmap.pfsmap_slice.ps_count);
}
    

main(int argc,char *argv[]){
 
  struct pfsmap my_pfsmap_t;
  struct pfsmap myg_pfsmap_t;
  char *buf;
  uint32_t stripe = 0;
  uint32_t stride = 0;
  uint32_t base = 2500;
  int i = 1, stime, fd;
  struct tm *ptr;
  long ltime;
  int file_exists = 1;     /* Flag denoting a file exists        */
  int force_overwrite = 0; /* Flag to overwrite an existing file */

  /******************************************
   * Print date, time, and program identification
   ******************************************/
  ltime = time(NULL);
  ptr = localtime(&ltime);
  fprintf(stderr, "Touch Parallel File (tpf) v1.0q: %s\n", asctime(ptr));
  
  /******************************************
   * Check that at least three input parameters are input or if the user has 
   * asked for help in using the routine. IN either case, print the routine 
   * usage summary.
   ******************************************/
  if( (argc < 3) || (!strcmp(argv[1], "-help")) || (!strcmp(argv[1], "help"))){
    print_help();
    return 1;
  }
  
  /******************************************
   * Look for the user input commands: default, get, or set.
   ******************************************/
  if( !strcmp(argv[1], "default") ){
    buf = (char *)malloc(128+strlen(argv[2]));
    
    stime = (unsigned) ltime/2;
    srand(stime);
    
    sprintf(buf,"%s/__tpf%c%c%c%c",argv[2], rand(), rand(), rand(), rand());

    if( (fd = open(buf, O_RDWR|O_CREAT,S_IRUSR|S_IWUSR|S_IRGRP|S_IWGRP|S_IROTH|S_IWOTH|_S_IFREG)) == -1){
      fprintf(stderr,"ERROR: Unable to get default PFS parameters.\n");
      fprintf(stderr,"Please enter the full path to a directory that you have write privileges to.\n");
      return 1;
    }
    ioctl(fd,PFSIO_GETFSMAP,&myg_pfsmap_t);

    fprintf(stdout,"PFS parameters for %s:\n", argv[2]);
    fprintf(stdout, "Number of Component File Systems: %d\n",
	    myg_pfsmap_t.pfsmap_slice.ps_count);
    fprintf(stdout, "Default PFS Stride (bytes): %d\n",
	    myg_pfsmap_t.pfsmap_stride);
    
    ioctl(fd,PFSIO_GETDFLTMAP,&myg_pfsmap_t);

    fprintf(stdout, "Default PFS Stripe: %d\n",
	    myg_pfsmap_t.pfsmap_slice.ps_count);
    
    close(fd);
    remove(buf);
    free(buf);
  }
  else if( !strcmp(argv[1], "get") ){
    
    get_pfsmap(argv[2], &myg_pfsmap_t);
    print_params(argv[2], myg_pfsmap_t);
    
  }
  else if ( !strcmp(argv[1], "set")){

    /* ************************
     * Open the file for read/write only and, if it exists, get the default 
     * PFS mapping information.
     **************************/
    if( (fd = open(argv[2], O_RDWR,S_IRUSR|S_IWUSR|S_IRGRP|S_IWGRP|S_IROTH|S_IWOTH|_S_IFREG)) == -1){
      file_exists = 0;
      ioctl(fd,PFSIO_GETMAP,&myg_pfsmap_t);
    }
    close(fd);
    
    /* ************************
     * Parse and check for valid input parameters:
     *    starting I/O device >= 0
     *    stride >= 64KB (power of 2?)
     *    stripe > 0
     **************************/
    i = 3; 
    while (i < argc){
      if(!strcmp(argv[i], "-stripe")){
	
	if(atoi(argv[i+1]) < 1){
	  fprintf(stderr,"ERROR: The stripe must be greater than zero; %d was input.\n",
		  atoi(argv[i+1]));
	  fprintf(stderr,"ERROR: File created with default parameters.\n");
	  close(fd);
	  return 1;
	}
	
	stripe = (uint32_t)atoi(argv[i+1]);
	i += 2;
      }
      else if(!strcmp(argv[i], "-stride")){

	if(atoi(argv[i+1]) < 65536){
	  fprintf(stderr,"ERROR: The stride must be greater than or equal to 64 KB; %d was input.\n",
		  atoi(argv[i+1]));
	  fprintf(stderr,"ERROR: File created with default parameters.\n");
	  close(fd);
	  return 1;
	}
	
	stride = (uint32_t)atoi(argv[i+1]);
	i += 2;
      }
      else if(!strcmp(argv[i], "-start_iodevice")){
	
	if(atoi(argv[i+1]) < 0){
	  fprintf(stderr,"ERROR: The starting disk must be greater than or equal to zero; %d was input.\n",
		  atoi(argv[i+1]));
	  fprintf(stderr,"ERROR: File created with default parameters.\n");
	  close(fd);
	  return 1;
	}
	base = (uint32_t)atoi(argv[i+1]);
	i += 2;
      }
      else if(!strcmp(argv[i], "-force")){
	force_overwrite = 1;
	i++;
      }
      else{ /* skip unrecognized input */
	i++;
      }
    }
    
    /********************************
     * If the file exists and the force flag was not specified, then exit 
     * with a warning message.
     ********************************/
    if(!force_overwrite && file_exists){
      fprintf(stderr,"File %s exists and the force flag is not on.\n", argv[2]);
      fprintf(stderr,"File %s was not modified.\n", argv[2]);
      return 1;
    }

    /********************************
     * We can now open the file with create since the file either does not 
     * exist or the force flag was specified and get the default PFS 
     * parameters.
     ********************************/
    if( (fd = open(argv[2], O_RDWR|O_CREAT,S_IRUSR|S_IWUSR|S_IRGRP|S_IWGRP|S_IROTH|S_IWOTH|_S_IFREG)) == -1){
      fprintf(stderr,"ERROR: Unable to open file %s.\n", argv[2]);
      return 1;
    }
    ioctl(fd,PFSIO_GETMAP,&myg_pfsmap_t);

    /* ******************
     * If a value was input,set the user supplied PFS parameters. If a value 
     * was not input, use the default value.
     ********************/
    if(stripe > 0)
      myg_pfsmap_t.pfsmap_slice.ps_count =  stripe;
    if(stride > 0)
      myg_pfsmap_t.pfsmap_stride = stride;
    
    if(base < 2500)
      myg_pfsmap_t.pfsmap_slice.ps_base = base;
    
    if( ioctl(fd,PFSIO_SETMAP,&myg_pfsmap_t) == -1){
      perror("ioctl set");
      fprintf(stderr,"ERROR: Unable to set PFS attributes for file %s. File created with default parameters.\n", 
	      argv[2]);
      close(fd);
      return 1;
    }
    
    /* ********
     * Get and display the PFS variables for the input file. 
     ***********/
    if( ioctl(fd,PFSIO_GETMAP,&myg_pfsmap_t) == -1){
      perror("ioctl get");
      fprintf(stderr,"ERROR: Unable to get PFS attributes for file %s. FIle was created iwth the input PFS mapping parameters.\n", 
	      argv[2]);
      close(fd);
      return 1;
    }

    print_params(argv[2], myg_pfsmap_t);
    close(fd);
  }
  else{
    printf("ERROR: Unknown operation (%s). File operation must be get, set, or default.\n", argv[1]);
    return 1;
  }
  
  return 0;
}
