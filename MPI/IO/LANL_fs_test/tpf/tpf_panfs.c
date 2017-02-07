#ifndef _KMEMUSER
#define _KMEMUSER
#endif
#include <stdio.h>
#include <stdlib.h>
#include <sys/ioctl.h>
#include <string.h>
//#include <stropts.h>
#include <sys/stat.h>
#include <sys/errno.h>
#include <sys/types.h>
#include <aio.h>
#include <sys/time.h>
#include <sys/param.h>
#include <fcntl.h>
#include <signal.h>
#include <errno.h>
#include <unistd.h>
#include <netdb.h>
#include <iostream>
using namespace std;
 
#include "pan_fs_client_cw_mode.h"
#include "pan_fs_client_statfs.h"
 
/*****************************************
 *
 *  TODO: 1. Add input for visit policy
 *        2. Add get umask from user environment for "mode"
 *        3. Complete help/documentation
 *
 ****************************************/
void print_help(){
  fprintf(stderr,"Touch Parallel File(tpf): Set or get the file mapping \n"
	  "attributes for the Panasas file system (panfs).\n\n");
  fprintf(stderr,"The tpf utility can be used to\n"
          "(1) get the default file attributes for an existing directory, \n"
          "(2) get the panfs attributes for an existing file, and \n"
          "(3) set the panfs attributes for a new file or directory. If the \"set\" option\n"
          "is called with an existing file name, a warning message will be \n"
          "printed and no action will take place. If the \"-force\" flag is\n"
          "used with the \"set\" option and if the file exists, it will be\n"
          "destroyed and recreated with the user provided attributes. In\n"
          "this case all data in the existing file will be lost.\n"
	  "The force option cannot be used with directories, i.e. a directory cannot be deleted and recreated with new parameters.\n\n");
 
  fprintf(stderr,"Usage:\n");
  fprintf(stderr,"tpf default write_path\n");
  fprintf(stderr,"where write_path is the path to a directory in a panfs file\n"
          "system where the user has write privileges. A file will be created\n"
	  "and removed in this directory.\n\n");
  
  fprintf(stderr,"tpf get file_name\n"
          "where file_name is the full path and name of an existing file in a panfs file system.\n\n");
  
  fprintf(stderr,"tpf set file_name -force -dir -stripe # -stride # -raid # -depth # -totalcomp #\n"
          "where file_name is the full path and filename, or, just a directory (when setting\n"
          "up directory attributes)\n\n");

  fprintf(stderr,"all flags are optional and are defined as:\n"
          "\"-force\" allows the user to destroy and recreate a file with\n"
          "\t the specified input parameters.\n"
	  "\"-dir\" specifies that a directory should be created with the\n"
	  "\t specified parameters. All files created in this directory will\n"
	  "\t have the supplied attributes.\n"
          "\"-stripe\" is for RAID 5 only and is the number of storage blades (disks) to write your data across.\n"
          "\"-stride\" is the number of bytes of data written to one disk\n"
          "\t before writing to the next disk. The stride must be at least\n"
          "\t 1 KB (1024 bytes).\n"
	  "\"-totalcomp\" the total number of blades that the file will be striped over.\n"
	  "\"-raid\" is the RAID level of the file to be created. Allowable\n"
	  "\t RAID levels: RAID 0 (\"0\"), RAID 5 (\"5\") or RAID 10 (\"10\")\n"
	  "Note: RAID 5 is actually a parity stripe file, if the file is smaller than\n"
	  "\t64 KB, the file will be mirrored and not striped.\n"
	  "\"-depth\" is used with RAID 5 and is the number of stripes\n"
	  "\twritten to a RAID before moving onto the next one and must be greater than zero.\n"
	  );
}
 
int print_params(char *fname, pan_fs_client_layout_query_args_t query_file, pan_fs_client_statfs_extended_res_t ext_query_file){
  fprintf(stdout,"panfs parameters for: %s\n", fname);
  fprintf(stdout,"Client Layout Version: %u\n",query_file.version);
  fprintf(stdout,"Layout is valid flag: %d\n",query_file.layout.layout_is_valid);
  fprintf(stdout,"Layout Type: %d\n",query_file.layout.agg_type);
  if(query_file.layout.agg_type == PAN_FS_CLIENT_LAYOUT_TYPE__INVALID){
    fprintf(stdout,"RAID type: Invalid (%d)\n",query_file.layout.agg_type);
  }
  else if(query_file.layout.agg_type == PAN_FS_CLIENT_LAYOUT_TYPE__DEFAULT){
    fprintf(stdout,"RAID type: Default (%d)\n",query_file.layout.agg_type);
  }
  else if(query_file.layout.agg_type == PAN_FS_CLIENT_LAYOUT_TYPE__RAID0){
    fprintf(stdout,"RAID type: RAID 0\n");
    fprintf(stdout,"Total Number of Components: %u\n", 
	    query_file.layout.u.raid0.total_num_comps);
    fprintf(stdout,"Stride (bytes): %u\n\n", 
	    query_file.layout.u.raid0.stripe_unit);
  }
  else if(query_file.layout.agg_type == PAN_FS_CLIENT_LAYOUT_TYPE__RAID1_5_PARITY_STRIPE){
    fprintf(stdout,"RAID type: RAID 5 Parity Stripe\n");
    fprintf(stdout,"Total Number of Components: %u\n",
	    query_file.layout.u.raid1_5_parity_stripe.total_num_comps);
    fprintf(stdout,"Stride (bytes): %u\n",
	    query_file.layout.u.raid1_5_parity_stripe.stripe_unit);
    fprintf(stdout,"RAID width: %u\n",
	    query_file.layout.u.raid1_5_parity_stripe.parity_stripe_width);
    fprintf(stdout,"Depth: %u\n",
	    query_file.layout.u.raid1_5_parity_stripe.parity_stripe_depth);
    fprintf(stdout,"Layout Policy: %u\n",
	    (unsigned int)query_file.layout.u.raid1_5_parity_stripe.layout_visit_policy);
  }
  else if(query_file.layout.agg_type == PAN_FS_CLIENT_LAYOUT_TYPE__RAID10){
    fprintf(stdout,"RAID type: RAID 10\n");
    fprintf(stdout,"Total Number of Components: %u\n", 
	    query_file.layout.u.raid10.total_num_comps);
    fprintf(stdout,"Stride (bytes): %u\n", 
	    query_file.layout.u.raid10.stripe_unit);
    fprintf(stdout,"Layout Policy: %u\n", 
	    query_file.layout.u.raid10.layout_visit_policy);
  }
  else{
    fprintf(stdout,"Unknown RAID type (agg_type = %d)\n",
	    query_file.layout.agg_type);
  }
  fprintf(stdout,"Maximum number of components available: %llu\n",ext_query_file.bladeset_storageblade_count);
  
  return 0;
}

int get_defaults(char *dirname, pan_fs_client_layout_query_args_t *query_file, pan_fs_client_statfs_extended_res_t *ext_query_file){

  pan_fs_client_layout_create_args_t create_file;
  pan_fs_client_layout_create_dir_args_t create_dir;
  
  int i = 1;
  int fd = 0;
  char *fname = NULL;
  char *rand_dirname = NULL;
  long ltime;
  unsigned int stime;

  struct stat status; 

    /*******************************************
     * Seed the random number generator and create a file with default 
     * panfs attributes to query.
     *************************************/
    ltime = time(NULL);
    stime = (unsigned) ltime/2;
    srand(stime);
    
    fname = (char *)malloc(128*sizeof(char));
    rand_dirname = (char *)malloc(132*sizeof(char) + strlen(dirname));
    sprintf(fname,"__tpf%u", rand());
    sprintf(rand_dirname,"%s/%s", dirname, fname);
    
    if( (fd = open(dirname, O_RDONLY)) == -1){
      fprintf(stderr,"ERROR: Unable to open directory %s.\n", dirname);
      return 1;
    }
    
    bzero(&create_file, sizeof(pan_fs_client_layout_create_args_t));
    create_file.version = PAN_FS_CLIENT_LAYOUT_VERSION;
    strcpy(create_file.filename, fname);
    create_file.mode = 420;
    create_file.layout.agg_type = PAN_FS_CLIENT_LAYOUT_TYPE__DEFAULT;
    create_file.layout.layout_is_valid = 1;
    
    if( ioctl(fd, PAN_FS_CLIENT_LAYOUT_CREATE_FILE, &create_file) == -1 ){
      fprintf(stderr,"ERROR: Unable to create file %s (%s).\n", 
              fname, strerror(errno) );
      close(fd);
      return 1;
    }
    close(fd);
    
    /*********************************************************
     * Open the newly created file and query it's panfs file attributes.
     ******************************************************/
    if( (fd = open(rand_dirname,O_RDONLY)) == -1){
      fprintf(stderr,"ERROR: Unable to open file %s.\n", rand_dirname);
      return 1;
    }

    if( ioctl(fd, PAN_FS_CLIENT_LAYOUT_QUERY_FILE, query_file) == -1 ){
      fprintf(stderr,"ERROR: Unable to get the panfs parameters for file %s.\n", fname);
      return 1;
    }
    if( ioctl(fd, PAN_FS_CLIENT_STATFS_EXTENDED, ext_query_file) == -1 ){
      fprintf(stderr,"ERROR: Unable to get the panfs parameters for file %s.\n", fname);
      return 1;
    }
    close(fd);

    remove(rand_dirname);

    /* XXXXX */  
  /* stat the new file to get the permissions */

    sprintf(fname,"__tpf%u", rand());
    sprintf(rand_dirname,"%s/%s", dirname, fname);

    if( (fd = open(rand_dirname,O_CREAT)) == -1){
      fprintf(stderr,"TEST: Unable to open file %s.\n", rand_dirname);
      return 1;
    }
    close(fd);
    stat(rand_dirname, &status);
    remove(rand_dirname);
    /*
    fprintf(stderr,"Status mode = %lu minus 32768 = %lu\n", 
	    status.st_mode, status.st_mode - 32768);
    */
    /* XXXXX */  

    free(fname);
    free(rand_dirname);

    return 0;
}


main(int argc,char *argv[]){
  
  pan_fs_client_layout_query_args_t query_file;
  pan_fs_client_layout_create_args_t create_file;
  pan_fs_client_layout_create_dir_args_t create_dir;
  pan_fs_client_layout_t layout;
  pan_fs_client_statfs_extended_res_t ext_query_file;
 
  int i = 1;
  int fd = 0;
  char *rand_fname = NULL;
  char *fname = NULL;
  char *rand_dirname, *dirname = NULL;
  unsigned int stime, stripe = 8;
  unsigned long stride = 65536;
  unsigned int total_comps = 8;
  unsigned int depth = 2000;
  pan_fs_client_layout_agg_type_t raid_type = PAN_FS_CLIENT_LAYOUT_TYPE__DEFAULT;       
  /* RAID option choosen by user. Defaults to DEFAULT type */ 
  int fmode = -1;
  int make_dir = 0;
  int file_exists = 1;     /* Flag denoting a file exists        */
  int force_overwrite = 0; /* Flag to overwrite an existing file */
 
  char fsobj[15];
  struct tm *ptr;
  long ltime;

  /******************************************
   * Print date, time, and program identification
   ******************************************/
  ltime = time(NULL);
  ptr = localtime(&ltime);
  fprintf(stderr, "Touch Parallel File (tpf) v1.0p: %s\n", asctime(ptr));

  /******************************************
   * Check that at least three input parameters are present or if the user has 
   * asked for help in using the routine. In either case, print the 
   * usage summary.
   ******************************************/
  if(argc < 3){
    fprintf(stderr,"ERROR: At least two inputs are required; an action (set, get, or default) and a path or file name.\n");
    print_help();
    return 1;
  }
  if( (argc < 3) || (!strcmp(argv[1], "-help")) || (!strcmp(argv[1], "help"))){
    print_help();
    return 1;
  }
 
  /******************************************
   * Look for the user input commands: default, get, or set.
   ******************************************/
  if( !strcmp(argv[1], "default") ){
 
    /*********************************************************
     * Call the routines to get the 
     ******************************************************/
    bzero(&query_file, sizeof(pan_fs_client_layout_query_args_t));
    query_file.version = PAN_FS_CLIENT_LAYOUT_VERSION;
    if(get_defaults(argv[2], &query_file, &ext_query_file)){
      fprintf(stderr,"ERROR: Problem getting default file parameters in %s.\n",
	      argv[2]);
      return 1;
    }
    if(print_params(argv[2], query_file, ext_query_file)){
      fprintf(stderr,"ERROR: Problem printing default file parameters for %s.\n",
	      argv[2]);
      return 1;
    }

  }
  else if( !strcmp(argv[1], "get") ){
 
    /***********************************
     * Open the file to get the panfs file attributes.
     *******************************************/
    if( (fd = open(argv[2],O_RDONLY)) == -1){
      fprintf(stderr,"ERROR: Unable to open file %s .\n", argv[2]);
      return 1;
    }
    
    bzero(&query_file, sizeof(pan_fs_client_layout_query_args_t));
    query_file.version = PAN_FS_CLIENT_LAYOUT_VERSION;

    if( ioctl(fd,PAN_FS_CLIENT_LAYOUT_QUERY_FILE, &query_file) == -1 ){
      fprintf(stderr,"ERROR: Unable to get the panfs parameters for file %s .\n", argv[2]);
      close(fd);
      return 1;
    }
    close(fd);
    

    if(print_params(argv[2], query_file, ext_query_file)){
      fprintf(stderr,"ERROR: Problem printing default file parameters for %s.\n",
	      argv[2]);
      return 1;
    }
    
  }
  else if ( !strcmp(argv[1], "set")){
 
    
    /* ************************
     * Parse and check for valid input parameters:
     *    stride > 1024 bytes (1 KB). Panasas allows for a stripe of 1 byte
     *             or above. The 1 KB requirement is an artificial minimum.
     *    stripe > 1 and < 10. For RAID1, stripe = 2.
     *    raid = 0 or 5
     **************************/
    i = 3; 
    while (i < argc){
      if(!strcmp(argv[i], "-stripe")){
	/*
        if( (atoi(argv[i+1]) < 2) || (atoi(argv[i+1]) > 10)){
          fprintf(stderr,"ERROR: The stripe must be greater than one and less than 11; %d was input.\n",
                  atoi(argv[i+1]));
          fprintf(stderr,"ERROR: File was not created.\n");
          close(fd);
          return 1;
        }
	*/
        stripe = (unsigned int)atoi(argv[i+1]);
        i += 2;
      }
      else if(!strcmp(argv[i], "-stride")){
        stride = strtoul(argv[i+1], NULL, 10);
        if(stride  < 1024){
            cerr << "ERROR: The stride must be greater than or equal to 1024 bytes; "
                 << argv[i+1] << " was input (parsed " << stride << ")" << endl;
          fprintf(stderr,"ERROR: File was not created.\n");
          close(fd);
          return 1;
        }
        i += 2;
      }
      else if(!strcmp(argv[i], "-raid")){
        if( !strcmp(argv[i+1], "0")){
	  raid_type = PAN_FS_CLIENT_LAYOUT_TYPE__RAID0;
        }
        else if( !strcmp(argv[i+1], "5")){
	  raid_type = PAN_FS_CLIENT_LAYOUT_TYPE__RAID1_5_PARITY_STRIPE;
        }
        else if( !strcmp(argv[i+1], "10")) {
	  raid_type = PAN_FS_CLIENT_LAYOUT_TYPE__RAID10;
        }
        else{
          fprintf(stderr,"ERROR: Invalid RAID type entered: %s\n", argv[i+1]);
          fprintf(stderr,"Valid RAID types are RAID 0 ('0') or RAID 5 ('5').\n");
          return 1;
        }
        i += 2;
      }
      else if(!strcmp(argv[i], "-totalcomp")){
	if(atoi(argv[i+1]) < 1){
	  fprintf(stderr,"ERROR: Total number of components must be greater than 0; %d entered.\n", 
		  atoi(argv[i+1]));
          return 1;
	}
        total_comps = (unsigned int)atoi(argv[i+1]);
        i += 2;
      }
      else if(!strcmp(argv[i], "-depth")){
	if(atoi(argv[i+1]) < 1){
	  fprintf(stderr,"ERROR: RAID 5 depth must be greater than 0; %d entered.\n", 
		  atoi(argv[i+1]));
          return 1;
	}
        depth = (unsigned int)atoi(argv[i+1]);
        i += 2;
      }
      else if(!strcmp(argv[i], "-force")){
        force_overwrite = 1;
        i++;
      }
      else if(!strcmp(argv[i], "-dir")){
        make_dir = 1;
        i++;
      }
      else if(!strcmp(argv[i], "-help")){
	print_help();
	return 1;
      }
      else{ /* skip unrecognized input */
        fprintf(stderr,"Skipping unrecognized input: %s\n", argv[i]);
        i++;
      } 
    }
  
    /* ************************
     * Set the string for all print statements
     **************************/
    if(make_dir) strcpy(fsobj, "directory");
    else         strcpy(fsobj, "file");

    /* ************************
     * Try to open the directory/file for read/write only and, if it exists, 
     * set the overwrite flag. If the directory/file exists and the force flag
     * was not specified, then exit with a warning message. Else delete the 
     * file/directory.
     **************************/
    if( (fd = open(argv[2], O_RDWR,S_IRUSR|S_IWUSR|S_IRGRP|S_IWGRP|S_IROTH|S_IWOTH)) == -1){
      file_exists = 0;
    } 
    else{
      close(fd); 

      if(!force_overwrite){
	fprintf(stderr,"ERROR: %s %s exists and the force flag is not set.\n",
		fsobj, argv[2]);
	fprintf(stderr,"%s %s was not modified.\n", fsobj, argv[2]);
	return 1;
      }
      else{
	if(remove(argv[2])){
	fprintf(stderr,"ERROR: Unable to remove %s %s.\n", fsobj, argv[2]);
	return 1;
	}

      }
    }
    
    /********************************************************
     * Manipulate input file name to get the directory name. fname will now 
     * contain the file name with a '/' in front of it and dirname will 
     * contain the directory to create the file in. Remove the '/' before
     * the file name
     ************************************************************/
    fname = strrchr(argv[2], '/');
    if(fname == NULL){
      fprintf(stderr,"Error: Please enter the full path to the %s to be created.\n", fsobj);
      return 1;
    }
 
    i = strlen(argv[2]) - strlen(fname);
    dirname = (char *)malloc((i+1)*sizeof(char));
    strncpy(dirname, argv[2], i);
    dirname[i] = '\0';
    fname++;
    
    /*********************************************************
     * Get the default file system file layout parameters
     ******************************************************/
    bzero(&query_file, sizeof(pan_fs_client_layout_query_args_t));
    query_file.version = PAN_FS_CLIENT_LAYOUT_VERSION;

    if(get_defaults(dirname, &query_file, &ext_query_file)){
      fprintf(stderr,"Error: Problem getting default file parameters.\n");
      return 1;
    }

    /********************************
     * Set parameters for the new directory/file and set the layout 
     * structure to the defaults
     ********************************/
    if(make_dir){
      bzero(&create_dir, sizeof(pan_fs_client_layout_create_dir_args_t));
      create_dir.version = PAN_FS_CLIENT_LAYOUT_VERSION;
      strcpy(create_dir.dirname, fname); 
      /*
      if(fmode > -1 ) create_dir.mode = fmode;
      else            create_dir.mode = 0744;
      */
      create_dir.mode = 493;
    }
    else{
      bzero(&create_file, sizeof(pan_fs_client_layout_create_args_t));
      create_file.version = PAN_FS_CLIENT_LAYOUT_VERSION;
      strcpy(create_file.filename, fname);
      /* Mode is currently set to default rw-r--r--
      if(fmode > -1 ) create_file.mode = fmode;
      else            create_file.mode = 0644;
      */
      create_file.mode = 420;
    }
    layout = query_file.layout;
    
    /********************************
     * Overwrite all default parameters with user supplied parameters
     ********************************/
    layout.agg_type = (pan_fs_client_layout_agg_type_t)raid_type;

    if(layout.agg_type == PAN_FS_CLIENT_LAYOUT_TYPE__RAID0){
      layout.u.raid0.stripe_unit = stride;
      layout.u.raid0.total_num_comps = total_comps;
      fprintf(stderr,"Trying to make RAID0 stride = %ld (%ld) total comps %ld\n", 
	      layout.u.raid0.stripe_unit, stride, layout.u.raid0.total_num_comps);
	      
    }
    else if(layout.agg_type == PAN_FS_CLIENT_LAYOUT_TYPE__RAID1_5_PARITY_STRIPE){
      layout.u.raid1_5_parity_stripe.stripe_unit = stride;
      layout.u.raid1_5_parity_stripe.parity_stripe_width = stripe; 
      layout.u.raid1_5_parity_stripe.parity_stripe_depth = depth;
      layout.u.raid1_5_parity_stripe.total_num_comps = total_comps; 
      
      /******************************
       * Only one RAID Group layout specified; round robin. When more are
       * available, this should be made available to users.XXXXXX
       *****************************/
      layout.u.raid1_5_parity_stripe.layout_visit_policy = (pan_fs_client_layout_visit_t)1;
    }
    else if(layout.agg_type == PAN_FS_CLIENT_LAYOUT_TYPE__RAID10){
      layout.u.raid10.stripe_unit = stride;
      layout.u.raid10.total_num_comps = total_comps;
      layout.u.raid10.layout_visit_policy = (pan_fs_client_layout_visit_t)1;
      fprintf(stderr,"RAID10 stride = %ld (%ld)total comps %ld layout visit policy %ld\n", 
	      layout.u.raid10.stripe_unit, stride, layout.u.raid10.total_num_comps, 
              layout.u.raid10.layout_visit_policy);
	      
    }

    else{
      fprintf(stderr,"ERROR: Unrecognized RAID layout (%d).\n", 
	      layout.agg_type);
      return 1;
    }

    /********************************
     * Open the directory and create the user requested directory/file.
     ********************************/
    if( (fd = open(dirname,O_RDONLY)) == -1){
      fprintf(stderr,"ERROR: Unable to open the directory %s.\n", dirname);
      return 1;
    }
    
    if(make_dir){
      create_dir.child_layout = layout;
      if( ioctl(fd, PAN_FS_CLIENT_LAYOUT_CREATE_DIR, &create_dir) == -1 ){
	fprintf(stderr,"ERROR: Unable to create directory %s.\n", fname);
	close(fd);
	return 1;
      }
    }
    else{
      create_file.layout = layout;
      if( ioctl(fd, PAN_FS_CLIENT_LAYOUT_CREATE_FILE, &create_file) == -1 ){
	fprintf(stderr,"ERROR: Unable to create file %s: %s\n", fname, strerror(errno));
	close(fd);
	return 1;
      }
    }

    close(fd);
  }
  else{
    fprintf(stderr, "ERROR: File operation must be default, get, or set, not: %s\n", argv[1]);
    return 1;
  }
 
  return 0;
}
