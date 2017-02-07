
#include <getopt.h>
#include <stdio.h>
#include <sys/ioctl.h>
#include <sys/types.h>
#include <fcntl.h>
#include <ctype.h>
#include <stdlib.h>
#include <string.h>
#include <strings.h>
#include <unistd.h>
#include "pan_fs_client_cw_mode.h"
#include "pan_fs_client_statfs.h"

int
main (argc, argv)
     int argc;
     char **argv;
{
  int c, fd, i;
  int width = 0;
  int size = 0;
  pan_fs_client_layout_create_dir_args_t dirargs;
  char *fname;
  char *dirname;;
 
  while (1) {
    static struct option long_options[] = {
      {"width",  required_argument, 0, 'w'},
      {"size",   required_argument, 0, 's'},
      {"help",   no_argument,       0, 'h'},
      {0, 0, 0}
    };

    /* getopt_long stores the option index here. */
    int option_index = 0;
      
    c = getopt_long (argc, argv, "w:s:h",
		     long_options, &option_index);

    /* Detect the end of the options. */
    if (c == -1)
      break;
      
    switch (c) {
      case 'w':
        width = atoi(optarg);
        break;
      case 's':
        size = atoi(optarg);
	break;
      default:
      case 'h':
        fprintf(stderr, "Create a RAID0 directory on Panasas with specified parameters.\n\n");
        fprintf(stderr, "%s -s SIZE -w WIDTH TARGET\n", argv[0]);
        fprintf(stderr, "  SIZE   - stripe size in bytes\n");
        fprintf(stderr, "  WIDTH  - total components allowed in a file\n");
        fprintf(stderr, "  TARGET - full path to target directory to create (must not exist already)\n");
        exit(0);
	break;
    }
  }

  if (size == 0 || width == 0 || !argv[optind]) {
    fprintf(stderr, "Must specify -w <width> -s <size> <full path to target>\n");
    exit(1);
  }
  
  bzero(&dirargs, sizeof(pan_fs_client_layout_create_dir_args_t));

  dirargs.version = PAN_FS_CLIENT_LAYOUT_VERSION;
  dirargs.child_layout.layout_is_valid = 1;

  dirargs.child_layout.agg_type = PAN_FS_CLIENT_LAYOUT_TYPE__RAID0;
  dirargs.child_layout.u.raid0.total_num_comps = width;
  dirargs.child_layout.u.raid0.stripe_unit = size;

  dirargs.mode = 480;

  fname = strrchr(argv[optind], '/');
  if(fname == NULL){
    fprintf(stderr,"Error: Please enter the full path to be created.\n");
    return 1;
  }
 
  i = strlen(argv[optind]) - strlen(fname);
  dirname = (char *)malloc((i+1)*sizeof(char));
  strncpy(dirname, argv[optind], i);
  dirname[i] = '\0';
  fname++;

  strcpy(dirargs.dirname, fname);

  if( (fd = open(dirname,O_RDONLY)) == -1){
    perror("Unable to open directory");
    return 1;
  }
  
  if( ioctl(fd, PAN_FS_CLIENT_LAYOUT_CREATE_DIR, &dirargs) == -1 ){
    perror("Unable to set the panfs parameters");
    if(close(fd) == -1)
      perror("Error closing fd.");
    return 1;
  }
  
  if(close(fd) == -1){
      perror("Error closing fd.");
      return 1;
  }

  return 0;
}
