#include <stdio.h>
#include <sys/ioctl.h>
#include <sys/types.h>
#include <fcntl.h>
#include <ctype.h>

#include "pan_fs_client_cw_mode.h"
#include "pan_fs_client_statfs.h"

int mygeti(int *result){
  char c, buff [ 13 ]; /* signed 32-bit value, extra room for '\n' and '\0' */
  return fgets(buff, sizeof buff, stdin) && !isspace(*buff) &&
    sscanf(buff, "%d%c", result, &c) == 2 && (c == '\n' || c == '\0');
}

int main(int argc,char *argv[]){
  pan_fs_client_layout_create_dir_args_t dirargs;
  int fd,value,i;
  char text[20];
  char *fname, *dirname;

  bzero(&dirargs, sizeof(pan_fs_client_layout_create_dir_args_t));

  dirargs.version = PAN_FS_CLIENT_LAYOUT_VERSION;
  dirargs.child_layout.layout_is_valid = 1;

  printf("Enter Layout Type:\n1: Default\n2: RAID0\n3: RAID5\n4: RAID10\n\n>");
  mygeti(&value);
  if(value == 1){
    dirargs.child_layout.agg_type = PAN_FS_CLIENT_LAYOUT_TYPE__DEFAULT;
  }
  else if(value == 2){
    dirargs.child_layout.agg_type = PAN_FS_CLIENT_LAYOUT_TYPE__RAID0;
    printf("Enter Total Num Components:\n\n>");
    mygeti(&value);
    dirargs.child_layout.u.raid0.total_num_comps = value;
    printf("Enter Stripe Unit (bytes):\n\n>");
    mygeti(&value);
    dirargs.child_layout.u.raid0.stripe_unit = value;
  }
  else if(value == 3){
    dirargs.child_layout.agg_type = PAN_FS_CLIENT_LAYOUT_TYPE__RAID1_5_PARITY_STRIPE;
    printf("Enter Total Num Components:\n\n>");
    mygeti(&value);
    dirargs.child_layout.u.raid1_5_parity_stripe.total_num_comps = value;
    printf("Enter Stripe Unit (bytes):\n\n>");
    mygeti(&value);
    dirargs.child_layout.u.raid1_5_parity_stripe.stripe_unit = value;
    printf("Enter Stripe Width:\n\n>");
    mygeti(&value);
    dirargs.child_layout.u.raid1_5_parity_stripe.parity_stripe_width = value;
    printf("Enter Stripe Depth:\n\n>");
    mygeti(&value);
    dirargs.child_layout.u.raid1_5_parity_stripe.parity_stripe_depth = value;
    printf("Enter Layout Visit Policy\n(1:ROUND_ROBIN 2:ROUND_ROBIN_WITH_SINGLE_OFFSET 3:ROUND_ROBIN_WITH_HASHED_OFFSET:\n\n>");
    mygeti(&value);
    dirargs.child_layout.u.raid1_5_parity_stripe.layout_visit_policy = value;
  }
  else if(value == 4){
    dirargs.child_layout.agg_type = PAN_FS_CLIENT_LAYOUT_TYPE__RAID10;
    printf("Enter Total Num Components:\n\n>");
    mygeti(&value);
    dirargs.child_layout.u.raid10.total_num_comps = value;
    printf("Enter Stripe Unit (bytes):\n\n>");
    mygeti(&value);
    dirargs.child_layout.u.raid10.stripe_unit = value;
    printf("Enter Layout Visit Policy\n(1:ROUND_ROBIN 2:ROUND_ROBIN_WITH_SINGLE_OFFSET 3:ROUND_ROBIN_WITH_HASHED_OFFSET:\n\n>");
    mygeti(&value);
    dirargs.child_layout.u.raid10.layout_visit_policy = value;
  } 
  else{
    printf("invalid entry\n");
    return 0;
  }

  //printf("Enter Directory mode (permissions):\n\n>");
  //mygeti(&value);
  dirargs.mode = 493;

  fname = strrchr(argv[1], '/');
  if(fname == NULL){
    fprintf(stderr,"Error: Please enter the full path to be created.\n");
    return 1;
  }
 
  i = strlen(argv[1]) - strlen(fname);
  dirname = (char *)malloc((i+1)*sizeof(char));
  strncpy(dirname, argv[1], i);
  dirname[i] = '\0';
  fname++;

  //printf("fname: %s\ndirname: %s\n", fname, dirname);

  strcpy(dirargs.dirname, fname);
  //snprintf(dirargs.dirname, PAN_FS_CLIENT_CW_MODE_FILE_NAME_LEN_MAX, "%s", argv[1]);

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
