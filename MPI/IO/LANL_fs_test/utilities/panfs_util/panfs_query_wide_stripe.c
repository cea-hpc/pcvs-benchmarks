#include <stdio.h>
#include <sys/ioctl.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <fcntl.h>

#include "pan_fs_client_cw_mode.h"
#include "pan_fs_client_statfs.h"

int main(int argc,char *argv[]) {
  pan_fs_client_layout_query_args_t query;
  int fd;
  long int max;
  char *ptr;

  max = strtol(argv[2], &ptr, 10);
  if(max == 0 && ptr == argv[2])
    perror("Bad max value");

  bzero(&query, sizeof(pan_fs_client_layout_query_args_t));
  query.version = PAN_FS_CLIENT_LAYOUT_VERSION;

  if( (fd = open(argv[1],O_RDONLY)) == -1){
    fprintf(stderr,"Unable to open file.\n");
    return 1;
  }
  
  if( ioctl(fd, PAN_FS_CLIENT_LAYOUT_QUERY_FILE, &query) == -1 ){
    fprintf(stderr,"Unable to get the panfs parameters for %s.\n", argv[1]);
    close(fd);
    return 1;
  }

  close(fd);

  if(query.layout.layout_is_valid != 1) { 
    printf("Layout is invalid\n");
    return 1;
  }
  if(query.layout.agg_type == PAN_FS_CLIENT_LAYOUT_TYPE__INVALID){
    printf("RAID type: Invalid (%d)\n",query.layout.agg_type);
  }
  else if(query.layout.agg_type == PAN_FS_CLIENT_LAYOUT_TYPE__RAID0){
    if(query.layout.u.raid0.total_num_comps > max)
      printf("%s: %u\n", 
	     argv[1],
	     query.layout.u.raid0.total_num_comps);
  }
  else if(query.layout.agg_type == PAN_FS_CLIENT_LAYOUT_TYPE__RAID1_5_PARITY_STRIPE){
    if(query.layout.u.raid1_5_parity_stripe.total_num_comps > max)
      printf("%s: %u\n",
	     argv[1],
	     query.layout.u.raid1_5_parity_stripe.total_num_comps);
  }
  else if(query.layout.agg_type == PAN_FS_CLIENT_LAYOUT_TYPE__RAID10){
    if(query.layout.u.raid10.total_num_comps > max)
      printf("%s: %u\n",
	     argv[1],
	     query.layout.u.raid10.total_num_comps);
  }
  else{
    printf("Unknown RAID type (agg_type = %d)\n",
            query.layout.agg_type);
  }

  return 0;
}
