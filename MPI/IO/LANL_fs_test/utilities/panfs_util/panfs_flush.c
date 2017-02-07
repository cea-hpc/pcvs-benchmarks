#include <stdio.h>
#include <sys/ioctl.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <fcntl.h>

#include "pan_fs_client_cw_mode.h"

int main(int argc,char *argv[]) {
  int fd;  
  pan_fs_client_cw_mode_flush_args_t flushargs;

  bzero(&flushargs, sizeof(pan_fs_client_cw_mode_flush_args_t));

  printf("Flushing %s...\n", argv[1]);
  if( (fd = open(argv[1],O_RDONLY)) == -1){
    fprintf(stderr,"Unable to open file.\n");
    perror("ERROR");
    return 1;
  }
  
  flushargs.flags = PAN_FS_CLIENT_CW_MODE_FLUSH_F__INVAL_BUFS;

  if( ioctl(fd, PAN_FS_CLIENT_CW_MODE_FLUSH, &flushargs) == -1 ){
    fprintf(stderr,"Unable to get the panfs parameters for %s.\n", argv[1]);
    close(fd);
    perror("ERROR");
    return 1;
  }

  flushargs.flags = PAN_FS_CLIENT_CW_MODE_FLUSH_F__INVAL_ATTRS;

  if( ioctl(fd, PAN_FS_CLIENT_CW_MODE_FLUSH, &flushargs) == -1 ){
    fprintf(stderr,"Unable to get the panfs parameters for %s.\n", argv[1]);
    close(fd);
    perror("ERROR");
    return 1;
  }

  close(fd);

  return 0;
}
