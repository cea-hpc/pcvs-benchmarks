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
  pan_fs_client_statfs_extended_res_t fsquery;

  bzero(&query, sizeof(pan_fs_client_layout_query_args_t));
  query.version = PAN_FS_CLIENT_LAYOUT_VERSION;

  printf("Opening %s...\n", argv[1]);
  if( (fd = open(argv[1],O_RDONLY)) == -1){
    fprintf(stderr,"Unable to open file.\n");
    return 1;
  }
  
  if( ioctl(fd, PAN_FS_CLIENT_LAYOUT_QUERY_FILE, &query) == -1 ){
    fprintf(stderr,"Unable to get the panfs parameters for %s.\n", argv[1]);
    close(fd);
    return 1;
  }

  if( ioctl(fd, PAN_FS_CLIENT_STATFS_EXTENDED, &fsquery) == -1 ){
    fprintf(stderr,"Unable to get the extended panfs parameters for %s.\n", argv[1]);
    close(fd);
    return 1;
  }

  close(fd);

  printf("Client Layout Version: %u\n",query.version);
  printf("Layout is valid flag: %d\n",query.layout.layout_is_valid);
  if(query.layout.agg_type == PAN_FS_CLIENT_LAYOUT_TYPE__INVALID){
    printf("RAID type: Invalid (%d)\n",query.layout.agg_type);
  }
  else if(query.layout.agg_type == PAN_FS_CLIENT_LAYOUT_TYPE__DEFAULT){
    printf("RAID type: Default (%d)\n",query.layout.agg_type);
  }
  else if(query.layout.agg_type == PAN_FS_CLIENT_LAYOUT_TYPE__RAID0){
    printf("RAID type: RAID 0\n");
    printf("Total Number of Components: %u\n", 
            query.layout.u.raid0.total_num_comps);
    printf("Stride (bytes): %u\n\n", 
            query.layout.u.raid0.stripe_unit);
  }
  else if(query.layout.agg_type == PAN_FS_CLIENT_LAYOUT_TYPE__RAID1_5_PARITY_STRIPE){
    printf("RAID type: RAID 5 Parity Stripe\n");
    printf("Total Number of Components: %u\n",
            query.layout.u.raid1_5_parity_stripe.total_num_comps);
    printf("Stride (bytes): %u\n",
            query.layout.u.raid1_5_parity_stripe.stripe_unit);
    printf("RAID width: %u\n",
            query.layout.u.raid1_5_parity_stripe.parity_stripe_width);
    printf("Depth: %u\n",
            query.layout.u.raid1_5_parity_stripe.parity_stripe_depth);
    printf("Layout Policy: %u\n",
            (unsigned int)query.layout.u.raid1_5_parity_stripe.layout_visit_policy);
  }
  else if(query.layout.agg_type == PAN_FS_CLIENT_LAYOUT_TYPE__RAID10){
    printf("RAID type: RAID 10\n");
    printf("Total Number of Components: %u\n", 
            query.layout.u.raid10.total_num_comps);
    printf("Stride (bytes): %u\n", 
            query.layout.u.raid10.stripe_unit);
    printf("Layout Policy: %u\n", 
            query.layout.u.raid10.layout_visit_policy);
  }
  else{
    printf("Unknown RAID type (agg_type = %d)\n",
            query.layout.agg_type);
  }

  printf("\n");
  printf("Struct Version: %u\n", fsquery.struct_version);
  printf("Mount Name: %s\n", fsquery.mount_from_name);
  printf("Mount Name Length: %i\n", fsquery.mount_from_name_len);
  printf("Volume ID: %lu\n", fsquery.volume_id);
  printf("Bladeset ID: %lu\n", fsquery.bladeset_id);
  printf("Bladeset Storageblade Count: %lu\n", fsquery.bladeset_storageblade_count);
  printf("Bladeset Total Bytes: %ju\n", fsquery.bladeset_total_bytes);
  printf("Bladeset Free Bytes: %ju\n", fsquery.bladeset_free_bytes);
  printf("Bladeset Unreserved Total Bytes: %ju\n", fsquery.bladeset_unreserved_total_bytes);
  printf("Bladeset Unreserved Free Bytes: %ju\n", fsquery.bladeset_unreserved_free_bytes);
  printf("Bladeset Recon Spare Total Bytes: %ju\n", fsquery.bladeset_recon_spare_total_bytes);
  printf("Volume Live Bytes Used: %ju\n", fsquery.volume_live_bytes_used);
  printf("Volume Snapshot Bytes Used: %ju\n", fsquery.volume_snapshot_bytes_used);
  printf("Volume Hard Quota (bytes): %ju\n", fsquery.volume_hard_quota_bytes);
  printf("Volume Soft Quota (bytes): %ju\n", fsquery.volume_soft_quota_bytes);
  
  return 0;
}
