/*
 * pan_fs_client_statfs.h
 * 
 * IOCTL interface into the PanFS client for extended statfs information
 *
 * @author  ehogan
 * @version 1.0
 *
 */
/*
 * Copyright (c) 1999-2006 Panasas, Inc.  All rights reserved.  See the
 * LICENSE file included in this package for licensing details.
 */
#ifndef _PAN_FS_CLIENT__PAN_FS_CLIENT_STATFS_H_
#define _PAN_FS_CLIENT__PAN_FS_CLIENT_STATFS_H_

#ifndef KERNEL

#if (__FreeBSD__ > 0)
#include <sys/ioccom.h>
#endif /* (__FreeBSD__ > 0) */

#if (__linux__ > 0)
#include <sys/ioctl.h>
#endif /* (__linux__ > 0) */

#endif /* !defined(KERNEL) */

#if defined(_MSC_EXTENSIONS)
typedef unsigned __int64      pan_fs_client_statfs_uint64_t;
#elif (__LP64__ > 0)
typedef unsigned long         pan_fs_client_statfs_uint64_t;
#else /* (__LP64__ > 0) */
typedef unsigned long long    pan_fs_client_statfs_uint64_t;
#endif /* (__LP64__ > 0) */

#define PAN_FS_CLIENT_STATFS_IOCTL                    ((unsigned int)0x24)

#define PAN_FS_CLIENT_STATFS_MAX_MOUNT_NAME_LEN       256

#define PAN_FS_CLIENT_STATFS_VERSION_V1               1

typedef struct pan_fs_client_statfs_extended_res_v1_s pan_fs_client_statfs_extended_res_v1_t;
typedef pan_fs_client_statfs_extended_res_v1_t        pan_fs_client_statfs_extended_res_t;

/*
 * extended statfs() ioctl results structure
 *
 * struct_version
 *    Version number for results struct.  Currently must be 1.
 *
 * mount_from_name
 *    The device from where we mount the panfs file system.
 *
 * mount_from_name_len
 *    The actual length of the above field.
 *
 * volume_id
 *    Opaque volume identifier.  Two objects in the same volume are
 *    guaranteed to have the same volume_id.  Two objects in different
 *    volumes are guaranteed to have different volume_ids.
 *
 * bladeset_id
 *    Opaque bladeset identifier.  Two objects in the same BladeSet
 *    are guaranteed to have the same bladeset_id.  Two objects in
 *    different BladeSets are guaranteed to have different
 *    bladeset_ids.
 *
 * bladeset_storageblade_count
 *    number of StorageBlades in this object's BladeSet.
 *
 * bladeset_total_bytes
 *    Total number of bytes in the bladeset, including all reserved
 *    space, space used by this and other volumes, and all free space.
 *
 * bladeset_free_bytes
 *    Number of bytes in the bladeset not currently in use.  Includes
 *    space that is not in use but is reserved for reconstruction
 *    spares.
 *
 * bladeset_unreserved_total_bytes
 *    Total number of bytes in the bladeset available for user data,
 *    not including space reserved for reconstruction spares.
 *
 * bladeset_unreserved_free_bytes
 *    Number of bytes in the bladeset not currently in use and
 *    available for user data.  Does not include space that is
 *    reserved for reconstruction spares (used or free).
 *
 * bladeset_recon_spare_total_bytes
 *    Total number of bytes in the bladeset reserved for
 *    reconstruction spares.
 *
 * volume_live_bytes_used
 *    Number of bytes used by this volume's "live" data (i.e., not
 *    including snapshots).
 *
 * volume_snapshot_bytes_used
 *    Number of bytes used by this volume's snapshot(s).  This field
 *    is currently not supported and will always be returned as 0.
 *
 * volume_hard_quota_bytes
 *    Hard quota for this volume, in bytes.  If the volume does not
 *    have a hard quota this field will be 0.
 *
 * volume_soft_quota_bytes
 *    Soft quota for this volume, in bytes.  If the volume does not
 *    have a soft quota this field will be 0.
 *
 * filler
 *    Reserved for future expansion.
 *
 * Note that the amount of space available in a volume for new files
 * is limited both by the volume's hard quota, and by the space
 * remaining in the bladeset.  The total amount of space, and amount
 * of space remaining in the volume that is available for users, can
 * be computed as:
 *
 * if(volume_hard_quota_bytes > 0) {
 *   if(volume_hard_quota_bytes >= volume_live_bytes_used) {
 *     free_space = MIN(bladeset_unreserved_free_bytes,
 *                      volume_hard_quota_bytes - volume_live_bytes_used);
 *   }
 *   else {
 *     free_space = volume_hard_quota_bytes - volume_live_bytes_used;
 *   }
 *   total_space = MIN(bladeset_unreserved_free_bytes,
 *                     volume_hard_quota_bytes);
 * }
 * else {
 *   free_space = bladeset_unreserved_free_bytes;
 *   total_space = bladeset_unreserved_total_bytes;
 * } 
 *
 * Due to metadata & parity overhead, the amount of additional file
 * data that can be written into the volume will generally be less
 * than the free_space figure.
 */
struct pan_fs_client_statfs_extended_res_v1_s {
  unsigned int                    struct_version;

  char                            mount_from_name[PAN_FS_CLIENT_STATFS_MAX_MOUNT_NAME_LEN];
  int                             mount_from_name_len;

  unsigned long                   volume_id;
  unsigned long                   bladeset_id;

  unsigned long                   bladeset_storageblade_count;
  pan_fs_client_statfs_uint64_t   bladeset_total_bytes;
  pan_fs_client_statfs_uint64_t   bladeset_free_bytes;
  pan_fs_client_statfs_uint64_t   bladeset_unreserved_total_bytes;
  pan_fs_client_statfs_uint64_t   bladeset_unreserved_free_bytes;
  pan_fs_client_statfs_uint64_t   bladeset_recon_spare_total_bytes;

  pan_fs_client_statfs_uint64_t   volume_live_bytes_used;
  pan_fs_client_statfs_uint64_t   volume_snapshot_bytes_used;
  pan_fs_client_statfs_uint64_t   volume_hard_quota_bytes;
  pan_fs_client_statfs_uint64_t   volume_soft_quota_bytes;

  unsigned long                   filler[16];
};

/**
 * An ioctl used to collect extended statfs information about a mountpoint
 * 
 * @author  ehogan
 * @version 1.0
 *
 * @since   1.0
 */
#define PAN_FS_CLIENT_STATFS_EXTENDED \
  _IOWR(PAN_FS_CLIENT_STATFS_IOCTL,80,pan_fs_client_statfs_extended_res_t)

#define PAN_FS_CLIENT_STATFS_MIN(_a_,_b_) (((_a_)<(_b_))?(_a_):(_b_))

/**
 * Compute the effective total & free values for a volume based
 * on STATFS_EXTENDED results.
 *
 * @in  unres_total_bytes
 *    total unreserved space
 * @in  unres_free_bytes
 *    free unreserved space
 * @in  hard_quota_bytes
 *    size of the volume's hard quota, or 0 if no quota
 * @in  live_bytes
 *    space consumed by the volume's "live" data (vs. snapshots)
 * @out total
 *    effective total size of the volume (in bytes)
 * @out free
 *    effective free space in the volume (in bytes)
 */
#define pan_fs_client_statfs_compute_vol(_unres_total_bytes_,_unres_free_bytes_,_hard_quota_bytes_,_live_bytes_,_total_,_free_) { \
  if((_hard_quota_bytes_) == 0) { \
    *(_total_)  = (_unres_total_bytes_); \
    *(_free_)   = (_unres_free_bytes_); \
  } \
  else if((_live_bytes_) <= (_hard_quota_bytes_)) { \
    *(_total_)  = PAN_FS_CLIENT_STATFS_MIN((_hard_quota_bytes_), (_unres_total_bytes_)); \
    *(_free_)   = PAN_FS_CLIENT_STATFS_MIN((_hard_quota_bytes_) - (_live_bytes_), \
                                           (_unres_free_bytes_)); \
  } \
  else { \
    *(_total_)  = (_live_bytes_); \
    *(_free_)   = 0; \
  } \
}

#endif /* _PAN_FS_CLIENT__PAN_FS_CLIENT_STATFS_H_ */

/* Local Variables:  */
/* indent-tabs-mode: nil */
/* tab-width: 2 */
/* End: */
