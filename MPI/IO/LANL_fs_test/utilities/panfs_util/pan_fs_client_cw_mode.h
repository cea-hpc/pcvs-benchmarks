/*
 * pan_fs_client_cw_mode.h
 * 
 * IOCTL interface into the PanFS client for concurrent write services
 *
 * @author  ehogan
 * @version 1.0
 *
 */
/*
 * Copyright (c) 1999-2006 Panasas, Inc.  All rights reserved.  See the
 * LICENSE file included in this package for licensing details.
 */
#ifndef _PAN_FS_CLIENT__PAN_FS_CLIENT_CW_MODE_H_
#define _PAN_FS_CLIENT__PAN_FS_CLIENT_CW_MODE_H_

#ifndef KERNEL

#if (__FreeBSD__ > 0)
#include <sys/ioccom.h>
#endif /* (__FreeBSD__ > 0) */

#if (__linux__ > 0)
#include <sys/ioctl.h>
#endif /* (__linux__ > 0) */

#endif /* !defined(KERNEL) */

/* Files can be opened in concurrent write mode using the
 * following flag to open(). */
#define O_CONCURRENT_WRITE                         020000000000

/*---------------------------------------------------------------*/
/*---------------------------------------------------------------*/
                        
#define PAN_FS_CLIENT_CW_MODE_IOCTL                ((unsigned int)0x24)

/*---------------------------------------------------------------*/
/*---------------------------------------------------------------*/

#define PAN_FS_CLIENT_CW_MODE_FLUSH_F__NONE        0x00000000
#define PAN_FS_CLIENT_CW_MODE_FLUSH_F__INVAL_BUFS  0x00000001
#define PAN_FS_CLIENT_CW_MODE_FLUSH_F__INVAL_ATTRS 0x00000002

typedef struct pan_fs_client_cw_mode_flush_args_s pan_fs_client_cw_mode_flush_args_t;
struct pan_fs_client_cw_mode_flush_args_s {
  unsigned int     flags;
};

/**
 * An ioctl for flushing cached writes to stable storage. The ioctl can also be
 * used to invalidate the contents of client's buffer cache after flushing.
 * 
 * @author  ehogan
 * @version 1.0
 *
 * @since   1.0
 */
#define PAN_FS_CLIENT_CW_MODE_FLUSH \
  _IOWR(PAN_FS_CLIENT_CW_MODE_IOCTL,60,pan_fs_client_cw_mode_flush_args_t)

/*---------------------------------------------------------------*/
/*---------------------------------------------------------------*/

#define PAN_FS_CLIENT_LAYOUT_VERSION               2

#define PAN_FS_CLIENT_CW_MODE_FILE_NAME_LEN_MAX    255

#define PAN_FS_CLIENT_LAYOUT_CREATE_F__NONE        0x00000000
#define PAN_FS_CLIENT_LAYOUT_CREATE_DIR_F__NONE    0x00000000

enum pan_fs_client_layout_agg_type_e {
  PAN_FS_CLIENT_LAYOUT_TYPE__INVALID                 = 0,
  PAN_FS_CLIENT_LAYOUT_TYPE__DEFAULT                 = 1,
  PAN_FS_CLIENT_LAYOUT_TYPE__RAID0                   = 2,
  PAN_FS_CLIENT_LAYOUT_TYPE__RAID1_5_PARITY_STRIPE   = 3,
  PAN_FS_CLIENT_LAYOUT_TYPE__RAID10                  = 4
};
typedef enum pan_fs_client_layout_agg_type_e pan_fs_client_layout_agg_type_t;

enum pan_fs_client_layout_visit_e {
  PAN_FS_CLIENT_LAYOUT_VISIT__INVALID                          = 0,
  PAN_FS_CLIENT_LAYOUT_VISIT__ROUND_ROBIN                      = 1,
  PAN_FS_CLIENT_LAYOUT_VISIT__ROUND_ROBIN_WITH_SINGLE_OFFSET   = 2,
  PAN_FS_CLIENT_LAYOUT_VISIT__ROUND_ROBIN_WITH_HASHED_OFFSET   = 3
};
typedef enum pan_fs_client_layout_visit_e pan_fs_client_layout_visit_t;

typedef struct pan_fs_client_layout_s pan_fs_client_layout_t;
struct pan_fs_client_layout_s {
  pan_fs_client_layout_agg_type_t  agg_type;
  int                              layout_is_valid;
  union {
    struct {
      unsigned int                     total_num_comps;
      unsigned int                     stripe_unit;
    } raid0;
    struct {
      unsigned int                     total_num_comps;
      unsigned int                     stripe_unit;
      unsigned int                     parity_stripe_width;
      unsigned int                     parity_stripe_depth;
      pan_fs_client_layout_visit_t     layout_visit_policy;
    } raid1_5_parity_stripe;
    struct {
      unsigned int                     total_num_comps;
      unsigned int                     stripe_unit;
      pan_fs_client_layout_visit_t     layout_visit_policy;
    } raid10;
  } u;
};

typedef struct pan_fs_client_layout_create_args_s pan_fs_client_layout_create_args_t;
struct pan_fs_client_layout_create_args_s {
  unsigned short                   version;
  char                             filename[PAN_FS_CLIENT_CW_MODE_FILE_NAME_LEN_MAX+1];
  unsigned int                     mode;
  pan_fs_client_layout_t           layout;
  unsigned int                     flags;
};

typedef struct pan_fs_client_layout_create_dir_args_s pan_fs_client_layout_create_dir_args_t;
struct pan_fs_client_layout_create_dir_args_s {
  unsigned short                   version;
  char                             dirname[PAN_FS_CLIENT_CW_MODE_FILE_NAME_LEN_MAX+1];
  unsigned int                     mode;
  pan_fs_client_layout_t           child_layout;
  unsigned int                     flags;
};

typedef struct pan_fs_client_layout_query_args_s pan_fs_client_layout_query_args_t;
struct pan_fs_client_layout_query_args_s {
  unsigned short                   version;
  pan_fs_client_layout_t           layout;
};

/**
 * An ioctl that can be used to create files of a specified storage layout
 * in a PanFS Filesystem. The target of the ioctl should be a directory in
 * which to create the new file.
 * 
 * @author  ehogan
 * @version 3.0
 *
 * @since   1.0
 */
#define PAN_FS_CLIENT_LAYOUT_CREATE_FILE \
  _IOWR(PAN_FS_CLIENT_CW_MODE_IOCTL,61,pan_fs_client_layout_create_args_t)

/**
 * An ioctl that can be used to create a dir in which all future child files
 * created will be of a specified layout. This ioctl does not alter the layout
 * of the directory itself.
 * 
 * @author  ehogan
 * @version 3.0
 *
 * @since   1.0
 */
#define PAN_FS_CLIENT_LAYOUT_CREATE_DIR \
  _IOWR(PAN_FS_CLIENT_CW_MODE_IOCTL,62,pan_fs_client_layout_create_dir_args_t)

/**
 * An ioctl that can be used to query the storage layout of a filesystem object
 * 
 * @author  ehogan
 * @version 3.0
 *
 * @since   1.0
 */
#define PAN_FS_CLIENT_LAYOUT_QUERY_FILE \
  _IOWR(PAN_FS_CLIENT_CW_MODE_IOCTL,63,pan_fs_client_layout_query_args_t)

#endif /* _PAN_FS_CLIENT__PAN_FS_CLIENT_CW_MODE_H_ */

/* Local Variables:  */
/* indent-tabs-mode: nil */
/* tab-width: 2 */
/* End: */
