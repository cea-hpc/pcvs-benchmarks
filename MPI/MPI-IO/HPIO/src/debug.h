/* (C) Northwestern University
 * See COPYING in the top-level directory . */

#ifndef DEBUG_H
#define DEBUG_H

#define _GNU_SOURCE
#include "mpi.h"
#include <mpi.h>
#include <getopt.h>
#include <string.h>
#include <stdio.h>
#include <unistd.h>
#include <stdio.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <fcntl.h>
#include <assert.h>
#include <errno.h>
#include <stdlib.h>
#include <sys/time.h>
#include <unistd.h>
#include <limits.h>

#ifdef __KERNEL__
#include <linux/types.h>
#else
#include <stdint.h>
#endif

#define DEBUG_ON  0
#define DEBUG_OFF 1

#define MASK_NONE               (uint64_t)0
#define MASK_CORRECT            ((uint64_t) 1 << 0)
#define MASK_CORRECT_INFO       ((uint64_t) 1 << 1)
#define MASK_FILENAMES          ((uint64_t) 1 << 2)
#define MASK_RESUME             ((uint64_t) 1 << 3)
#define MASK_PARSE_ARGS         ((uint64_t) 1 << 4)
#define MASK_IO                 ((uint64_t) 1 << 5)
#define MASK_AVERAGE            ((uint64_t) 1 << 6)
#define MASK_MULTIPLE_PROCESSES ((uint64_t) 1 << 7)
#define MASK_HUMAN              ((uint64_t) 1 << 8)

#define MASK_ALL (uint64_t)                                      \
(MASK_CORRECT + MASK_FILENAMES + MASK_RESUME + MASK_PARSE_ARGS + \
 MASK_IO + MASK_AVERAGE)

int has_mask(uint64_t mask_val);
uint64_t debug_convert_to_mask(const char *keyword_str);
int print_debug_mask(void);
int debug_mode_set(int mode);
int mask_fprintf(uint64_t mask, 
		 FILE *stream,
		 const char *format,
		 ...);
int mask_stdout(uint64_t mask, 
		const char *format,
		...);
int mask_stderr(uint64_t mask, 
		const char *format,
		...);
int mask_stdout_stream(FILE *stream,
		       uint64_t mask, 
		       const char *format,
		       ...);
int mask_stderr_stream(FILE *stream,
		       uint64_t mask, 
		       const char *format,
		       ...);
int debug_fprintf(FILE *stream,
		  const char *format,
		  ...);
int debug_stdout(const char *format,
		 ...);
int debug_stderr(const char *format,
		 ...);
int debug_stdout_stream(FILE *stream,
			const char *format,
			...);
int debug_stderr_stream(FILE *stream,
			const char *format,
			...);


#endif

/*
 * Local variables:
 *  mode: c
 *  c-indent-level: 4
 *  c-basic-offset: 4
 * End:
 *
 * vim: ft=c ts=8 sts=4 sw=4 expandtab
 */
