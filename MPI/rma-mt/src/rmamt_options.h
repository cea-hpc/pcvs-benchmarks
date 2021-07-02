
#if !defined(OPTIONS_H)
#define OPTIONS_H

#include <stdbool.h>

#define MAX_THREADS 512
#define RMAMT_MAX_SIZE (1 << 22)

enum {
  RMAMT_PUT,
  RMAMT_GET,
  RMAMT_OPERATIONS_MAX,
};

extern char *rmamt_operation_strings[];

enum {
  RMAMT_LOCK_ALL,
  RMAMT_FENCE,
  RMAMT_LOCK,
  RMAMT_FLUSH,
  RMAMT_PSCW,
  RMAMT_ALL_FLUSH,
  RMAMT_FLUSH_ALL,
  RMAMT_FLUSH_LOCAL,
  RMAMT_FLUSH_LOCAL_ALL,
  RMAMT_SYNC_MAX,
};

extern char *rmamt_sync_strings[];

extern bool rmamt_win_per_thread;
extern bool rmamt_use_ibarrier;
extern int rmamt_threads;
extern int rmamt_iterations;
extern unsigned rmamt_sleep_interval;
extern int rmamt_sync;
extern int rmamt_operation;
extern unsigned long rmamt_max_size;
extern unsigned long rmamt_min_size;
extern bool rmamt_bind_threads;
extern char *rmamt_output_file;

int rmamt_parse_options (const char *name, int argc, char *argv[]);


#endif /* !defined(OPTIONS_H) */
