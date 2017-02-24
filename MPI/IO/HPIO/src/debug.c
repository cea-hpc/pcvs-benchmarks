/* (C) Northwestern University
 * See COPYING in the top-level directory . */

#include <stdarg.h>
#include "debug.h"

/* Turn on/off debugging */
int debug_mode      = 0;
/* Mask for deciding which messages are printed */
uint64_t debug_mask = 0;

typedef struct
{
    char *keyword;
    uint64_t mask_val;
}  keyword_mask_t;

static keyword_mask_t keyword_mask_map[] =
{
    {"correct", MASK_CORRECT},
    {"correct_info", MASK_CORRECT_INFO},
    {"filenames", MASK_FILENAMES},
    {"resume", MASK_RESUME},
    {"parse_args", MASK_PARSE_ARGS},
    {"io", MASK_IO},
    {"average", MASK_AVERAGE},
    {"multiple_processes", MASK_MULTIPLE_PROCESSES},
    {"human", MASK_HUMAN}
};

static const int keyword_mask_map_ct = (int)       \
(sizeof(keyword_mask_map) / sizeof(keyword_mask_t));

int has_mask(uint64_t mask_val)
{
    return (debug_mask & mask_val);
}

uint64_t debug_convert_to_mask(const char *keyword_str)
{
    uint64_t mask = 0;
    char *s = NULL, *t = NULL;
    const char *toks = ", ";
    int i = 0, negate = 0;

    if (keyword_str)
    {
	s = strdup(keyword_str);
        t = strtok(s, toks);
	
        while(t)
        {
            if (*t == '-')
            {
                negate = 1;
                ++t;
            }

            for(i = 0; i < keyword_mask_map_ct; i++)
            {
                if (!strcmp(t, keyword_mask_map[i].keyword))
                {
                    if (negate)
                    {
                        mask &= ~keyword_mask_map[i].mask_val;
                    }
                    else
                    {
                        mask |= keyword_mask_map[i].mask_val;
                    }
                    break;
                }
            }
            t = strtok(NULL, toks);
        }
        free(s);
    }
    return mask;
}

int print_debug_mask(void)
{
    int i;

    fprintf(stdout, "mask: ");    
    for (i = 0; i < keyword_mask_map_ct; i++)
    {
	if ((1 << i) & debug_mask)
	    fprintf(stdout, "1");
	else
	    fprintf(stdout, "0");
    }
    fprintf(stderr, "\n");
    for (i = 0; i < keyword_mask_map_ct; i++)
    {
	if ((1 << i) & debug_mask)
	{
	    fprintf(stdout, "mask index %d: %s\n", 
		    i, keyword_mask_map[i].keyword);
	}
    }

    return 0;
}

int debug_mode_set(int mode)
{
    assert(mode == DEBUG_ON || 
	   mode == DEBUG_OFF);
	   
    debug_mode = mode;
    return 0;
}

int mask_fprintf(uint64_t mask, 
		 FILE *stream,
		 const char *format,
		 ...)
{
    va_list ap;

    /* fix the variable arguments */
    va_start(ap, format);

    if (debug_mode == DEBUG_ON && debug_mask & mask)
    {
	vfprintf(stream, format, ap);
	fflush(stream);
    }

    va_end(ap);

    return 0;
}

int mask_stdout(uint64_t mask, 
		const char *format,
		...)
{
    va_list ap;

    /* fix the variable arguments */
    va_start(ap, format);

    if (debug_mode == DEBUG_ON && debug_mask & mask)
    {
	vfprintf(stdout, format, ap);
	fflush(stdout);
    }

    va_end(ap);

    return 0;
}

int mask_stderr(uint64_t mask, 
		const char *format,
		...)
{
    va_list ap;

    /* fix the variable arguments */
    va_start(ap, format);

    if (debug_mode == DEBUG_ON && debug_mask & mask)
    {
	vfprintf(stderr, format, ap);
	fflush(stderr);
    }

    va_end(ap);

    return 0;
}

int mask_stdout_stream(FILE *stream,
		       uint64_t mask, 
		       const char *format,
		       ...)
{
    va_list ap;

    /* fix the variable arguments */
    va_start(ap, format);

    if (debug_mode == DEBUG_ON && debug_mask & mask)
    {
	vfprintf(stdout, format, ap);
	fflush(stdout);
	
	vfprintf(stream, format, ap);
	fflush(stream);
    }

    va_end(ap);

    return 0;
}

int mask_stderr_stream(FILE *stream,
		       uint64_t mask, 
		       const char *format,
		       ...)
{
    va_list ap;

    /* fix the variable arguments */
    va_start(ap, format);

    if (debug_mode == DEBUG_ON && debug_mask & mask)
    {
	vfprintf(stderr, format, ap);
	fflush(stderr);

	vfprintf(stream, format, ap);
	fflush(stream);
    }

    va_end(ap);

    return 0;
}

int debug_fprintf(FILE *stream,
		  const char *format,
		  ...)
{
    va_list ap;
    
    /* fix the variable arguments */
    va_start(ap, format);

    vfprintf(stream, format, ap);
    fflush(stream);

    va_end(ap);

    return 0;
}

int debug_stdout(const char *format,
		 ...)
{
    va_list ap;

    /* fix the variable arguments */
    va_start(ap, format);

    vfprintf(stdout, format, ap);
    fflush(stdout);

    va_end(ap);

    return 0;
}

int debug_stderr(const char *format,
		 ...)
{
    va_list ap;

    /* fix the variable arguments */
    va_start(ap, format);

    vfprintf(stderr, format, ap);
    fflush(stderr);

    va_end(ap);

    return 0;
}

int debug_stdout_stream(FILE *stream,
			const char *format,
			...)
{
    va_list ap;

    /* fix the variable arguments */
    va_start(ap, format);

    vfprintf(stdout, format, ap);
    fflush(stdout);

    vfprintf(stream, format, ap);
    fflush(stream);

    va_end(ap);

    return 0;
}

int debug_stderr_stream(FILE *stream,
			const char *format,
			...)
{
    va_list ap;

    /* fix the variable arguments */
    va_start(ap, format);

    vfprintf(stderr, format, ap);
    fflush(stderr);

    vfprintf(stream, format, ap);
    fflush(stream);

    va_end(ap);

    return 0;
}

/*
 * Local variables:
 *  mode: c
 *  c-indent-level: 4
 *  c-basic-offset: 4
 * End:
 *
 * vim: ft=c ts=8 sts=4 sw=4 expandtab
 */
