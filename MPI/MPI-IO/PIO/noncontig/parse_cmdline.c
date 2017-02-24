#include <stdlib.h>



#ifndef lint 
static char vcid[] = "$Id: parse_cmdline.c,v 1.2 2005/07/01 21:34:17 robl Exp $";
#endif

/* 
  This file contains routines for processoing options of the form
  -name <value>.  In order to simplify processing by other handlers, 
  the routines eliminate the values from the argument string by compressing
  it.
 */

#include <string.h>
#include <stdio.h>
#include <stdio.h>           /* For error handling */
/*@C
   SYArgSqueeze - Remove all null arguments from an arg vector; 
   update the number of arguments.
 @*/
void SYArgSqueeze( Argc, argv )
int  *Argc;
char **argv;
{
int argc, i, j;
    
/* Compress out the eliminated args */
argc = *Argc;
j    = 0;
i    = 0;
while (j < argc) {
    while (argv[j] == 0 && j < argc) j++;
    if (j < argc) argv[i++] = argv[j++];
    }
/* Back off the last value if it is null */
if (!argv[i-1]) i--;
*Argc = i;
}

/*@C
   SYArgFindName -  Find a name in an argument list.

   Input Parameters:
+  argc - number of arguments
.  argv - argument vector
-  name - name to find

   Returns:
   index in argv of name; -1 if name is not in argv
 @*/
int SYArgFindName( argc, argv, name )
int  argc;
char **argv;
char *name;
{
int  i;

for (i=0; i<argc; i++) {
    if (strcmp( argv[i], name ) == 0) return i;
    }
return -1;
}

/*@C
  cmdline_get_int - Get the value (integer) of a named parameter.
  
  Input Parameters:
+ Argc  - pointer to argument count
. argv  - argument vector
. rflag - if true, remove the argument and its value from argv
- val   - pointer to value (will be set only if found)

  Returns:
  1 on success

  Note:
  This routine handles both decimal and hexidecimal integers.
@*/
int cmdline_get_int( Argc, argv, rflag, name, val )
int  *Argc, rflag, *val;
char **argv, *name;
{
int idx;
char *p;

idx = SYArgFindName( *Argc, argv, name );
if (idx < 0) return 0;

if (idx + 1 >= *Argc) {
    fprintf(stderr,"Error: %s\n","Missing value for argument" );
    return 0;
    }

p = argv[idx+1];
/* Check for hexidecimal value */
if (((int)strlen(p) > 1) && p[0] == '0' && p[1] == 'x') {
    sscanf( p, "%i", val );
    }
else {
    if ((int)strlen(p) > 1 && p[0] == '-' && p[1] >= 'A' && p[1] <= 'z') {
        fprintf(stderr,"Error: %s\n","Missing value for argument" );	
        return 0;
        }	
    *val = atoi( p );
    }

if (rflag) {
    argv[idx]   = 0;
    argv[idx+1] = 0;
    SYArgSqueeze( Argc, argv );
    }
return 1;
}

/*@C
  cmdline_get_double - Get the value (double) of a named parameter.
  
  Input Parameters:
+ Argc  - pointer to argument count
. argv  - argument vector
. rflag - if true, remove the argument and its value from argv
- val   - pointer to value (will be set only if found)

  Returns:
  1 on success
@*/
int cmdline_get_double( Argc, argv, rflag, name, val )
int    *Argc, rflag;
char   **argv, *name;
double *val;
{
int idx;
extern double atof();

idx = SYArgFindName( *Argc, argv, name );
if (idx < 0) return 0;

if (idx + 1 >= *Argc) {
    fprintf(stderr,"Error: %s\n","Missing value for argument" );
    return 0;
    }

*val = atof( argv[idx+1] );
if (rflag) {
    argv[idx]   = 0;
    argv[idx+1] = 0;
    SYArgSqueeze( Argc, argv );
    }
return 1;
}

/*@C
  cmdline_get_string - Get the value (string) of a named parameter.
  
  Input Parameters:
+ Argc  - pointer to argument count
. argv  - argument vector
. rflag - if true, remove the argument and its value from argv
. val   - pointer to buffer to hold value (will be set only if found).
- vallen- length of val
 
  Returns:
  1 on success
@*/
int cmdline_get_string( Argc, argv, rflag, name, val, vallen )
int  *Argc, rflag, vallen;
char **argv, *name, *val;
{
int idx;

idx = SYArgFindName( *Argc, argv, name );
if (idx < 0) return 0;

if (idx + 1 >= *Argc) {
    fprintf(stderr,"Error: %s\n","Missing value for argument" );
    return 0;
    }

strncpy( val, argv[idx+1], vallen );
if (rflag) {
    argv[idx]   = 0;
    argv[idx+1] = 0;
    SYArgSqueeze( Argc, argv );
    }
return 1;
}

/*@C
  cmdline_has_name - Return 1 if name is in argument list
  
  Input Parameters:
+ Argc  - pointer to argument count
. argv  - argument vector
. rflag - if true, remove the argument and its value from argv
- name  - name to search for

  Returns:
  1 on success
@*/
int cmdline_has_name( Argc, argv, rflag, name )
int  *Argc, rflag;
char **argv, *name;
{
int idx;

idx = SYArgFindName( *Argc, argv, name );
if (idx < 0) return 0;

if (rflag) {
    argv[idx]   = 0;
    SYArgSqueeze( Argc, argv );
    }
return 1;
}

/*@C
  cmdline_get_intvec - Get the value (integers) of a named parameter.
  
  Input Parameters:
+ Argc  - pointer to argument count
. argv  - argument vector
. rflag - if true, remove the argument and its value from argv
. n     - number of values to read
- val   - pointer to value (will be set only if found)

  Note: 
  The form of input is "-name n1 n2 n3 ..."
  Returns:
  1 on success
@*/
int cmdline_get_intvec( Argc, argv, rflag, name, n, val )
int  *Argc, rflag, *val, n;
char **argv, *name;
{
int idx, i;

idx = SYArgFindName( *Argc, argv, name );
if (idx < 0) return 0;

/* Fail if there aren't enough values */
if (idx + n + 1 > *Argc) {
    fprintf(stderr,"Error: %s\n","Not enough values for vector of integers");
    return 0;
    }

for (i=0; i<n; i++) {
    val[i] = atoi( argv[idx+i+1] );
    if (rflag) {
	argv[idx+i+1] = 0;
	}
    }
if (rflag) {
    argv[idx]   = 0;
    SYArgSqueeze( Argc, argv );
    }

return 1;
}

/*@C
  cmdline_get_intlist - Get the value (integers) of a named parameter.
  
  Input Parameters:
+ Argc  - pointer to argument count
. argv  - argument vector
. rflag - if true, remove the argument and its value from argv
. n     - maximum number of values to read
- val   - pointer to values (will be set only if found)

  Note: 
  The form of input is "-name n1,n2,n3 ..."

  Returns:
  Number of elements found.  0 if none or error (such as -name with 
  no additional arguments)
@*/
int cmdline_get_intlist( Argc, argv, rflag, name, n, val )
int  *Argc, rflag, *val, n;
char **argv, *name;
{
int  idx, i;
char *p, *pcomma;

idx = SYArgFindName( *Argc, argv, name );
if (idx < 0) return 0;

/* Fail if there aren't enough values */
if (idx + 2 > *Argc) {
    fprintf(stderr,"Error: %s\n","Not enough values for vector of integers");
    return 0;
    }

p = argv[idx + 1];
i = 0;
while (i + 1 < n && p && *p) {
    /* Find next comma or end of value */
    pcomma = strchr( p, ',' );
    if (pcomma) {
	pcomma[0] = 0;
	pcomma++;
	}
    val[i++] = atoi( p );
    p      = pcomma;
    }

if (rflag) {
    argv[idx]   = 0;
    argv[idx+1] = 0;
    SYArgSqueeze( Argc, argv );
    }

return i;
}
