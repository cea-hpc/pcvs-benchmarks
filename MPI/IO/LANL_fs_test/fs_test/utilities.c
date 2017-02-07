/* $Id: utilities.c,v 1.6 2011/09/27 16:46:46 atorrez Exp $ */
#include <arpa/inet.h>
//#include <stropts.h>
#include <ctype.h>
#include <errno.h>
#include <fcntl.h>
#include <libgen.h>
#include <limits.h>
#include <netinet/in.h>
#include <stdarg.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/file.h>
#include <sys/socket.h>
#include <sys/stat.h>
#include <sys/statvfs.h>
#include <sys/time.h>
#include <sys/types.h>
#include <sys/types.h>
#include <sys/uio.h>
#include <sys/un.h>
#include <unistd.h>
#include <assert.h>
#include <time.h>
//#include <malloc.h>
#define _XOPEN_SOURCE 500
//#define __USE_FILE_OFFSET64
#include <unistd.h>
#include "mpi.h"
#include "utilities.h"

#if defined(MYSQL_HOST) || defined(MYSQL_FILE)
#if defined(MYSQL_HOST)
#include <mysql.h>
#endif
#include <sys/utsname.h>
#include <netdb.h>
#include <sys/socket.h>
#include <sys/types.h>
#include <pwd.h>
#include <string.h>
#endif

#define BIG_FILE_OFFSET 8

/*******************************************
 *
 * The purpose of this global variable is to make checks for whether or not the
 * application was invoked with the "-nodb" option. It is possible that this is
 * the case, even when the MYSQL* environment variables are defined.
 *
 * This variable is static so that it cannot be referenced from outside this
 * file, but it is in the scope of all the functions of this file.
 *
 *******************************************/
#if defined(MYSQL_HOST) || defined(MYSQL_FILE)
static int database_enabled = 1;
#else
static int database_enabled = 0;
#endif

/*******************************************
 *   ROUTINE: set_using_db
 *   PURPOSE: This function is used for a client application to set whether or
 *            not it is using the database, irrespective of whether or not the
 *            application was compiled with the MYSQL* environment variables
 *            set to some value. zero means FALSE, non-zero means TRUE.
 *
 *   DATE: November 5, 2010
 *   LAST MODIFIED: November 5, 2010 [brettk]
 *******************************************/
void set_using_db( int using_db ) {
  database_enabled = using_db;
}

#define EXIT_UNLESS_DB(X) if(!database_enabled) return(X);

/*******************************************
 *   ROUTINE: parse_size
 *   PURPOSE: Convert character inputs to number bytes. Allow 
 *            for (Linux) dd-like expressions; w 2, b 512, KB 1000, K 1024, 
 *            MB 1,000,000,  M 1,048,576, GB 1,000,000,000, and G 1,073,741,824
 *            A positive number followed by one of the above letter will 
 *            result in the two multiplied, i.e. 2b => 1024.
 *
 *   DATE: February 19, 2004
 *   LAST MODIFIED: September 7, 2005 [jnunez]
 *                  October 8, 2009 Removed find_panfs_client and
 *                                  removed code from collect_additional_
 *                                  config so that env_to_db.tcsh can
 *                                  now do exteranlly
 *******************************************/
int parse_size(int my_rank, char *chbytes, long long int *out_value){

  char last, next_last;
  unsigned long long int insize = 0;
  
  last = chbytes[strlen(chbytes) - 1];
  next_last = chbytes[strlen(chbytes) - 2];
  
  if( !isdigit(last))
    switch(last){
    case 'w':
      if(insize == 0) insize = 2;
    case 'b':
      if(insize == 0) insize = 512;
    case 'K':
      if(insize == 0) insize = 1024;
    case 'M':
      if(insize == 0) insize = 1048576;
    case 'G':
      if( !isdigit(next_last)){
	fprintf(stderr,"[RANK %d] ERROR: Unknown multiplicative suffix (%c%c) for input string %s.\n", 
		my_rank, next_last, last, chbytes);
	return 1;
      }
      chbytes[strlen(chbytes) - 1] = '\0';
      if(insize == 0) insize = 1073741824;
      break;
    case 'B':
      if( isdigit(next_last)){
	fprintf(stderr,"[RANK %d] ERROR: Unknown multiplicative suffix (%c) for input string %s.\n", 
		my_rank, last, chbytes);
	return 1;
      }
      
      chbytes[strlen(chbytes) - 1] = '\0';
      last = chbytes[strlen(chbytes) - 1];
      next_last = chbytes[strlen(chbytes) - 2];
      
      if( !isdigit(next_last)){
	fprintf(stderr,"[RANK %d] ERROR: Unknown multiplicative suffix (%cB) for input string %s.\n", 
		my_rank, last, chbytes);
	return 1;
      }
      
      switch(last){
      case 'K':
	if(insize == 0) insize = 1000;
      case 'M':
	if(insize == 0) insize = 1000000;
      case 'G':
	if(insize == 0) insize = 10000000000ULL;
	chbytes[strlen(chbytes) - 1] = '\0';
	break;
      default:
	fprintf(stderr,"[RANK %d] ERROR: Unknown multiplicative suffix (%cB) for input string %s.\n", 
		my_rank, last, chbytes);
	return 1;
	break;
      }
      
      break;
    default:
      fprintf(stderr,"[RANK %d] ERROR: Unknown multiplicative suffix (%c) for input string %s.\n", 
	      my_rank, last, chbytes);
      return 1;
    }
  else{
    insize = 1;
  }

  /*  XXXX Must find way to handle decimal points instead of atol
  fprintf(stderr,"chbytes = %s atol = %ld\n", chbytes, atol(chbytes));
  */
  *out_value = insize * (unsigned long long)atol(chbytes);

  return 0;
}

/***************************************************************************
*
*  FUNCTION:  string_to_int_array
*  PURPOSE:   Parse an input string, with user defined delimined substrings,
*             into an array of integers. The original input string is not
*             modified by this routine.
*
*  PARAMETERS:
*             char *input_string    - Input string to parse containing
*                                     deliminted substrings
*             char *delimiter       - String used to parse the input string.
*                                     May be multiple characters, ex " *:"
*             int *num_args         - Number of substrings contained in the
*                                     input string
*             int **output_ints     - Array of integers contained in the
*                                     input string
*             int *sum              - Sum of the integers in the string
*
*  RETURN:    TRUE(1) on successful exit; FALSE(0) otherwise
*  HISTORY:   03/05/01 [jnunez] Original version.
*
****************************************************************************/
int string_to_int_array(char *input_string, char *delimiter,
                           int *num_args, int **output_ints, int *sum){

  char *temp_string = NULL;
  char *str_ptr = NULL;
  int counter;


/*******************************************************************
* Check for missing input string.
******************************************************************/
  if(input_string == NULL){
    printf("Input string to parse does not exist (NULL).\n");
    return(FALSE);
  }

/*******************************************************************
* Allocate memory for a copy of the input string. This is used to count
* the number of and maximum length of the substrings and not destroy the
* input string.
******************************************************************/
  if( (temp_string = (char *)malloc(strlen(input_string) +1)) == NULL){
    printf("Unable to allocate memory for copy of the input string.\n");
    return(FALSE);
  }
  strcpy(temp_string, input_string);

/*******************************************************************
* Parse the input string to find out how many substrings it contains
* and the maximum length of substrings.
******************************************************************/
  str_ptr = strtok(temp_string, delimiter);
  counter = 1;

  while(str_ptr){
    str_ptr = strtok(NULL, delimiter);
    if(str_ptr)
      counter++;
  }

/*******************************************************************
* Allocate memory for the array of strings
******************************************************************/
  if( (*output_ints = (int *)calloc(counter, sizeof(int))) == NULL){
    printf("Unable to allocate memory for the output string.\n");
    return(FALSE);
  }

/*******************************************************************
* Allocate memory for a copy of the input string to parse and copy into
* the output array of strings.
******************************************************************/
  if( (temp_string = (char *)malloc(strlen(input_string) + 1)) == NULL){
    printf("Unable to allocate memory for a copy of the input string.\n");
    return(FALSE);
  }
  
  strcpy(temp_string, input_string);

/*******************************************************************
* Finally, copy the input substrings into the output array
******************************************************************/
  str_ptr = strtok(temp_string, delimiter);
  *sum = 0;
  counter = 0;
  while(str_ptr){
    if(str_ptr){
      (*output_ints)[counter] =  atoi(str_ptr);
      *sum += (*output_ints)[counter];
      counter++;
    }
    str_ptr = strtok(NULL, delimiter);
  }
  /*
  for(i=0; i < counter; i++)
    printf("output string %d = %d.\n", i, (*output_ints)[i]);
  printf("output string sum %d.\n", *sum);
  */
/*******************************************************************
* Copy the number of substrings into the output variable and return
******************************************************************/
  *num_args = counter;

  return(TRUE);
}

/*
 * This function fills "nn_dir_num_buf" with a 0 (zero) padded number, whose
 * total length is equal to "max_num_nn_dirs_len". The buffer should
 * be pre-allocated and have enough room for that, plus the \0 (NULL) byte at
 * the end.
 *
 * This function returns 0 (zero) on success, -1 otherwise.
 */

extern int
make_nn_dir_num_string(
    char *nn_dir_num_buf,
    unsigned int nn_dir_num,
    size_t max_num_nn_dirs_len,
    struct State *state ) {

  int  ret_val;

  ret_val = sprintf( nn_dir_num_buf, "%0*u", ( int ) max_num_nn_dirs_len, nn_dir_num );
  if ( ret_val == 0 ) {
    fprintf(
        state->efptr,
        "ERROR: Failed to convert nn_dir_num to a zero-padded string\n" );
    return -1;
  }

  return 0;
}


/*
 * This function allocates memory for and fills "nn_dir_buf" with the concatenation of:
 *
 * parent_dir
 * "/"
 * nn_dir_prefix
 * string representation of "nn_dir_num" that is zero-padded to "max_num_nn_dirs_len".
 *
 * This function returns 0 (zero) on success, -1 otherwise.
 *
 * The caller must free "nn_dir_buf".
 */

extern int
make_nn_dir_string(
    char **nn_dir_buf,
    char *parent_dir,
    char *nn_dir_prefix,
    unsigned int nn_dir_num,
    size_t max_num_nn_dirs_len,
    struct State *state ) {

  char temp[33];


  if ( make_nn_dir_num_string(
           temp,
           nn_dir_num,
           max_num_nn_dirs_len,
           state )) {
    fprintf(
        state->efptr,
        "ERROR: Failed to make the nn_dir_num, %u, into a string\n", nn_dir_num );
    return -1;
  }

  *nn_dir_buf = ( char * )malloc (
      strlen( parent_dir )    +
      strlen( "/" )           +
      strlen( nn_dir_prefix ) +
      strlen( temp )          +
      1 );
  *nn_dir_buf = strcpy( *nn_dir_buf, parent_dir );
  *nn_dir_buf = strcat( *nn_dir_buf, "/" );
  *nn_dir_buf = strcat( *nn_dir_buf, nn_dir_prefix );
  *nn_dir_buf = strcat( *nn_dir_buf, temp );
  /*
  fprintf( stderr, "nn_dir_buf is \"%s\"\n", *nn_dir_buf );
  */

  return 0;
}


/*
 * Make the subdirectories for the N-N I/O job under "parent_dir" with the
 * base name "nn_dir_prefix" and numbers appended to that.
 *
 * This function returns 0 if successful, otherwise it prints an error and
 * returns -1.
 *
 * Don't forget to make sure nn_dir_prefix has a value before calling this.
 */

extern int
make_nn_dirs(
    char *parent_dir,
    char *nn_dir_prefix,
    unsigned int num_nn_dirs,
    struct State *state ) {

  struct stat st;
  char max_num_nn_dirs[33];
  unsigned int i;
  size_t max_num_nn_dirs_len;
  char *nn_dir_str = NULL;
                    

  /*
   * Stat the parent directory to get st.st_mode that we will use to set
   * permissions on the directories we create with mkdir.
   */

  if ( lstat( parent_dir, &st )) {
    fprintf(
        state->efptr,
        "ERROR: Failed to lstat the directory, \"%s\"\n", parent_dir );
    return -1;
  }

  sprintf( max_num_nn_dirs, "%u", num_nn_dirs-1 );
  max_num_nn_dirs_len = strlen( max_num_nn_dirs );

  /*
   * We're going to make subdirectories in "parent_dir" whose names are
   * "nn_dir_prefix" concatenated with integers from 0 to num_nn_dirs-1.
   */

  for ( i = 0; i < num_nn_dirs; i++ ) {
    if ( make_nn_dir_string(
             &nn_dir_str,
             parent_dir,
             nn_dir_prefix,
             i,
             max_num_nn_dirs_len,
             state )) {
      fprintf(
          state->efptr,
          "ERROR: Failed to create nn_dir_string in the directory, \"%s\"\n",
          parent_dir );
      return -1;
    }

    /*
    fprintf( stderr, "About to make the directory \"%s\"\n", nn_dir_str );
    */
    if ( mkdir( nn_dir_str, st.st_mode )) {
      fprintf(
          state->efptr,
          "INFO: Failed to create the directory, \"%s\"\n", nn_dir_str );
      fprintf(
          state->efptr,
          "INFO: It may already exist.\n" );
    }
    /*
    fprintf( stderr, "About to free nn_dir_str\n" );
    */

    free( nn_dir_str );

    /*
    fprintf( stderr, "Freed nn_dir_str\n" );
    */
  }
  return 0;
}


/*
 * Remove the subdirectories for the N-N I/O job under "parent_dir" with the
 * base name "nn_dir_prefix" and numbers appended to that.
 *
 * This function returns 0 if successful, otherwise it prints an error and
 * returns -1.
 *
 * Don't forget to make sure nn_dir_prefix has a value before calling this.
 */

extern int
remove_nn_dirs(
    char *parent_dir,
    char *nn_dir_prefix,
    unsigned int num_nn_dirs,
    struct State *state ) {

  char max_num_nn_dirs[33];
  unsigned int i;
  size_t max_num_nn_dirs_len;
  char *nn_dir_str = NULL;
                    

  sprintf( max_num_nn_dirs, "%u", num_nn_dirs-1 );
  max_num_nn_dirs_len = strlen( max_num_nn_dirs );

  /*
   * We're going to remove subdirectories in "parent_dir" whose names are
   * "nn_dir_prefix" concatenated with integers from 0 to num_nn_dirs-1.
   */

  for ( i = 0; i < num_nn_dirs; i++ ) {
    if ( make_nn_dir_string(
             &nn_dir_str,
             parent_dir,
             nn_dir_prefix,
             i,
             max_num_nn_dirs_len,
             state )) {
      fprintf(
          state->efptr,
          "ERROR: Failed to create nn_dir_string to remove it from the directory, \"%s\"\n",
          parent_dir );
      return -1;
    }

    if ( rmdir( nn_dir_str )) {
      fprintf(
          state->efptr,
          "INFO: Failed to remove the directory, \"%s\"\n", nn_dir_str );
      fprintf(
          state->efptr,
          "INFO: It may have other files in it.\n" );
    }
    free( nn_dir_str );
  }
  return 0;
}


/*
 * If the user provided a value for nn_dir_prefix, this function does nothing.
 * If the user did not provide a value for nn_dir_prefix, this function sets
 * it to the default value.
 *
 * This function returns 0 if successful, otherwise it prints an error and
 * returns -1.
 */

extern int
set_nn_dir_prefix(
    char **nn_dir_prefix,
    struct State *state ) {

  if (( *nn_dir_prefix == NULL ) || ( strlen( *nn_dir_prefix ) == 0 )) {
    /*
     * printf( "nn_dir_prefix is NULL or zero-length.\n" );
     */
    *nn_dir_prefix = strdup( "nn_dir" );
    if ( *nn_dir_prefix == NULL ) {
      fprintf(
          state->efptr,
          "ERROR: Could not allocate memory for nn_dir_prefix\n" );
      return -1;
    }
  }

  return 0;
}

/*
 * This function takes in path and returns a newly allocated string that is
 * the concatenation of:
 * 
 * dirname( path )
 * "/"
 * nn_dir_prefix (if NULL, then the default is "nn_dir"
 * "%d" (the expansion indicator for the NN dir number calculated in expand_path)
 * "/"
 * basename( path )
 *
 * Also, if nn_dir_prefix is the NULL or empty string, a default value will be
 * assigned to it and returned to the caller via a call to set_nn_dir_prefix.
 */ 

extern char *
expand_tfname_for_nn(
    char *path,
    char **nn_dir_prefix,
    struct State *state ) {

  char *directory;
  char *filename;
  char *new_path;
                

  /*
  fprintf( stderr, "Before setting, nn_dir_prefix is \"%s\"\n", nn_dir_prefix );
  */

  if ( set_nn_dir_prefix( nn_dir_prefix, state )) {
    fprintf(
      state->efptr,
      "ERROR: Failed to set nn_dir_prefix to a string value\n" );
    return NULL;
  }

  /*
  fprintf( stderr, "After setting, nn_dir_prefix is \"%s\"\n", nn_dir_prefix );
  */

  directory = strdup( path );
  directory = dirname( directory );
  /*
  fprintf( stderr, "directory is \"%s\"\n", directory );
  */

  filename = strdup( path );
  filename = basename( filename );
  /*
  fprintf( stderr, "filename is \"%s\"\n", filename );
  */

  /*  
   * We're going to put a "/" before nn_dir_prefix, then nn_dir_prefix,
   * then "%d" (the substitution string for the nn_dir_num), and finally
   * another "/"
   */

  new_path = ( char * )malloc (
      strlen( directory )     +   
      strlen( "/" )           +   
      strlen( *nn_dir_prefix ) + 
      strlen( "%d/" )         +   
      strlen( filename )      +   
      1 );
  new_path = strcpy( new_path, directory );
  new_path = strcat( new_path, "/" );
  new_path = strcat( new_path, *nn_dir_prefix );
  new_path = strcat( new_path, "%d/" );
  new_path = strcat( new_path, filename );
  /*
  fprintf( stderr, "new_path is \"%s\"\n", new_path );
  */

  /*
   * Calling free on these causes a segmentation fault. Not sure why as
   * the documentation says that we should be able to free something that
   * was created with strdup. It must be what dirname and basename do to
   * that char pointer after the calls to strdup.
   *
  free( directory );
  free( filename );
  */

  return new_path;
}


extern char *
expand_path(
    char *str,
    long timestamp,
    unsigned int num_nn_dirs,
    struct State *state )
{
  char tmp1[2048];
  char tmp2[1024];
  
  char *p;
  char *q;

  char max_num_nn_dirs[33];
  size_t max_num_nn_dirs_len;
  
  int string_modified = FALSE;

  
  if (str == (char *)0) {
    return ((char *)0);
  }
  
  for (p = str, q = tmp1; *p != '\0';) {
    if (*p == '%') {
      string_modified = TRUE;
      p++;
      if (*p == '\0') {
        return ((char *)0);
      }

      switch (*p) {
      case 'h':
        if (gethostname(tmp2, sizeof(tmp2) - 1) != 0) {
          return ((char *)0);
        }
        tmp2[sizeof(tmp2) - 1] = '\0';
        strncpy(q, tmp2, (sizeof(tmp1) - (q - tmp1) - 1));
        tmp1[sizeof(tmp1) - 1] = '\0';
        q += strlen(tmp2);
        break;

      case 's':
        snprintf(tmp2, (sizeof(tmp2) - 1), "%ld", timestamp);
        tmp2[sizeof(tmp2) - 1] = '\0';
        strncpy(q, tmp2, (sizeof(tmp1) - (q - tmp1) - 1));
        tmp1[sizeof(tmp1) - 1] = '\0';
        q += strlen(tmp2);
        break;

      case 'p':
	      snprintf(tmp2, (sizeof(tmp2) - 1), "%d", getpid());
	      tmp2[sizeof(tmp2) - 1] = '\0';
	      strncpy(q, tmp2, (sizeof(tmp1) - (q - tmp1) - 1));
	      tmp1[sizeof(tmp1) - 1] = '\0';
	      q += strlen(tmp2);
	      break;

      case 'r':
	      snprintf(tmp2, (sizeof(tmp2) - 1), "%d", state->my_rank);
	      tmp2[sizeof(tmp2) - 1] = '\0';
	      strncpy(q, tmp2, (sizeof(tmp1) - (q - tmp1) - 1));
	      tmp1[sizeof(tmp1) - 1] = '\0';
	      q += strlen(tmp2);
	      break;
/*
 * %d expands to be a zero-padded integer bin into which a rank hashes.
 * It's intended use is to put a number on which subdirectory
 * a non-PLFS N-N I/O job will write/read its files.
 */
      case 'd':
        if ( num_nn_dirs > 1 ) {
          sprintf( max_num_nn_dirs, "%u", num_nn_dirs-1 );
          max_num_nn_dirs_len = strlen( max_num_nn_dirs );
          make_nn_dir_num_string(
              tmp2,
              ( unsigned int ) ( state->my_rank % num_nn_dirs ),
              max_num_nn_dirs_len,
              state );
	        strncpy(q, tmp2, (sizeof(tmp1) - (q - tmp1) - 1));
	        tmp1[sizeof(tmp1) - 1] = '\0';
	        q += strlen(tmp2);
        }
	      break;

      default:
	      return ((char *)0);
      }

      p++;
    } 
    else {
      *q++ = *p++;
    }
  }

  //fprintf( stderr, "%s: %s -> %s\n", __FUNCTION__, str, tmp1 );
  if(!string_modified) {
    strcpy(tmp1,str);
  }

  return (strdup(tmp1));
}

/***************************************************************************
*
*  FUNCTION:  string_to_string_array
*  PURPOSE:   Parse an input string, with user defined delimined substrings,
*             into an array of strings. The original input string is not
*             modified by this routine.
*
*  PARAMETERS:
*             char *input_string    - Input string to parse containing
*                                     deliminted substrings
*             char *delimiter       - String used to parse the input string.
*                                     May be multiple characters, ex " *:"
*             int *num_args         - Number of substrings contained in the
*                                     input string
*             char ***output_string - Array of substrings contained in the
*                                     input string
*
*  RETURN:    TRUE(1) on successful exit; FALSE(0) otherwise
*  HISTORY:   03/05/01 [jnunez] Original version.
*
****************************************************************************/
int string_to_string_array(char *input_string, char *delimiter,
                           int *num_args, char ***output_string){

  char *temp_string = NULL;
  char *str_ptr = NULL;
  int max_len, counter;
  int i;                          /* General for loop index */


/*******************************************************************
* Check for missing input string.
******************************************************************/
  if(input_string == NULL){
    printf("Input string to parse does not exist (NULL).\n");
    return(FALSE);
  }

/*******************************************************************
* Allocate memory for a copy of the input string. This is used to count
* the number of and maximum length of the substrings and not destroy the
* input string.
******************************************************************/
  if( (temp_string = (char *)malloc(strlen(input_string) +1)) == NULL){
    printf("Unable to allocate memory for copy of the input string.\n");
    return(FALSE);
  }
  strcpy(temp_string, input_string);

/*******************************************************************
* Parse the input string to find out how many substrings it contains
* and the maximum length of substrings.
******************************************************************/
  str_ptr = strtok(temp_string, delimiter);
  counter = 1;
  max_len = (int)strlen(str_ptr);

  while(str_ptr){
    str_ptr = strtok(NULL, delimiter);
    if(str_ptr){
      counter++;
      if(strlen(str_ptr) > max_len) max_len = (int)strlen(str_ptr);
    }
  }

/*******************************************************************
* Allocate memory for the array of strings
******************************************************************/
  if( (*output_string = (char **)calloc(counter, sizeof(char *))) == NULL){
    printf("Unable to allocate memory for the output string.\n");
    return(FALSE);
  }
  for(i=0; i < counter; i++)
    if( ((*output_string)[i] = (char *)malloc(max_len + 1)) == NULL){
      printf("Unable to allocate memory for the output string.\n");
      return(FALSE);
    }

/*******************************************************************
* Allocate memory for a copy of the input string to parse and copy into
* the output array of strings.
******************************************************************/
  if( (temp_string = (char *)malloc(strlen(input_string) + 1)) == NULL){
    printf("Unable to allocate memory for a copy of the input string.\n");
    return(FALSE);
  }

  strcpy(temp_string, input_string);

/*******************************************************************
* Finally, copy the input substrings into the output array
******************************************************************/
  str_ptr = strtok(temp_string, delimiter);
  counter = 0;
  while(str_ptr){
    if(str_ptr){
      strcpy((*output_string)[counter], str_ptr);
      counter++;
    }
    str_ptr = strtok(NULL, delimiter);
  }
  /*
  for(i=0; i < counter; i++)
    printf("output string %d = %s.\n", i, (*output_string)[i]);
  */
/*******************************************************************
* Copy the number of substrings into the output variable and return
******************************************************************/
  *num_args = counter;

  return(TRUE);
}

/*******************************************
 *   ROUTINE: MPIIO_set_hint
 *   PURPOSE: Set the MPI hint in the MPI_Info structure.
 *   LAST MODIFIED: 
 *******************************************/
int MPIIO_set_hint(int my_rank, MPI_Info *info, char *key, char *value)
{

  if((MPI_Info_set(*info, key, value)) !=  MPI_SUCCESS){
    if(my_rank == 0)
      fprintf(stderr, "[RANK %d] WARNING: Unable to set the hint %s to value %s in the MPI_info structure.\n", my_rank, key, value);
  }
  
  return 1;
}

char
printable_char( int index ) {
    index %= 27;    // for an alphabetic numbers
    index += 96;    // for the lowercase
    return ( index == 96 ? '\n' : (char)index ); // throw some newlines in
    //return (char)index;
}

void
fill_buf( char *check_buf, int blocksize, int rank, int i, int pagesize,
          int touch ) 
{
    int j = 0;
    double value = i * blocksize;
    double *buffer = (double *)check_buf;
    int each_buffer_unique = 0;

    if ( touch == 0 ) {
        return;
    }

    if ( i == 0 || each_buffer_unique ) {
	// only fill it in fully for first block
	    memset( check_buf, 0, blocksize );
	    //snprintf( check_buf, blocksize, "Rank %d for object %d blocksize %d",
	    //        rank, i, blocksize );
	    for( j = 0; j < blocksize / sizeof(double); j++ ) {
		buffer[j] = value;
		value++;
	    }
        buffer[0] = (char)rank; // put the rank in there too
	} else {
        // for other blocks, just change one double per page
		for( j = 0; j < blocksize / sizeof(double); j += pagesize ) {
			//printf( "writing at %ld\n", j * pagesize );
			buffer[j] = value;
			value++;
		}
	}	
    /*
	// put a string in there 
    snprintf( check_buf, blocksize, "Rank %d for object %d blocksize %d\n",
            rank, i, blocksize );
    */
}

int
verify_buf( int rank, double *expected, double *received, char *filename, 
    long which_block, long blocksize, int touch ) 
{
    int errors_found = 0;

    if ( touch == 0 ) { // don't check
        return 0;
    }

    if ( memcmp( expected, received, blocksize ) != 0 ) {
        int j = 0;
        for( j = 0; j < blocksize / sizeof(double); j++ ) {
            errors_found += compare_double( filename, rank, 
                    which_block, j, blocksize, 
                    received[j], expected[j] );
            if ( errors_found >= 100 ) {
                fatal_error( stderr, rank, 0, NULL, "Too many errors\n" );
            }
        }
    }
    return errors_found;
}

void bin_prnt_byte(int x)
{
   int n;
   for(n=0; n<8; n++)
   {
      if((x & 0x80) !=0)
      {
         fprintf(stderr,"1");
      }
      else
      {
         fprintf(stderr,"0");
      }
      if (n==3)
      {
         fprintf(stderr," "); /* insert a space between nybbles */
      }
      x = x<<1;
   }
}

void bin_prnt_int(int x)
{
   int hi, lo;
   hi=(x>>8) & 0xff;
   lo=x&0xff;
   bin_prnt_byte(hi);
   printf(" ");
   bin_prnt_byte(lo);
}

char *itob(int x) {
	static char buff[sizeof(int) * CHAR_BIT + 1];
	int i;
	int j = sizeof(int) * CHAR_BIT - 1;

	buff[j] = 0;
	for(i=0;i<sizeof(int) * CHAR_BIT; i++) {
		if(x & (1 << i))
			buff[j] = '1';
		else
			buff[j] = '0';
		j--;
	}
	return buff;
}


void
print_double( char *header, double one ) {
    char *c_one = (char *)&one;
    int c;
	int *b = (int *)&one;
    fprintf( stderr, "%s %lf \n\tchars(", header, one );

    for( c = 0; c < sizeof(double) / sizeof(char); c++ ) {
	fprintf( stderr, "%c", c_one[c] );
    }
    fprintf( stderr, ")\n\tbinary(" );

	for( c = 0; c < sizeof(double) / sizeof(int); c++ ) {
		//bin_prnt_int( b[c] );
		//fprintf( stderr, " " );
		fprintf( stderr, "%s ", itob( b[c] ) );
	}	
    fprintf( stderr, ")\n" );
}

int
compare_double( char *file, int rank, long block, long block_off, long bs, 
	double received, double expected ) 
{
	if ( received != expected ) {
        warning_msg( stderr, rank, 0, NULL, 
			"read error in %s at block %lu, block off %lu, file off %llu: "
			"%lf != %lf\n",
			file, block, block_off,  (long long)block *  (long long)bs + (long long)block_off * sizeof(double), 
			received, expected );
		print_double( "Expected", expected );
		print_double( "Received", received );
		return 1;
	} else {
		return 0;
	}
}

void *
Calloc( size_t nmemb, size_t size, FILE *fp, int rank ) {
    void * buffer;
    buffer = calloc( nmemb, size );
    if ( ! buffer ) {
        fatal_error( fp, rank, 0, NULL, 
				"calloc %ld failed %s", size, strerror(errno) );
    }
    return buffer;
}

void *
Malloc( size_t size, FILE *fp, int rank ) {
    void * buffer;
    buffer = malloc( size );
    if ( ! buffer ) {
        fatal_error( fp, rank, 0, NULL, 
				"malloc %uld failed %s", size, strerror(errno) );
    }
    return buffer;
}

void *
Valloc( size_t size, FILE *fp, int rank ) {
    void * mybuf;
    mybuf = valloc( size ); // obsolete and warns . . . 
    //mybuf = memalign(sysconf(_SC_PAGE-SIZE),size);
    if ( ! mybuf ) {
        fatal_error( fp, rank, 0, NULL, 
				"valloc %ld failed %s", size, strerror(errno) );
    }
    return mybuf;
}

char * 
chomp( char * str ) {
    int len = strlen(str);
    while ( str[len - 1] == '\n' ) str[len - 1] = '\0';
    return str;
}

#if defined(MYSQL_HOST) || defined(MYSQL_FILE)

/*
 * Asl far as I can tell, check_env is not used. It's not called internally and
 * fs_test does not call it.
 *
 * Nevertheless, I'm going to return zero of we're not using a database.
 */

int
check_env( const char *param, const char *value ) {
  EXIT_UNLESS_DB(0);
  return ( getenv(param) && ! strcmp( getenv(param), value ) );
}

char *
get_output( const char *command, char *tmp_dir_name, int redirect_stderr) {
    static char output[32768];
    static int uniquifier = 0;
    char full_command[4096];
    int input_fd;
    int len;
    char tmpfile[1024];


    /*
	fprintf( stderr, "# fork/exec not avail on roadrunner for %s\n",
                command );
    return NULL;    // broke in openmpi on infiniband . . . 
    */

    snprintf( tmpfile, 1024, "%s/%d.%d", tmp_dir_name, (int)getpid(), uniquifier++);
    snprintf( full_command, 4096, "%s > %s", command, tmpfile );
    if (redirect_stderr) {
       snprintf( full_command, 4096, "%s > %s 2>&1", command, tmpfile );
    }
    if ( system( full_command ) == -1 ) {
        fprintf( stderr, "Exec %s failed.", command );
        return NULL;
    } else {
        fprintf( stderr, "# Ran %s\n", full_command );
    }

    input_fd = open( tmpfile, O_RDONLY );
    if ( input_fd <= 0 ) {
        fprintf( stderr, "Open %s failed: %s\n", tmpfile, strerror(errno) );
        return NULL;
    }
    if ( ( len = read( input_fd, output, 32768 ) ) <= 0 ) {
        fprintf( stderr, "read %s failed: %s\n", tmpfile, strerror(errno) );
        return NULL;
    }
    close( input_fd );
    unlink( tmpfile );
    output[len] = '\0';
    chomp( output );
//    fprintf( stderr, "# Rec'd %s\n", output );
    return output;
}

char *
find_server_version( char *mountpt, char *temp_dirname) {

    static char version[128];
    char *panfs_mount;
    char scr_space[128];
    int stderr_redirect = FALSE;
    
    // determine which scratch space is being used
    panfs_mount = strstr(mountpt, "scratch");
    if ( !panfs_mount ) {
        fprintf( stderr, "Cannot determine which scratch space being used (%d).\n", 
                __LINE__ );
        return NULL;
    }
    sscanf(panfs_mount, "%[^/]", scr_space);

    char *mount = get_output( "/bin/mount", temp_dirname, stderr_redirect);
    if ( ! mount ) {
        fprintf( stderr, "Couldn't find panfs server version (%d).\n", 
                __LINE__ );
        return NULL;
    }

    // search for mounted scratch space then backup pointer to get web
    // address for that space
    mount = strstr( mount, scr_space );
    if ( ! mount ) {
        fprintf( stderr, "Targeted scratch space does not match mounted space (%d).\n", 
                __LINE__ );
        return NULL;
    }
    while (*mount != '\n') {
      mount--;
    }

    mount = strstr( mount, "panfs://" );
    if ( ! mount ) {
        fprintf( stderr, "Couldn't find panfs server version (%d).\n", 
                __LINE__ );
        return NULL;
    }

    int ip1, ip2, ip3, ip4;
    char curl_command[128];

    char curl_command_secure[128];
    if ( sscanf( mount, "panfs://%d.%d.%d.%d", &ip1, &ip2, &ip3, &ip4 ) == 4 ) {
     
        snprintf( curl_command, 128, "%s%d.%d.%d.%d", 
                  "curl -k -L http://",ip1, ip2, ip3, ip4 );
        snprintf( curl_command_secure, 128, "%s%d.%d.%d.%d", 
                  "curl -k -L https://",ip1, ip2, ip3, ip4 );

        char *substr;
        stderr_redirect = TRUE;
        char *curl_out = get_output( curl_command, temp_dirname, 
                                     stderr_redirect);
  
        if (( substr = strstr( curl_out, "command not found" ) ) != NULL) {
            fprintf( stderr, "Couldn't find panfs server version -- ");
            fprintf( stderr, "curl not installed (%d).\n", __LINE__);
            return NULL;
        }
        else if (( substr = strstr( curl_out, "Release Notes for Version" ) ) != 
                   NULL) {
              sscanf( substr, "Release Notes for Version %[^<]", version);
              fprintf(stdout, "Panfs server version = %s\n", version);
              return(version);
        }

        // else try using https for panfs server versions >= 4.0
        else {
            printf("Could not connect via http.. Trying https.\n");
            char *curl_out = get_output( curl_command_secure, temp_dirname, 
                                         stderr_redirect);
            if (( substr = strstr( curl_out, "Release Notes for Version" ) )
                                  != NULL) {
                sscanf( substr, "Release Notes for Version %[^<]", version);
                fprintf(stdout, "Panfs server version = %s\n", version);
                return(version);
            }
            else {
                fprintf( stderr, "Couldn't find panfs server version (%d).\n",
                         __LINE__);
                return NULL;
            }
        }   
    
    }
}   

int
mystr_count( char *needle, char *needle2, char *haystack ) {
    int count = 0;
    char *position = haystack;
    char full_needle[1024];

    snprintf( full_needle, 1024, "%s%s", needle, needle2 );
    //fprintf( stderr, "Looking for %s in %s\n", needle, haystack);
    while( ( position = strstr( position, full_needle ) ) != NULL ) {
        //fprintf( stderr, "Found %s at %s in %s\n", 
            //needle, position, haystack);
        count++;
        position++; // otherwise it will keep finding itself
    }
    return count;
}

void
set_hostlist( struct Parameters *params, int rank ) {
    char myname[256];
    char **hostnames;
    int i;
    int full_hostlist_length;
    char *full_hostlist;
	int mpi_ret;
    float average_procs_per_node = 0;

    int nproc = params->num_procs_world;

    memset( (void *)myname, 0, 256 );
    gethostname( myname, 256 );
    full_hostlist_length = strlen( myname ) + 10;   // give it slop

    //fprintf( stderr, "%d (%s): Entering set_hostlist\n", rank, myname );
    if ( rank == 0 ) {
        hostnames = (char **)Malloc( sizeof(char *) * nproc, stderr, rank);
        MPI_Status stat;
        for( i = 1; i < nproc; i++ ) {
            hostnames[i] = (char*)Malloc( 256, stderr, rank );
            memset( (void *)hostnames[i], 0, 256 );
            //fprintf( stderr, "0: recv'ing from %d\n", i );
            if ((mpi_ret = 
				  MPI_Recv( hostnames[i], 256, MPI_BYTE, i, 0, MPI_COMM_WORLD,
                    &stat ) ) != MPI_SUCCESS )
            {
                fatal_error( stderr, rank, mpi_ret, &stat, "MPI_Recv\n" );
            }
            //fprintf( stderr, "%s len %d, now full is %d", 
            //        hostnames[i],
            //        (int)strlen(hostnames[i]), 
            //        (int)(full_hostlist_length + strlen(hostnames[i] )) );
            full_hostlist_length += strlen( hostnames[i] ) + 2;
            //fprintf( stderr, "Recv'd %s (%d) from %d\n", 
            //        hostnames[i], (int)strlen(hostnames[i]), i );
        }
        full_hostlist = (char *)Malloc( full_hostlist_length, stderr, rank );
        snprintf( full_hostlist, full_hostlist_length, "%s,", myname );
        // iterate once to put them into a string
        for( i = 1; i < nproc; i++ ) {
            int cur_len = strlen(full_hostlist);
            snprintf( &(full_hostlist[cur_len]), 
                    full_hostlist_length - cur_len,
                    "%s,", hostnames[i] );
            //fprintf( stderr, "Writing %s at %d\n", hostnames[i], 
            //        full_hostlist_length - cur_len );
        }
        // iterate again to count them in the string and
        // to free the memory
        average_procs_per_node += mystr_count( myname, ",", full_hostlist );
        for( i = 1; i < nproc; i++ ) {
            average_procs_per_node 
                += mystr_count(hostnames[i],",",full_hostlist);
            free( hostnames[i] );
        }
        free( hostnames );
        // we used to use strdup here to copy full hostlist into params, but
        // it got so large at scale that now we just copy over the first 1024
        full_hostlist_length = 1024;    // only preserve some of it for the db
        params->host_list = (char *)Malloc(full_hostlist_length,stderr,rank);
        memcpy(params->host_list,full_hostlist,full_hostlist_length);
        params->procs_per_node = average_procs_per_node / nproc;
        free( full_hostlist );
    } else {
        if ( (mpi_ret 
			= MPI_Send( myname, 256, MPI_BYTE, 0, 0, MPI_COMM_WORLD))
                != MPI_SUCCESS )
        {
            fatal_error( stderr, rank, mpi_ret, NULL, "MPI_Send\n" );
        }
        //fprintf( stderr, "%d: Sent %s to 0\n", rank, myname );
    }
}

char *
find_mountpoint( const char *path ) {
    static char *mntpt = NULL;
    char *second_slash = NULL;

        // clean up if this is the Nth time we're called (N > 1)
    if ( mntpt ) free( mntpt );
    if ( ! path ) {
        fprintf( stderr, "path is NULL at %s:%d\n", __FUNCTION__, __LINE__ );
        //fatal_error( stderr, 0, 0, NULL, "path is NULL" );
        return NULL;
    }
    mntpt = strdup( path );

    second_slash = index( mntpt, '/' );
    if ( ! second_slash ) return mntpt;
    second_slash++; // move past the first slash
    second_slash = index( second_slash, '/' );
    if ( ! second_slash ) return mntpt; 
    *second_slash = '\0';    // change to a NULL term so that only the
                            // first path component is visible
    return mntpt;
}

void
addDBColumnName( int string_len, char  *columns, char *name ) {

    snprintf( &(columns[strlen(columns)]), 
              string_len - strlen(columns),    
              "%s %s",
              strlen(columns) ? "," : "",
              name );
}

void
addDBInt( int string_len, char *columns, char *values, char *mixed,
        long long value, char *name ) 
{

    addDBColumnName( string_len, columns, name );
    snprintf( &(values[strlen(values)]),
              string_len - strlen(values),
              "%s \'%ld\'",
              strlen(values) ? "," : "",
              (long)value );
    snprintf( &(mixed[strlen(mixed)]),
            string_len - strlen(mixed),
            "%s%s=\'%ld\'",
            strlen(mixed) ? "," : "",
            name, (long)value ); 
}

void
addPanFSInt( int string_len, char *columns, char *values, char *mixed,
        long long value, char *name ) 
{

	// for RAID-10, sometimes these values aren't found so don't
	// override possibly valid values if we didn't discover anything
	// values may already be set now that we have the FS_TEST_EXTRA 
	if ( value > 0 ) {
		addDBInt( string_len, columns, values, mixed, value, name );
	}
}

void
addDBFloat( int string_len, char *columns, char *values, char *mixed,
        float value, char *name ) 
{
    addDBColumnName( string_len, columns, name );
    snprintf( &(values[strlen(values)]),
              string_len - strlen(values),
              "%s \'%e\'",
              strlen(values) ? "," : "",
              value );
    snprintf( &(mixed[strlen(mixed)]),
            string_len - strlen(mixed),
            "%s%s=\'%e\'",
            strlen(mixed) ? "," : "",
            name, value ); 

}

void
addDBStr( int string_len, char *columns, char *values, char *mixed,
        char *value, char *name ) 
{
        // if value == NULL, just don't add to query, db will set it NULL
    if ( value ) {
        addDBColumnName( string_len, columns, name );
        snprintf( &(values[strlen(values)]),
                  string_len - strlen(values),
                  "%s \'%s\'",
                  strlen(values) ? "," : "",
                  value ? value : "" );
    }
    if ( value ) {
        snprintf( &(mixed[strlen(mixed)]),
            string_len - strlen(mixed),
            "%s%s=\'%s\'",
            strlen(mixed) ? "," : "",
            name, value ? value : "" );
    }
}
void addDBTimes_threads( int string_len, char *columns, char *values, 
                         char *mixed, struct time_info *thread_time,
                         struct Parameters *parameters, struct State *states )
{
    addDBFloat( string_len, columns, values, mixed, 
                        (float)states->total_mbs,
                        "total_size_mb" );
//    printf("AAAAAAAAAAA %f\n", thread_time->write_file_open_wait_time_max);
    if (parameters->thread_write) {
        addDBFloat( string_len, columns, values, mixed, thread_time->write_file_open_wait_time_max,
                    "write_file_open_wait_time_max");
        addDBFloat( string_len, columns, values, mixed, thread_time->write_file_open_wait_time_min,
                    "write_file_open_wait_time_min");
        addDBFloat( string_len, columns, values, mixed, thread_time->write_file_open_wait_time,
                    "write_file_open_wait_time");
        addDBFloat( string_len, columns, values, mixed, thread_time->write_total_op_time_max,
                    "write_total_op_time_max");
        addDBFloat( string_len, columns, values, mixed, thread_time->write_total_op_time_min,
                    "write_total_op_time_min");
        addDBFloat( string_len, columns, values, mixed, thread_time->write_total_op_time,
                    "write_total_op_time");
        addDBFloat( string_len, columns, values, mixed, thread_time->write_file_close_wait_time_max,
                    "write_file_close_wait_time_max");
        addDBFloat( string_len, columns, values, mixed, thread_time->write_file_close_wait_time_min,
                    "write_file_close_wait_time_min");
        addDBFloat( string_len, columns, values, mixed, thread_time->write_file_close_wait_time,
                    "write_file_close_wait_time");
        addDBFloat( string_len, columns, values, mixed, thread_time->write_total_time_max,
                    "write_total_time_max");
        addDBFloat( string_len, columns, values, mixed, thread_time->write_total_time_min,
                    "write_total_time_min");
    }
    else {
        addDBFloat( string_len, columns, values, mixed, thread_time->read_file_open_wait_time_max,
                    "read_file_open_wait_time_max");
        addDBFloat( string_len, columns, values, mixed, thread_time->read_file_open_wait_time_min,
                    "read_file_open_wait_time_min");
        addDBFloat( string_len, columns, values, mixed, thread_time->read_file_open_wait_time,
                    "read_file_open_wait_time");
        addDBFloat( string_len, columns, values, mixed, thread_time->read_total_op_time_max,
                    "read_total_op_time_max");
        addDBFloat( string_len, columns, values, mixed, thread_time->read_total_op_time_min,
                    "read_total_op_time_min");
        addDBFloat( string_len, columns, values, mixed, thread_time->read_total_op_time,
                    "read_total_op_time");
        addDBFloat( string_len, columns, values, mixed, thread_time->read_file_close_wait_time_max,
                    "read_file_close_wait_time_max");
        addDBFloat( string_len, columns, values, mixed, thread_time->read_file_close_wait_time_min,
                    "read_file_close_wait_time_min");
        addDBFloat( string_len, columns, values, mixed, thread_time->read_file_close_wait_time,
                    "read_file_close_wait_time");
        addDBFloat( string_len, columns, values, mixed, thread_time->read_total_time_max,
                    "read_total_time_max");
        addDBFloat( string_len, columns, values, mixed, thread_time->read_total_time_min,
                    "read_total_time_min");
    }
}

void
addDBTimeComponent( float value, int string_len, char *values, char *columns,
        char *mixed, char *modifier, char *op, char *name ) 
{
    char fullname[1024];

    snprintf( fullname, 1024, "%s%s%s%s", 
            (op?op:""), 
            (op?"_":""), 
            name, 
            modifier );
    addDBFloat( string_len, columns, values, mixed, value, fullname );
}

void
addDBTime( int my_rank, int num_procs,
        int string_len, char *columns, char *values, char *mixed,
        float value, char *op, char *name ) 
{
    double min, max, sum;
    int min_ndx, max_ndx;
	int mpi_ret;

    // this is a little ugly really.  We do this exact same thing
    // in collect_and_print_time in print.c.  Really we should not
    // do this twice
    if( (mpi_ret = get_min_sum_max(my_rank, value, &min, &min_ndx,
		      &sum, &max, &max_ndx, op, stdout, stderr) ) != MPI_SUCCESS)
    {
        fatal_error( stderr, my_rank, mpi_ret, NULL,
                "Problem computing min, max, and sum of %s %s time.\n", 
                       op, name );
    }

    addDBTimeComponent( min,             string_len, values, columns, mixed,
            "_min", op, name );
    addDBTimeComponent( max,             string_len, values, columns, mixed,
            "_max", op, name );
    addDBTimeComponent( sum / num_procs, string_len, values, columns, mixed,
            "",     op, name );


}

void
addDBTimes( int my_rank, 
            int num_procs, int string_len, char *columns, char *values,
            char *mixed, struct time_values *times, char *op ) 
{

    addDBTime( my_rank, num_procs, string_len, columns, values, mixed, 
            times->file_open_wait_time, op, "file_open_wait_time" );
    addDBTimeComponent( times->file_open_wait_elapsed_time, string_len, values, 
            columns, mixed, "",     op, "file_open_wait_elapsed_time" );

    addDBTime( my_rank, num_procs, string_len, columns, values, mixed,
            times->file_close_wait_time, op, "file_close_wait_time" );
    addDBTimeComponent( times->file_close_wait_elapsed_time, string_len, values,
            columns, mixed, "",     op, "file_close_wait_elapsed_time" );

    addDBTime( my_rank, num_procs, string_len, columns, values, mixed,
            times->barrier_wait_time, op, "barrier_wait_time" );

    addDBTime( my_rank, num_procs, string_len, columns, values, mixed,
            times->file_sync_wait_time, op, "file_sync_wait_time" );
    addDBTimeComponent( times->file_sync_wait_elapsed_time, string_len, values, 
            columns, mixed, "",     op, "file_sync_wait_elapsed_time" );

    addDBTime( my_rank, num_procs, string_len, columns, values, mixed,
            times->file_op_wait_time, op, "file_op_wait_time" );

    addDBTime( my_rank, num_procs, string_len, columns, values, mixed,
            times->total_op_time, op, "total_op_time" );
    addDBTimeComponent( times->total_op_elapsed_time, string_len, values, 
            columns, mixed, "",     op, "total_op_elapsed_time" );

    addDBTime( my_rank, num_procs, string_len, columns, values, mixed,
            times->total_time, op, "total_time" );
    addDBTimeComponent( times->total_elapsed_time, string_len, values, columns, 
            mixed, "",     op, "total_elapsed_time" );

    addDBTime( my_rank, num_procs, string_len, columns, values, mixed,
            times->stat_time, op, "stat_time" );
    addDBTimeComponent( times->stat_elapsed_time, string_len, values, columns, 
            mixed, "",     op, "stat_elapsed_time" );

    addDBTimeComponent( times->plfs_flatten_time, string_len, values, columns, 
            mixed, "",     NULL, "plfs_flatten_time" );
}

#endif

// this function doesn't appear to be called at all anymore.
// maybe it all got migrated into FS_TEST_EXTRA
int
get_panfs_file_info( struct Parameters *params ) {
#if defined(MYSQL_HOST) || defined(MYSQL_FILE)
    int mpi_ret;
    MPI_File wfh;
    MPI_Info info_used = MPI_INFO_NULL;
    int nkeys, i, dummy_int;
    char **keys;
    char **values;

    EXIT_UNLESS_DB(0);

	// info object not created for posix and plfs io
    MPI_Info info = params->io_type == MPI_IO ? params->info : MPI_INFO_NULL;

    mpi_ret = MPI_File_open( MPI_COMM_SELF, params->tfname, 
            MPI_MODE_RDONLY | MPI_MODE_UNIQUE_OPEN, info, &wfh );
    if ( mpi_ret != MPI_SUCCESS ) {
        warning_msg( stderr, 0, mpi_ret, NULL, "Unable to open %s",
                params->tfname );
        return -1;
    }

    MPI_File_get_info( wfh, &info_used );
    MPI_Info_get_nkeys( info_used, &nkeys );
    keys   = (char **)Malloc(nkeys * sizeof(char *), stderr, 0 );
    values = (char **)Malloc(nkeys * sizeof(char *), stderr, 0 ); 
    for( i = 0; i < nkeys; i++ ) {
        keys[i]   = (char*)Malloc(200 * sizeof(char), stderr, 0);
        values[i] = (char*)Malloc(200 * sizeof(char), stderr, 0);
        assert( keys[i] && values[i] );
        MPI_Info_get_nthkey(info_used, i, keys[i] );
        MPI_Info_get( info_used, keys[i], 200, values[i], &dummy_int );
        printf( "Key: %s = %s\n", keys[i], values[i] );
        if ( strstr( keys[i], "panfs_" ) ) {
            insert_panfs_info( keys[i], values[i] );
        }
    }
    MPI_File_close( &wfh );
#endif
    return 0;
}

// stuff for the hash table
static unsigned int
hashfromkey(void *ky)
{
    unsigned int sum = 0, i;
    char *str = (char*)ky;
    for( i = 0; i < strlen( str ); i++ ) {
        sum += (size_t)str[i];
    }
    return sum;
}

static int
equalkeys(void *k1, void *k2)
{
    return (0 == strcmp((char*)k1,(char*)k2));
}

int
collect_additional_config( struct Parameters *params, int rank, int argc,
        char **argv ) 
{
    /*****************************************************************
    * Check large file capability
    ******************************************************************/
    if (rank == 0){
        if (sizeof(off_t) < BIG_FILE_OFFSET) {
            fprintf(stderr,
              "64 bit file size addressing not supported on this platform\n");
            MPI_Finalize();
        }
    }

        // this used to be inside the #if defined but they need to be done
        // regardless since we use it in expand_path
    int mpi_ret;
    params->test_time = time(NULL);
    if ( (mpi_ret = 
            MPI_Bcast( &(params->test_time), 1, MPI_LONG, 0, MPI_COMM_WORLD ) )
            != MPI_SUCCESS ) 
    {
        fatal_error( stderr, rank, mpi_ret, NULL, "MPI_Bcast" );
    }
#if defined(MYSQL_HOST) || defined(MYSQL_FILE)
    char *panfs_version;
    char *mount_pt;
    struct passwd *pw;
    struct tm *mytime;
    struct utsname utsbuf;
    time_t seconds;
    int resultlen  = 0;
    int mpi_version    = 0;
    int mpi_subversion = 0;
    int arg_len = 0;
    int i = 0;

    EXIT_UNLESS_DB(0);

    //set_hostlist( params, rank ); // migrated to env_to_db.tcsh
    //return;   // if we return here, no bug
    //fprintf( stderr, "%d Received key %s\n", rank, params->db_key );

    params->db_key     = (char*)Malloc(512, stderr, rank);
    memset( params->db_key, 0, 512 );
    sprintf( params->db_key, "UNINITIALIZED\n" );
    if ( rank == 0 ) {
        int ret;
        struct statvfs statfs_buf;

        params->system     = (char*)Malloc(MPI_MAX_PROCESSOR_NAME, stderr,rank);
        params->datestr    = (char*)Malloc(16, stderr, rank);
        params->mpiversion = (char*)Malloc(16, stderr, rank);

        MPI_Get_processor_name( params->system, &resultlen );
        MPI_Get_version( &mpi_version, &mpi_subversion );
        snprintf( params->mpiversion, 16, "%d.%d", mpi_version,mpi_subversion);

        seconds = time(NULL);
        mytime = localtime( &seconds );
        snprintf( params->datestr, 16, "%d-%d-%d",
                mytime->tm_year+1900, mytime->tm_mon + 1, mytime->tm_mday );

        pw                = getpwuid(getuid());
        if ( pw ) {
            params->user      = strdup( pw->pw_name );
        } else {
            params->user = "GETPWUID FAILED";
        }

        mount_pt  = find_mountpoint( params->tfname );

        // still do server version internally because env_to_db.tcsh cannot handle this.... 
        // at least for now
//        fprintf( stderr, "Finding panfs server %s\n", mount_pt );
//        panfs_version = find_server_version(mount_pt, params->tmpdirname);
        panfs_version = find_server_version(params->tfname, params->tmpdirname);
        if ( panfs_version ) {
            params->panfs_srv = strdup( panfs_version );
        }
        // Get os version
        if ( uname( &utsbuf ) == 0 ) {
            params->os_version = strdup( utsbuf.release );
        } else {
            perror( "uname" );
        }
        snprintf( params->db_key, 512, 
                "date_ts=%ld AND user=\'%s\' AND system=\'%s\'",
                params->test_time, params->user, params->system );
        printf( "DB_KEY %s\n", params->db_key );

      // create a string in the params to hold the full set of args
      //  fprintf( stderr, "Finding full args from %d args\n", argc );
      for( i = 0; i < argc; i++ ) {
          arg_len += 1 + strlen( argv[i] );
          //fprintf( stderr, "Arg %d: %s\n", i, argv[i] );
      } 


      params->full_args = (char *)Malloc( arg_len + 10, stderr, rank ); 
      params->full_args[0] = '\0';
      for( i = 0; i < argc; i++ ) {
          int cur_len = strlen(params->full_args);
          snprintf( &(params->full_args[cur_len]), 
                  arg_len - cur_len,
                  "%s ", argv[i] );
      }
    }

    // create a string in the params to hold extra key/value pairs
    int failure=0;
    int stderr_redirect=FALSE;
    if ( rank == 0 && getenv( "FS_TEST_EXTRA" ) && ! params->no_extra ) {
        fprintf( stderr, "Reading extra database info from %s\n",
                getenv("FS_TEST_EXTRA") );
        params->ht = create_hashtable(16, hashfromkey, equalkeys);
        char extra_command[1024];
        snprintf( extra_command, 1024, "%s %s", 
                getenv( "FS_TEST_EXTRA" ), params->tfname );
        char *extra_info = get_output( extra_command, params->tmpdirname, stderr_redirect ); 

        char *delimiter = "\n";
        char *key_delimiter = " ";
        char *str_ptr = strtok( extra_info, delimiter ); 
        char *key, *val;

		// setup the sscanf format string bec we need to create it ourselves
		// since sscanf can't accept a varible field width
		int key_size=512;
		int val_size=4096;
        int keys_read=0;
		char sscanf_format[64];
		snprintf( sscanf_format, 64, "%%%ds %%%ds", key_size, val_size ); 

        while( str_ptr ) {
			// we can't free these bec the hashtable only stores pointers 
			// the db insert needs the values later
            key = Malloc(key_size, stderr, rank );
            val = Malloc(val_size, stderr, rank );

            if ( 2 == sscanf( str_ptr, sscanf_format, key, val ) ) {
                printf( "Scanned extra info %s => %s\n", key, val );
                // the plfsrc can now have multiple mount points, so make sure
                // no keys are repeated
                hashtable_remove(params->ht, key);
                if ( ! hashtable_insert( params->ht, key, val ) ) {
                    fprintf( stderr, "ERROR: Unable to insert into hash!\n" );
                    failure = 1;
                    break;
                }
                if ( ! strcmp( key, "ERROR" ) || ! strcmp( key, "error" ) ) {
                    if ( rank == 0 ) {
                        fprintf( stderr, 
                                "ERROR: FS_TEST_EXTRA reported failure: %s\n",
                                val );
                        failure = 1;
                        break;
                    }
                }
                keys_read++;
            }
            str_ptr = strtok( NULL, delimiter );
        }
        if ( ! keys_read ) {
            fprintf( stderr, 
                    "ERROR: FS_TEST_EXTRA (%s) produced no valid results.\n",
                    getenv("FS_TEST_EXTRA" ) );
            failure=1;
        }
    }

        // share the failure status with the rest of the ranks
    if ( (mpi_ret = MPI_Bcast( &failure, 1, MPI_INT, 0, MPI_COMM_WORLD ) )
            != MPI_SUCCESS ) 
    {
        fatal_error( stderr, rank, mpi_ret, NULL, "MPI_Bcast" );
    }

    if ( failure ) {
        MPI_Finalize();
        exit( 1 );
    }

    if ( rank == 0 ) {
        //printf( "Full nodelist %s\n", params->host_list );
        //printf( "Procs_per_node %.3f\n", params->procs_per_node );
        printf( "Full args %s\n", params->full_args );
    }
    //
        // share the db_key with the rest of the ranks
        //fprintf( stderr, "Bcasting %s\n", params->db_key );
    if ( (mpi_ret = 
            MPI_Bcast( params->db_key, 512, MPI_BYTE, 0, MPI_COMM_WORLD ) )
            != MPI_SUCCESS ) 
    {
        fatal_error( stderr, rank, mpi_ret, NULL, "MPI_Bcast" );
    }
#endif
    return 0;
}

// macros for turning macros into quoted strings
#define xstr(s) str(s)
#define str(s) #s

#define PASS_FD_MSG_SIZE 2048
#define FULL_QUERY_SIZE 262144
#define QUERY_SIZE 131072
#define QUERY_PATH 512

// all procs need to make this call because they do allgathers to collect
// timing info in addDBTimes, also conclude with a Barrier to keep everybody
// sync'd
int
db_insert(  int my_rank,
            int last_errno,
            char *error_message,
            struct Parameters *params, 
            struct State *state,
            struct time_values *write_times,
            struct time_values *read_times, 
            struct time_info *thread_times )
{
#if defined(MYSQL_HOST) || defined(MYSQL_FILE)
    static int initial = 1;
    static int connected = 0;
    static char default_path[QUERY_PATH];
    char query[FULL_QUERY_SIZE];
    char values[QUERY_SIZE];
    char columns[QUERY_SIZE];
    char mixed[QUERY_SIZE];
    //char mpi_version[16];
    //char user[64];
#ifdef MYSQL_HOST
    MYSQL mysql;
    char *host  = xstr(MYSQL_HOST); 
    unsigned int connect_timeout = 15;
    char *query_file = NULL;
#else 
    char *host = NULL;
    char *query_file  = xstr(MYSQL_FILE);
#endif
    char *database = "mpi_io_test";
    char *experiment_table = "experiment";
    char *thread_table = "mpi_threads";
    char *table;
    char *mpihome;
    char *mpihost;
    char *segment;

    int  error_handler = 0;
    int successful_query_write = 0;
    int suppress_reminder = 0;


    // default table = experiment otherwise check if  mpi_threads table
    table = experiment_table;    
    if ( params->numthreads ) {
        table = thread_table;
    }

    //char *myversion = xstr( MPI_IO_TEST_VERSION );

    /* turn this off now bec we just allow whichever rank wants to push
       in the error message */

    // are we in special mode where we just handle an error?
    if ( error_message ) {
        chomp(error_message);
        error_handler = 1;
        if ( ! strcmp( error_message, "partial" ) ) {
            error_handler = 0;
        }
            // only rank 0 should push the NULL at the end of the program
        if ( ! strcmp( error_message, "NULL" ) ) {
            if ( my_rank != 0 ) {
                return 1;
            }
        }
    }

    mpihome = getenv("MPIHOME");
    mpihost = getenv("MY_MPI_HOST");
    segment = getenv("HOSTNAME");
    values[0] = columns[0] = mixed[0] = '\0';  // null term

        // connect to db
    if ( my_rank == 0 || error_handler ) {
        if ( host == NULL ) {
            //assert( query_file );   // don't even try, just put in a file
            connected = 0;
        } else if ( mpihost && ! strcmp( mpihost, "flash" ) ) {
            // flash can't connect, don't even try
            connected = 0;
        } else

            // if connected == 1, this means we connected before, so try again
        if ( initial || connected ) {   // attempt to connect at least once
                                        // cant connect from bproc backend...
#ifdef MYSQL_HOST
            if ( my_rank != 0 ) {
                fprintf( stderr, "%d mysql_init with %s\n", my_rank,
                        error_message );
            }
            mysql_init(&mysql);
            mysql_options(&mysql, MYSQL_OPT_CONNECT_TIMEOUT,  
                    (char*) &connect_timeout);
            if (!mysql_real_connect(&mysql, 
                                    host,
                                    NULL,   // "mpi" for eureka
                                    NULL,   // "mpi" for eureka
                                    database,
                                    0,
                                    NULL,
                                    0 ) )
            {
                    // shoot, fatal_error would be recursive
                fprintf(stderr, "%d Failed to connect to database %s: %s (error %s)\n",
                    my_rank, host, mysql_error(&mysql), 
		    ( error_message ? error_message : "NULL" ) );
                connected = 0;  // only timeout on connect once
            } else {
                connected = 1;
                //debug_msg( stderr, 0, "Connected to db\n" );
            }
#else
            connected = 0;
#endif
        }
    }

    char *version = "$Revision: 1.6 $";

        // create the query string
    if ( ! error_handler ) {
        if ( initial ) {
            addDBStr( QUERY_SIZE, columns, values, mixed, params->barriers,
                "barriers" );
            addDBStr( QUERY_SIZE, columns, values, mixed, params->user,
                "user" );
            addDBStr( QUERY_SIZE, columns, values, mixed, params->mpiversion,   
                "mpi_version" );
            addDBStr( QUERY_SIZE, columns, values, mixed, version,  
                "version" ); 
            addDBStr( QUERY_SIZE, columns, values, mixed, params->system,       
                "system" );
            addDBStr( QUERY_SIZE, columns, values, mixed, params->hints,        
                "hints" );
            addDBStr( QUERY_SIZE, columns, values, mixed, params->tfname,       
                "target" );
            addDBStr( QUERY_SIZE, columns, values, mixed, params->ofname,       
                "output_file" );
            addDBStr( QUERY_SIZE, columns, values, mixed, params->experiment,   
                "description" );
            addDBStr( QUERY_SIZE, columns, values, mixed, params->io_type_str,
                "io_type"     );
            addDBStr( QUERY_SIZE, columns, values, mixed, mpihome,              
                "mpihome" );
            addDBStr( QUERY_SIZE, columns, values, mixed, segment,              
                "segment" );
            addDBStr( QUERY_SIZE, columns, values, mixed, mpihost,              
                "mpihost" );
            addDBStr( QUERY_SIZE, columns, values, mixed, params->datestr,      
                "yyyymmdd" );
            addDBStr( QUERY_SIZE, columns, values, mixed, params->full_args,    
                "full_args" );
            addDBStr( QUERY_SIZE, columns, values, mixed, params->full_hints,   
                "full_hints");
            addDBStr( QUERY_SIZE, columns, values, mixed, params->panfs_srv,    
                "panfs_srv" );
            addDBStr( QUERY_SIZE, columns, values, mixed, params->os_version,
                "os_version" );
            addDBStr( QUERY_SIZE, columns, values, mixed, params->host_list,    
                "host_list" );
            addDBStr( QUERY_SIZE, columns, values, mixed, error_message,        
                "error" );
            addDBInt( QUERY_SIZE, columns, values, mixed, params->test_time,
                "date_ts"  );
            addDBInt( QUERY_SIZE, columns, values, mixed, params->test_type,   
                "test_type");
            addDBInt( QUERY_SIZE, columns, values, mixed, params->num_objs,  
                "num_objs"  );
            addDBInt( QUERY_SIZE, columns, values, mixed, params->obj_size,   
                "obj_size"  );
            addDBInt( QUERY_SIZE, columns, values, mixed, params->touch, 
                "touch" );
            addDBInt( QUERY_SIZE, columns, values, mixed, params->strided_flag, 
                "strided" );
            addDBInt( QUERY_SIZE, columns, values, mixed, params->sync_flag,   
                "sync" );
            addDBInt( QUERY_SIZE, columns, values,mixed,params->write_only_flag,
                "write_only");
            addDBInt( QUERY_SIZE, columns, values, mixed,params->read_only_flag,
                "read_only" );
            addDBInt( QUERY_SIZE, columns, values, mixed, params->sleep_seconds,
                "sleep" );
            addDBInt( QUERY_SIZE, columns, values, mixed, last_errno,          
                "errno" );
            addDBInt( QUERY_SIZE, columns, values,mixed,params->collective_flag,
                "collective");
            addDBInt( QUERY_SIZE, columns, values,mixed,params->num_procs_world,
                "num_hosts" );
            addDBInt( QUERY_SIZE, columns, values, mixed, 
                    params->shift_flag,   
                "read_shift" );
            addDBInt( QUERY_SIZE, columns, values, mixed, 
                    params->plfs_flatten,   
                "plfs_flatten" );
            addDBInt( QUERY_SIZE, columns, values, mixed, 
                    params->max_offset,
                    "max_offset" );
            addDBInt( QUERY_SIZE, columns, values, mixed, 
                    params->max_size,
                    "max_size" );
            addDBInt( QUERY_SIZE, columns, values, mixed, 
                    params->time_limit,   
                "time_limit" );
            addDBInt( QUERY_SIZE, columns, values, mixed, 
                    (params->io_type == IO_POSIX ? 1 : 
                     params->io_type == IO_MPI   ? 0 :
                     params->io_type == IO_PLFS  ? 2 : -1 ), "posix_io" );
            addDBInt( QUERY_SIZE, columns, values, mixed, 
                    params->check_data_ndx,  "check_data_ndx" );
            addDBInt( QUERY_SIZE, columns, values, mixed, params->superblocks,  
                "superblocks" );
            addDBFloat( QUERY_SIZE, columns, values, mixed, 
            ((float)params->num_objs* params->obj_size* params->num_procs_world
             * params->superblocks )
                / (1024.0 * 1024), "total_size_mb" );
            // for threads
            if ( params->numthreads ) {
                addDBInt( QUERY_SIZE, columns, values,mixed,params->num_procs_world,
                          "num_pes" );
                addDBInt( QUERY_SIZE, columns, values,mixed,params->totalthreads,
                          "total_threads" );
            }

            /*
               // migrated to env_to_db.tcsh
               addDBFloat( QUERY_SIZE, columns, values, mixed, 
                    params->procs_per_node, "procs_per_node" );
            */

                // add any extra scraped info
            if ( params->ht != NULL ) {
                struct hashtable_itr *itr;
                if (hashtable_count(params->ht) > 0) {
                    itr = hashtable_iterator(params->ht);
                    do {
                        char *k = (char*)hashtable_iterator_key(itr);
                        char *v = (char*)hashtable_iterator_value(itr);
                        addDBStr( QUERY_SIZE, columns, values, mixed, v, k );
                    } while (hashtable_iterator_advance(itr));
                }
                free(itr);
            }
        }

        if ( write_times ) {
            addDBTimes( my_rank, params->num_procs_world,
                QUERY_SIZE, columns, values, mixed, write_times, "write" );

            // need to add state->total_mbs here when time_limit or max_size
            if ( params->time_limit || params->max_size ) {
                addDBFloat( QUERY_SIZE, columns, values, mixed, 
                        (float)state->total_mbs,
                        "total_size_mb" );
                addDBInt( QUERY_SIZE, columns, values, mixed, state->ave_objs,  
                        "num_objs"  );
                addDBInt( QUERY_SIZE, columns, values, mixed, state->min_objs, 
                        "num_objs_min"  );
                addDBInt( QUERY_SIZE, columns, values, mixed, state->max_objs,
                        "num_objs_max"  );
                addDBInt( QUERY_SIZE, columns, values, mixed, params->supersize,
                        "supersize" );
            }
        }
        if ( read_times ) {
            addDBTimes( my_rank, params->num_procs_world,
                QUERY_SIZE, columns, values, mixed, read_times,  "read" );
            if ( params->max_size && params->read_only_flag ) {
                addDBFloat( QUERY_SIZE, columns, values, mixed, 
                        (float)state->total_mbs,
                        "total_size_mb" );
            }
        }

        // nton threaded program call
        if ( thread_times && my_rank==0 ) {
            addDBTimes_threads(QUERY_SIZE, columns, values, mixed,        
                                   thread_times, params, state );
        }

        if ( params->delete_flag && 
                (read_times || (write_times && params->write_only_flag) ) )
        {
            struct time_values *times = (read_times ? read_times : write_times);
            addDBTime( my_rank, params->num_procs_world, 
                    QUERY_SIZE, columns, values, mixed, 
                        times->file_open_wait_time, 
                        NULL, "unlink_time" );
        }

        if ( params->panfs_info_valid ) {
            // still do this within code because panfs_cw not available to tpf
            addDBInt( QUERY_SIZE, columns, values, mixed,
                    params->panfs_concurrent_write, "panfs_cw" );
        }

        int ret = 0;
        if (strlen(columns)>=(QUERY_SIZE-1) || strlen(values)>=(QUERY_SIZE-1)) {
            fprintf( stderr, "Need to grow db strings\n" );
            MPI_Abort( MPI_COMM_WORLD, -1 );
        }
        if ( initial ) {
            ret = snprintf( query, FULL_QUERY_SIZE, 
                    "INSERT INTO %s (%s) VALUES (%s)", 
                table, columns, values ); 
        } else {
            ret = snprintf( query, FULL_QUERY_SIZE, 
                    "UPDATE %s set %s WHERE %s", 
                table, mixed, params->db_key );
        }
    } else {
        if ( ! strcmp( error_message, "NULL" ) ) {  // this means clear the
                                                    // partial
                // add additional info to only clear if set to partial
                // so if someone else (like in negative number) set a 
                // non-fatal error, we won't overwrite.
            int ret = snprintf( query, FULL_QUERY_SIZE, 
                "UPDATE %s set error = NULL WHERE %s AND error like 'partial'",
                table, params->db_key ); 
            suppress_reminder = 1;  // a reminder was just set after the read
            error_handler = 1;  // only rank 0 should send this one
                                // everyone else already exited
            if ( ret >= FULL_QUERY_SIZE ) {
                fprintf( stderr, "query truncated!!! NEED TO FIX\n" );
                MPI_Abort( MPI_COMM_WORLD, -1 );
            }
        } else {
            // we could concatenate here to merge multiple error states
            // but this would also concatenate with the partial
            // so first we need to clear the partial and then we
            // need to concatenate the error string and use coalesce so
            // it gets rid of the NULL
            //UPDATE table SET fieldName = 
            // CONCAT(fieldName, 'New Data To Add Here') WHERE ...
            //UPDATE table SET field = 
            // CONCAT(COALESCE(field, ''), 'New Data') WHERE ...
            int ret = snprintf( query, FULL_QUERY_SIZE, 
                "UPDATE %s set error=CONCAT(COALESCE(error,''),\'%s\'),"
                "errno=%d WHERE %s",
                table, error_message, last_errno, params->db_key ); 
            if ( ret >= FULL_QUERY_SIZE ) {
                fprintf( stderr, "query truncated!!! NEED TO FIX\n" );
                MPI_Abort( MPI_COMM_WORLD, -1 );
            }
        }
    }

        // send the query string
    if ( my_rank == 0 || error_handler ) {
        if ( connected ) {
#ifdef MYSQL_HOST
            if ( mysql_query( &mysql, query ) != 0 ) {
                fprintf(stderr, "%d Failed to send query %s: %s\n",
                    my_rank, query, mysql_error(&mysql));
            } else {
                successful_query_write = 1;
            }
            mysql_close( &mysql );
#endif
        } else {
            if ( ! query_file ) {
                query_file = default_path; 
                snprintf( query_file, 512, "%s/db_up", getenv("HOME") );
            }
            assert( query_file );
            FILE *fp = fopen( query_file, "a" );
            if ( ! fp ) {
                fprintf(stderr, 
                        "Failed to open %s for append: %s\n", 
                        query_file, strerror(errno));
            } else {
                if ( state ) {
                    fprintf( state->ofptr, "Locking db up file . . . " );
                    fflush( state->ofptr );
                }
                // need to put a ; and a \n on the back of the query
                int ret = snprintf( &(query[strlen(query)]), 
                        FULL_QUERY_SIZE -strlen(query), ";\n" );
                if ( ret >= FULL_QUERY_SIZE ) {
                    fprintf( stderr, "query truncated!!! NEED TO FIX\n" );
                    MPI_Abort( MPI_COMM_WORLD, -1 );
                }
                lockf( fileno( fp ), F_LOCK, 0 ); // lock
                ret = fputs( query, fp );
                if ( ret == EOF ) {
                    fprintf(stderr, "wtf fputs failed!! %s\n", strerror(errno));
                    MPI_Abort( MPI_COMM_WORLD, -1 );
                }
                rewind( fp ); // seek to 0, so unlock will work
                if ( state ) {
                    fprintf( state->ofptr, "unlocking db up file\n" );
                }
                lockf( fileno( fp ), F_ULOCK, 0 ); // unlock
                fclose( fp );
                successful_query_write = 1;
            }
        }

        if ( successful_query_write ) {
            if ( connected ) {
                debug_msg( stdout, my_rank, "INSERT'd %ld -> mysql %s:%s:%s\n",
                    params->test_time, host, database, table );
            } else {
                if ( ! suppress_reminder ) {
                    debug_msg(stdout, my_rank, 
                        "Query in %s needs to be uploaded\n", query_file );
                }
            }
        } else {
            debug_msg( stdout, my_rank, "Was unable to write query\n" );
        }

        initial = 0;
    }

        // this is the normal case
        // do a barrier to get everybody sync'd for the next operation
        // if error_handler, means some individual rank by themselves
        // has failed and is doing this call individually, so a Barrier
        // would not be good.  But in the normal (i.e. non-error) case,
        // everybody comes here so do a barrier so they're all sync'd up
        // afterwards so any subsequent timed open's are not skewed 
        // because of rank 0 being the only one doing the db query
    if ( ! error_handler ) {
      MPI_Barrier(MPI_COMM_WORLD);
    }

    return 1;    
#else
    return 1;
#endif
}

ssize_t 
Write(int fd, const void *vptr, size_t nbyte ) {
    size_t      nleft;
    ssize_t     nwritten = 0;
    const char  *ptr;

    ptr = (char*)vptr;
    nleft = nbyte;
    while (nleft > 0) {
        ssize_t this_write = 0;
        if ( (this_write = write(fd, ptr, nleft ) ) <= 0) {
            if (errno == EINTR) {
                nwritten = 0;       /* and call write() again */
            } else {
                printf( "write to %d error: %s\n", fd, strerror(errno) );
                return(-1);         /* error */
            }
        }
        nleft    -= this_write;
        ptr      += this_write;
        nwritten += this_write;
    }
    assert( (size_t)nwritten == nbyte );
    return(nbyte);
}

ssize_t 
Read(int fd, void *vptr, size_t nbyte ) {
    size_t      nleft;
    ssize_t     nread = 0;
    char  *ptr;

    ptr = (char*)vptr;
    nleft = nbyte;
    while (nleft > 0) {
        ssize_t this_read = 0;
        if ( (this_read = read(fd, ptr, nleft ) ) <= 0) {
            if (errno == EINTR) {
                nread = 0;       /* and call write() again */
            } else {
                printf( "read from %d error: %s\n", fd, strerror(errno) );
                return(-1);         /* error */
            }
        }
        nleft    -= this_read;
        ptr      += this_read;
        nread    += this_read;
    }
    assert( (size_t)nread == nbyte );
    return(nbyte);
}

ssize_t 
Pwrite64(int fd, const void *vptr, size_t nbyte, off_t offset) {
    size_t      nleft;
    ssize_t     nwritten = 0;
    const char  *ptr;

    ptr = (char*)vptr;
    nleft = nbyte;
    while (nleft > 0) {
        //printf( STDERR, "pwrite64 %d %x %lld %lld
	ssize_t this_write = 0;
#if defined(CADDY) // Need this because Cadillac is a 32 bit machine
        if ( (this_write = pwrite64(fd, ptr, nleft, offset + nwritten) ) <= 0) {
#else 
        if ( (this_write = pwrite(fd, ptr, nleft, offset + nwritten) ) <= 0) {
#endif
            if (errno == EINTR) {
                nwritten = 0;       /* and call write() again */
            } else {
                printf( "pwrite to %lld error: %s\n", 
                        (long long)offset,
                        strerror(errno) );
                return(-1);         /* error */
            }
        }
        nleft    -= this_write;
        ptr      += this_write;
	nwritten += this_write;
    }
    assert( (size_t)nwritten == nbyte );
    //printf( "pwrite64 successful!\n" );
    return(nbyte);
}

ssize_t 
Pread64(int fd, void *vptr, size_t nbyte, off_t offset) {
    size_t      nleft;
    ssize_t     nread = 0;
    char  *ptr;

    ptr = (char*)vptr;
    nleft = nbyte;
    while (nleft > 0) {
#if defined(CADDY) // Need this because Cadillac is a 32 bit machine
        if ( (nread = pread64(fd, ptr, nleft, offset + nread) ) <= 0) {
#else
        if ( (nread = pread(fd, ptr, nleft, offset + nread) ) <= 0) {
#endif
            if (errno == EINTR)
                nread = 0;       /* and call write() again */
            else
                return(-1);         /* error */
        }
        nleft -= nread;
        ptr   += nread;
    }
    assert( (size_t)nread == nbyte );
    return(nbyte);
    /*
ssize_t 
Pread64(int fildes, void *buf, size_t nbyte, off_t offset) {
	int ret;
	do {
		ret = pread64( fildes, buf, nbyte, offset );
		if ( ret == EAGAIN ) {
			perror( "pwrite64" );
		}
	} while ( ret == EAGAIN ); 
    return nbyte;
    */
}

void
add_mpi_error_string( char *msg, int msg_len, int mpi_err ) {
    // stupid error strings are not valid in mpich and don't work in
    // lampi, because program is directly aborted by romio lib
    // just save the number . . . 
    snprintf( &msg[strlen(msg)], msg_len - strlen(msg),
                " (MPI_Error = %d)", mpi_err );
    return;
    /*
    char err_msg[MPI_MAX_ERROR_STRING];
    int err_len, error_class;
    MPI_Error_string( mpi_err, err_msg, &err_len );
    snprintf( &msg[strlen(msg)], msg_len - strlen(msg),
                "\nMPI_Error_string(%d)= %s", mpi_err, err_msg );
    fprintf( fp, "mpi error %d = %s\n", mpi_err, err_msg );
    MPI_Error_class(mpi_err, &error_class);
    MPI_Error_string( error_class, err_msg, &err_len );
    snprintf( &msg[strlen(msg)], msg_len - strlen(msg),
            "\nMPI_Error_string(class %d)=%s", error_class, err_msg );
    fprintf( fp, "mpi code %d = %s\n", error_class, err_msg );
    */
}
    

#define ERR_MSG_LEN 4096

int
make_error_messages( int my_rank, char *hdr, char *full, char *query, int len, 
        const char *format, va_list args, MPI_Status *mstat, int mpi_ret )
{
    char myname[256];
    char *newline  = NULL;
    memset( full, 0, len );
    memset( query, 0, len );
    gethostname( myname, 256 );
    vsnprintf( query, len, format, args );
    va_end( args );
    chomp( query );
    if ( errno ) {
        snprintf( &query[strlen(query)], 
                len - strlen(query),
                " (errno=%s)", strerror(errno) );
    }
    if ( mstat && mstat->MPI_ERROR != MPI_SUCCESS ) {
        add_mpi_error_string( query, len, mstat->MPI_ERROR );
    }
    if ( mpi_ret != MPI_SUCCESS ) {
        add_mpi_error_string( query, len, mpi_ret );
    }
    snprintf( full, len, "Rank %d Host %s %s %ld: %s", 
            my_rank, myname, hdr, time(NULL), query );
    strcpy( query, full );

    /*
       // was trying to send error message to rank 0, but problem is
       // that rank 0 can't catch the MPI_Abort . . . 
    if ( my_rank == 0 ) {
    } else {
        if ( MPI_Send( solo_error, strlen(solo_error), MPI_BYTE, 0, ERROR_TAG, 
                    MPI_COMM_WORLD) != MPI_SUCCESS ) 
        {
            fprintf(fp, "%d Couldn't send %s to rank 0\n", my_rank, solo_error);
        }
    }
    */

    while( ( newline = strstr( query, "\n" ) ) != NULL ) {
        *newline = '.';
    }
    if ( strstr( query, "\n" ) ) {
        fprintf( stderr, "Need to remove all newlines before sending to db\n" );
    }
        // append a comma
    snprintf( &query[strlen(query)], len - strlen(query), "," );

    /*
    MPI_Errhandler_set(MPI_COMM_WORLD, MPI_ERRORS_RETURN);
    error_code = MPI_Send(send_buffer, strlen(send_buffer) + 1, MPI_CHAR,
                              addressee, tag, MPI_COMM_WORLD);
    if (error_code != MPI_SUCCESS) {
        char error_string[BUFSIZ];
        int length_of_error_string, error_class;
        MPI_Error_class(error_code, &error_class);
        MPI_Error_string(error_class, error_string, &length_of_error_string);
        fprintf(stderr, "%3d: %s\n", my_rank, error_string);
        MPI_Error_string(error_code, error_string, &length_of_error_string);
        fprintf(stderr, "%3d: %s\n", my_rank, error_string);
        send_error = TRUE;
    }
    */
    return 0;
}

int
warning_msg( FILE *fp, int my_rank, int mpi_ret, MPI_Status *mstat,
        const char *format, ... ) 
{
    char message[ERR_MSG_LEN];
    char query[ERR_MSG_LEN];
    va_list args;   
    va_start( args, format );
    make_error_messages( my_rank, "WARNING ERROR", message, query, 
            ERR_MSG_LEN, format, args, mstat, mpi_ret );
    fprintf( fp, "%s\n", message );
    fflush( fp );
    if(params.trenddata){
       fprintf(stderr,"FAILED\n");
       exit(1);
    }
    EXIT_UNLESS_DB(0);
    return db_insert( my_rank, errno, query, &params, NULL, NULL, NULL, NULL );
}


void
setTime( double *value, double *value_time_start, double *value_time_end, 
         double initial, double end_time, double start_time,
         char *name, char *file, int line, FILE *efptr, int my_rank ) 
{
    if (initial < 0 || start_time < 0 || end_time < 0 || end_time < start_time){
            // do a warning msg and then allow to continue
        warning_msg( efptr, my_rank, 0, NULL,
                "Negative value passed from %s:%d for %s.  "
                "Init %f, end %f, start %f. "
                "MPI_WTIME_IS_GLOBAL %d", 
                file, line, name, initial, end_time, start_time,
                MPI_WTIME_IS_GLOBAL );
        *value = 0;
    } else {
        *value = initial + ( end_time - start_time );
        *value_time_start = start_time;
        *value_time_end = end_time;
    }
}

void
file_system_check( int my_rank ) {
    char *new_file;
    char myname[256];
    int fd;
    int my_errno = 0;
    char *my_err_msg = "";
    gethostname( myname, 256 );
    new_file = (char*)Malloc(strlen(params.tfname) + 10  + strlen(myname), 
            stderr, my_rank );
    memset( new_file, 0, strlen(params.tfname + 10 + strlen(myname)) );
    strcpy( new_file, params.tfname );
    sprintf( new_file,"%s", params.tfname );
    sprintf( &(new_file[strlen(new_file)]), ".%d.%s", my_rank, myname );

    fd = open( new_file, O_WRONLY | O_CREAT | O_TRUNC, 0x666 );
    if ( fd > 0 ) {
        unlink( new_file );
    } else {
        my_errno = errno;
        my_err_msg = strerror(errno);
    }
    fprintf( stderr, "FNF: Rank %d Host %s.  Open %s: %d %s\n",
            my_rank, myname,
            new_file, my_errno, my_err_msg );

        // enter infinite loop to try to preserve system state
    /*
    fprintf( stderr, "FNF: Rank %d Host %s.  Entering infinite loop.\n",
            my_rank, myname );
    while( 1 ) { }
    */
}

void 
insert_panfs_info( char *key, char *value ) {
    if ( ! strcmp( key, "panfs_layout_type" ) ) {
        params.panfs_type = atoi(value);
    } else if ( ! strcmp( key, "panfs_layout_stripe_unit" ) ) {
        params.panfs_stripe = atoi(value);
    } else if ( ! strcmp( key, "panfs_layout_parity_stripe_width" ) ) {
        params.panfs_width = atoi(value);
    } else if ( ! strcmp( key, "panfs_layout_parity_stripe_depth" ) ) {
        params.panfs_depth = atoi(value);
    } else if ( ! strcmp( key, "panfs_layout_total_num_comps" ) ) {
        params.panfs_comps = atoi(value);
    } else if ( ! strcmp( key, "panfs_layout_visit_policy" ) ) {
        params.panfs_visit = atoi(value);
    } else if ( ! strcmp( key, "panfs_concurrent_write" ) ) {
        params.panfs_concurrent_write = atoi(value);
    } else {
        fprintf( stderr, "Unknown panfs_key %s -> %s", key, value );
    }
    params.panfs_info_valid = 1;
}

int
fatal_error( FILE *fp, int my_rank, int mpi_ret, MPI_Status *mstat, 
		const char *format, ... ) 
{
    
    char message[ERR_MSG_LEN];
    char query[ERR_MSG_LEN];
    va_list args;   
    va_start( args, format );
    make_error_messages( my_rank, "FATAL ERROR", message, query, 
            ERR_MSG_LEN, format, args, mstat, mpi_ret );
    fprintf( fp, "%s\n", message );
    fflush( fp );
        // who cares, leave the damn partial in there....
        // doesn't work anyway maybe . . . makes it hang when it tries barrier
    //db_insert( my_rank, 0, "NULL", &params, NULL, NULL ); // clear the partial
    if ( ! params.db_key ) {
        fprintf( stderr, "Doing a fatal error before we set the db_key. Insert impossible\n" );
    } else {
        db_insert( my_rank, errno, query, &params, NULL, NULL, NULL, NULL );
    }
    if ( errno == ENOENT ) {
        file_system_check( my_rank );
    }
        // turn this off for speeding up debugging
    fprintf(fp,"[RANK %d] Waiting 60secs\n", my_rank);
    sleep( 60 );    // allow other nodes to fail or succeed first
    fflush(fp);

    if(!params.trenddata){
       fprintf(stderr, "FAILED\n");
    }

    MPI_Abort( MPI_COMM_WORLD, -1 );
    return 0;
}

int
debug_msg( FILE *fp, int my_rank, const char *format, ... ) {
    va_list args;   
    va_start( args, format );
    fprintf( fp, "Rank [%d] DEBUG: ", my_rank );
    vfprintf( fp, format, args );
    fflush( fp );
    va_end( args );
    return 1;
}


//#ifdef HAS_SCM_RIGHTS // this is broken
#if 0

// this functions receives a fd from the sd param and returns the fd 
int 
receive_fd( int sd, int rank, FILE *fp ) {
    
    struct iovec vector;    /* file name for the passed file - phony */
    struct msghdr msg;        /* full message */
    static struct cmsghdr *cmsg = NULL;
                            /* control message which includes the fd */
    int fd;

        // set up the iovec to read in the filename
    char filename[PASS_FD_MSG_SIZE];
    vector.iov_base = filename;
    vector.iov_len  = PASS_FD_MSG_SIZE;

        // the message we're expecting to receive
    msg.msg_name = NULL;
    msg.msg_namelen = 0;
    msg.msg_iov = &vector;
    msg.msg_iovlen = 1;

        // the control message
    if ( cmsg == NULL ) {
            // it will be NULL the first time the function is called
        cmsg = (struct cmsghdr *)
            Malloc( sizeof(struct cmsghdr) + sizeof(fd), fp, rank );
        cmsg->cmsg_len = sizeof(struct cmsghdr) + sizeof(fd);
    }

        // connect the message and the control message
    msg.msg_control = cmsg;
    msg.msg_controllen = cmsg->cmsg_len;
    
    if ( recvmsg( sd, &msg, 0 ) <= 0 ) {
        fatal_error( fp, rank, 0, NULL, "recvmsg failed %s", strerror(errno) );
    }

        // grab the file descriptor
    memcpy( &fd, CMSG_DATA( cmsg ), sizeof( fd ) );
    return fd;
}

// this function passes the param fd to the socket sd
int
send_fd( int sd, int fd, int rank, FILE *fp ) {
    struct iovec   vector;     /* some data to pass with fd */
    struct msghdr  msg;         /* the complete message */
    static struct cmsghdr *cmsg = NULL;
                            /* the control message which includes the fd */

        // send a bogus filename to make the code work
        // I can't figure out why this is necessary
    char filename[PASS_FD_MSG_SIZE];
    memset( filename, 0, PASS_FD_MSG_SIZE );
    snprintf( filename, sizeof(filename), "%s", "foo" );
    vector.iov_base = filename;
    vector.iov_len  = strlen( filename ) + 1;

        // assemble first part of message
    msg.msg_name = NULL;
    msg.msg_namelen = 0;
    msg.msg_iov = &vector;
    msg.msg_iovlen = 1;

        // assemble the control message
    if ( cmsg == NULL ) {
            // cmsg is NULL the first time the function is called
        cmsg = (struct cmsghdr *)Malloc(
                sizeof(struct cmsghdr) + sizeof(fd), fp, rank );
        cmsg->cmsg_len   = sizeof( struct cmsghdr) + sizeof( fd );
        cmsg->cmsg_level = SOL_SOCKET;
        cmsg->cmsg_type  = SCM_RIGHTS;
    }

        // copy the file descriptor onto the end of the control message
    memcpy( CMSG_DATA( cmsg ), &fd, sizeof( fd ) );

        // connect the message and the control message
    msg.msg_control = cmsg;
    msg.msg_controllen = cmsg->cmsg_len;

    if ( sendmsg( sd, &msg, 0 ) != (int) vector.iov_len ) {
        fatal_error( fp, rank, 0, NULL, "sendmsg failed %s", strerror(errno) );
    }

    return 0;
}

#elif 0

int
send_fd( int sd, int fd, int rank, FILE *fp ) {
    struct msghdr mh;
    struct cmsghdr cmh[0];
    struct iovec iov;

    memset(&mh,0,sizeof(mh));
    mh.msg_name = 0;
    mh.msg_namelen = 0;
    mh.msg_iov = &iov;
    mh.msg_iovlen = 1;
    mh.msg_control = (caddr_t)&cmh;
    mh.msg_controllen = sizeof(cmh) + sizeof(int);
    mh.msg_flags = 0;
    iov.iov_base = "hello";
    iov.iov_len = strlen(iov.iov_base) + 1;
    cmh[0].cmsg_level = SOL_SOCKET;
    cmh[0].cmsg_type = SCM_RIGHTS;
    cmh[0].cmsg_len = sizeof(cmh) + sizeof(int);
    *(int *)&cmh[0] = fd;
    if ( sendmsg(sd,&mh,0) < 0 ) {
        fatal_error( fp, rank, 0, NULL, "sendmsg %d: %s", 
                __LINE__, strerror(errno) );
    }
    return 1;
}

int
receive_fd( int sd, int rank, FILE *fp ) {
    struct msghdr mh;
    struct cmsghdr cmh[0];
    struct iovec iov;
    char tbuf[100];

    mh.msg_name = 0;
    mh.msg_namelen = 0;
    mh.msg_iov = &iov;
    mh.msg_iovlen = 1;
    mh.msg_control = (caddr_t)&cmh;
    mh.msg_controllen = sizeof(cmh[0]) * 2;
    iov.iov_base = tbuf;
    iov.iov_len = sizeof(tbuf);
    cmh[0].cmsg_len = sizeof(cmh[0]) + sizeof(int);
    if ( recvmsg(sd,&mh,0) < 0 ) {
        fatal_error( fp, rank, 0, NULL, "recvmsg %d: %s", 
                __LINE__, strerror(errno) );
    }
    return *(int *)&cmh[0];
}

#elif 0

int
receive_fd( int sd, int rank, FILE *fp ) {

    struct strrecvfd recvfd;

    if( ioctl(sd, I_RECVFD, &recvfd) < 0)
        fatal_error( fp, rank, 0, NULL, "ioctl in %s failed %s", 
                __FUNCTION__, strerror(errno) );
    else
        return recvfd.fd;

}

int
send_fd( int sd, int fd, int rank, FILE *fp ) {

    if( ioctl(sd, I_SENDFD, fd) < 0){
        fatal_error( fp, rank, 0, NULL, "ioctl in %s failed %s", 
                __FUNCTION__, strerror(errno) );
    }

    return 0;
}

#else

int 
receive_fd( int sd, int rank, FILE *fp ) {
    return -1;
}

int 
send_fd( int sd, int fd, int rank, FILE *fp ) {
    return -1;
}

#endif /* HAS_SCM_RIGHTS */

/*******************************************
 *   ROUTINE: get_min_sum_max
 *   PURPOSE: Collect minimum, sum, and maximum of input value across all 
 *            processors. Only processor with rank 0 will have the value and 
 *            rank of the processor with the maximum, minimum, and sum
 *******************************************/
int 
get_min_sum_max(int my_rank, 
                double base_num, double *min, int *min_ndx,
	        double *sum, double *max, int *max_ndx, char *op, 
	        FILE *ofptr, FILE *efptr)
{
  int mpi_ret;
  struct{
    double dval;
    int rank;
  } in, out;
  
  in.dval = base_num;
  in.rank = my_rank;
  
  if( (mpi_ret = MPI_Reduce(&in, &out, 1, MPI_DOUBLE_INT,
		MPI_MAXLOC, 0, MPI_COMM_WORLD)) != MPI_SUCCESS) 
  {
      fatal_error( efptr, my_rank, mpi_ret, NULL,
			  "Unable to find (reduce) maximum %s.\n", op);
  }
  
  *max = out.dval;
  *max_ndx = out.rank;
  MPI_Barrier(MPI_COMM_WORLD);

  if( (mpi_ret = MPI_Reduce(&base_num, sum, 1, MPI_DOUBLE,
		MPI_SUM, 0, MPI_COMM_WORLD)) != MPI_SUCCESS) 
  {
      fatal_error( efptr, my_rank, mpi_ret, NULL,
			  "Unable to find (reduce) sum of %s.\n", op);
  }
  
  MPI_Barrier(MPI_COMM_WORLD);

  if( (mpi_ret = MPI_Reduce(&in, &out, 1, MPI_DOUBLE_INT,
		MPI_MINLOC, 0, MPI_COMM_WORLD)) != MPI_SUCCESS) 
  {
      fatal_error( efptr, my_rank, mpi_ret, NULL,
			  "Unable to find (reduce) minimum %s.\n", op);
  }
  
  *min = out.dval;
  *min_ndx = out.rank;

  return MPI_SUCCESS;
}

/*******************************************
 *   ROUTINE: get_all_proc_min_max_time
 *   PURPOSE: implements max(reduce(t2)) - min(reduce(t1)) in order
 *            to find the max time delta over all processes as opposed 
 *            to get_min_sum_max which finds the max delta on a process
 *            max(reduce(t2-t1))
 *******************************************/
void
get_all_proc_min_max_time(
        int     my_rank,
        double  rank_min_value,
        double  rank_max_value,
        char    *op,
        FILE    *ofptr,
        FILE    *efptr,
        double  *elapsed_value)
{

    double min_value, time_sum, max_value;
    int    min_rank, max_rank;
    int    mpi_ret;
    double reduce_min_value;


    if( (mpi_ret = get_min_sum_max(my_rank, rank_min_value, 
         &min_value, &min_rank, &time_sum, &max_value, &max_rank,
         op, ofptr, efptr)) != MPI_SUCCESS)
    {
        fatal_error( efptr, my_rank, mpi_ret, NULL,
                     "Problem computing min, max, and sum "
                     "of %s time.\n", op );
    }
    reduce_min_value = min_value;

    if( (mpi_ret = get_min_sum_max(my_rank, rank_max_value, 
         &min_value, &min_rank, &time_sum, &max_value, &max_rank,
         op, ofptr, efptr)) != MPI_SUCCESS)
    {
        fatal_error( efptr, my_rank, mpi_ret, NULL,
                     "Problem computing min, max, and sum "
                     "of %s time.\n", op );
    }
    *elapsed_value = max_value  - reduce_min_value;
}


#if 0
int
get_connecting_socket( int port, int rank, FILE *fp ) {
    struct hostent *hostptr;
    struct sockaddr_in serv_addr;
    int sockfd;

    if ((hostptr = gethostbyname("localhost")) == NULL) {
        fatal_error( fp, rank, 0, NULL, "gethostbyname %s", strerror(errno) );
    }

    bzero((char *)&serv_addr,sizeof(serv_addr));
    serv_addr.sin_family = AF_INET;
    memcpy((char *)&serv_addr.sin_addr,(char *)hostptr->h_addr,
            hostptr->h_length);
    serv_addr.sin_port = htons(port);

    if ((sockfd = socket(AF_INET,SOCK_STREAM,0)) < 0) {
        fatal_error( fp, rank, 0, NULL, "socket %s", strerror(errno) );
    }
    if ( connect(sockfd,(struct sockaddr *)&serv_addr,sizeof(serv_addr)) < 0)
    {
        fatal_error( fp, rank, 0, NULL, "connect %s", strerror(errno) );
    }
    return sockfd;
}

int 
get_listening_socket( int port, int backlog, int rank, FILE *fp ) {
    int sockfd;
    struct sockaddr_in serv_addr;

    // Create a new TCP socket...
    if ((sockfd = socket(AF_INET,SOCK_STREAM,0)) < 0) {
        fatal_error( fp, rank, 0, NULL, "socket failed %s", strerror(errno) );
    }

    bzero((char *)&serv_addr,sizeof(serv_addr)) ;

    serv_addr.sin_family = AF_INET;
    serv_addr.sin_addr.s_addr = htonl(INADDR_ANY);
    serv_addr.sin_port = htons(port);

    // Bind the socket to the server's ( this process ) address.
    if (bind(sockfd,(struct sockaddr *)&serv_addr,sizeof(serv_addr)) < 0) {
        fatal_error( fp, rank, 0, NULL, "bind failed %s", strerror(errno) );
    }
    
    // Listen for connections in this socket
    if ( listen(sockfd,backlog) < 0 ) {
        fatal_error( fp, rank, 0, NULL, "listen failed %s", strerror(errno) );
    }
    return sockfd;
}
#endif

int 
Accept( int sockfd, int rank, FILE *fp ) {
    socklen_t clilen;
    struct sockaddr_in cliaddr;
    clilen = sizeof(cliaddr);
    int accepted;

    while( 1 ) {
        accepted = accept( sockfd, (struct sockaddr *)&cliaddr, &clilen );
        if ( accepted < 0 ) {
            if ( errno == EINTR ) {
                continue;
            } else {
                fatal_error( fp, rank, 0, NULL, "accept: %s", strerror(errno) );
            }
        } else {
            return accepted;
        }
    }
}
