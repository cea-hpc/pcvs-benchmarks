/*-----------------------------------------------------------------------------
MESSAGE PASSING INTERFACE TEST CASE SUITE

Copyright - 1996 Intel Corporation

Intel Corporation hereby grants a non-exclusive license under Intel's
copyright to copy, modify and distribute this software for any purpose
and without fee, provided that the above copyright notice and the following
paragraphs appear on all copies.

Intel Corporation makes no representation that the test cases comprising
this suite are correct or are an accurate representation of any standard.

IN NO EVENT SHALL INTEL HAVE ANY LIABILITY FOR ANY DIRECT, INDIRECT OR
SPECULATIVE DAMAGES, (INCLUDING WITHOUT LIMITING THE FOREGOING, CONSEQUENTIAL,
INCIDENTAL AND SPECIAL DAMAGES) INCLUDING, BUT NOT LIMITED TO INFRINGEMENT,
LOSS OF USE, BUSINESS INTERRUPTIONS, AND LOSS OF PROFITS, IRRESPECTIVE OF
WHETHER INTEL HAS ADVANCE NOTICE OF THE POSSIBILITY OF ANY SUCH DAMAGES.

INTEL CORPORATION SPECIFICALLY DISCLAIMS ANY WARRANTIES INCLUDING, BUT NOT
LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY, FITNESS FOR A
PARTICULAR PURPOSE AND NON-INFRINGEMENT.  THE SOFTWARE PROVIDED HEREUNDER
IS ON AN "AS IS" BASIS AND INTEL CORPORATION HAS NO OBLIGATION TO PROVIDE
MAINTENANCE, SUPPORT, UPDATES, ENHANCEMENTS OR MODIFICATIONS.
-----------------------------------------------------------------------------*/

/*  Utility functions */

#define _MPITEST_LIB 1

#include <stdio.h>
#include <math.h>
#include "mpitest_def.h"
#include "mpitest.h"
#include <assert.h>

extern int MPITEST_message_lengths[];
extern int MPITEST_comm_sizes[];
extern int MPITEST_types[];

/* To aid in debugging MPI implementations with this test suite, these local
   ints can be set to turn on debugging prints */
static int printComm     = 0;
static int printDatatype = 0;

static void MPITEST_datatype_init(void);
static void MPITEST_datatype_init(void)
/****************************************************************************
   This routine must be called by all test codes to initialize the array, 
   since types may not be integer constants which can be initialized directly
   into the above array .

   Take care to make sure these match exactly MPITEST_* values.
****************************************************************************/
{
int          type_count,
             i,
             k,
             ierr;
int          length_array[15];
MPI_Aint     displs_array[15];
MPI_Datatype type_array[15];
derived1     drv1[2];

  MPITEST_mpi_datatypes[MPITEST_int]            = MPI_INT;
  MPITEST_mpi_datatypes[MPITEST_short_int]      = MPI_SHORT;
  MPITEST_mpi_datatypes[MPITEST_long]           = MPI_LONG;
  MPITEST_mpi_datatypes[MPITEST_unsigned_short] = MPI_UNSIGNED_SHORT;
  MPITEST_mpi_datatypes[MPITEST_unsigned]       = MPI_UNSIGNED;
  MPITEST_mpi_datatypes[MPITEST_unsigned_long]  = MPI_UNSIGNED_LONG;
  MPITEST_mpi_datatypes[MPITEST_float]          = MPI_FLOAT;
  MPITEST_mpi_datatypes[MPITEST_double]         = MPI_DOUBLE;
  MPITEST_mpi_datatypes[MPITEST_char]           = MPI_CHAR;
  MPITEST_mpi_datatypes[MPITEST_unsigned_char]  = MPI_UNSIGNED_CHAR;
#if MPITEST_longlong_def
  MPITEST_mpi_datatypes[MPITEST_longlong]       = MPI_LONG_LONG_INT;
#endif
#if MPITEST_long_double_def
  MPITEST_mpi_datatypes[MPITEST_long_double]    = MPI_LONG_DOUBLE;
#endif
  MPITEST_mpi_datatypes[MPITEST_byte]           = MPI_BYTE;

for (k=0; k<2; k++)
  {
    for (i=0; i<14; i++)
      {
	length_array[i] = 1;
      }

    ierr = MPI_Address(&drv1[0],                  &displs_array[0]);
    type_array[0] = MPI_LB;

    ierr = MPI_Address(&drv1[0].Int[k],           &displs_array[1]);
    type_array[1] = MPI_INT;

    ierr = MPI_Address(&drv1[0].ShortInt[k],      &displs_array[2]);
    type_array[2] = MPI_SHORT;

    ierr = MPI_Address(&drv1[0].Long[k],          &displs_array[3]);
    type_array[3] = MPI_LONG;

    ierr = MPI_Address(&drv1[0].UnsignedShort[k], &displs_array[4]);
    type_array[4] = MPI_UNSIGNED_SHORT;

    ierr = MPI_Address(&drv1[0].Unsigned[k],      &displs_array[5]);
    type_array[5] = MPI_UNSIGNED;

    ierr = MPI_Address(&drv1[0].UnsignedLong[k],  &displs_array[6]);
    type_array[6] = MPI_UNSIGNED_LONG;

    ierr = MPI_Address(&drv1[0].Float[k],         &displs_array[7]);
    type_array[7] = MPI_FLOAT;

    ierr = MPI_Address(&drv1[0].Char[k],          &displs_array[8]);
    type_array[8] = MPI_CHAR;

    ierr = MPI_Address(&drv1[0].Double[k],        &displs_array[9]);
    type_array[9] = MPI_DOUBLE;

    ierr = MPI_Address(&drv1[0].UnsignedChar[k],  &displs_array[10]);
    type_array[10] = MPI_UNSIGNED_CHAR;

    type_count = 11;

#if MPITEST_longlong_def
    ierr = MPI_Address(&drv1[0].LongLong[k],      &displs_array[type_count]);
    type_array[type_count] = MPI_LONG_LONG_INT;
    type_count++;
#endif

#if MPITEST_long_double_def
    ierr = MPI_Address(&drv1[0].LongDouble[k],    &displs_array[type_count]);
    type_array[type_count] = MPI_LONG_DOUBLE;
    type_count++;
#endif

    ierr = MPI_Address(&drv1[1], &displs_array[type_count]);
    type_array[type_count] = MPI_UB;
    type_count++;

    for (i=1; i<type_count; i++)
      {
        displs_array[i] = displs_array[i] - displs_array[0];
      }
    displs_array[0] = 0;

    ierr = MPI_Type_struct(type_count, length_array, displs_array, type_array,
	&MPITEST_mpi_datatypes[MPITEST_derived1+k]);
    assert(ierr == MPI_SUCCESS);
    ierr = MPI_Type_commit(&MPITEST_mpi_datatypes[MPITEST_derived1+k]);
    assert(ierr == MPI_SUCCESS);

  }

if (MPITEST_me%2 == 0)
    MPITEST_mpi_datatypes[MPITEST_derived2] =
	MPITEST_mpi_datatypes[MPITEST_derived1];

}


int MPITEST_init(int argc, char *argv[])
/***************************************************************
  Initialize the MPITEST environment.

  Arguments :
  int argc             The number of command line arguments
  char *argv[]         The command line arguments

  The Arguments should be the ones that were passed to main().

* Determines the number of ranks in the current application
  (MPITEST_nump) and the calling process' global rank (MPITEST_me).

* Gets the command-line parameters if there are any.

* Sets global variable MPITEST_verbose to its default value.
  This may be changed by MPITEST_get_parameters().

  History :

  10/13/95   gm  Created
   1/15/96   gt  Set MPI_Errhandler on MPI_COMM_WORLD to MPI_ERRORS_RETURN;
   2/16/95   st  Added MPI_datatype_init() in MPITEST_init()
***************************************************************/
{
  int error = 0;
  char info_buf[MPI_MAX_ERROR_STRING];
  int size;
  static char myvers[]="@(#)MPI V1.1 Validation Suite V1.0-01";

  sprintf(info_buf, "%s\n", myvers);
/* Set the global communicator size and this process' rank */
  error = MPI_Comm_size(MPI_COMM_WORLD, &MPITEST_nump);
  if (error != MPI_SUCCESS)
    {
      sprintf(info_buf, "Error (%d) from MPI_Comm_size() in MPITEST_init()", error);
      MPITEST_message(MPITEST_NONFATAL, info_buf); 
      MPI_Error_string(error, &info_buf[0], &size); 
      MPITEST_message(MPITEST_FATAL, info_buf);
    }

  error = MPI_Comm_rank(MPI_COMM_WORLD, &MPITEST_me);
  if (error != MPI_SUCCESS)
    {
      sprintf(info_buf, "Error (%d) from MPI_Comm_rank() in MPITEST_init()", error);
      MPITEST_message(MPITEST_NONFATAL, info_buf);
      MPI_Error_string(error, &info_buf[0], &size);
      MPITEST_message(MPITEST_FATAL, info_buf);
    }

/* the current rank, until changed via  a call to
   MPITEST_get_communicator() */
  MPITEST_current_rank = MPITEST_me;


/* The verbose flag may be changed in MPITEST_get_parameters(),
   if the command line flag '-verbose' is given.
   By default, it is 0 (FALSE) */
  MPITEST_verbose = 0;

/* The user codes need to know the value of the communicator
   type token corresponding to MPI_COMM_WORLD */
  MPITEST_world = MPITEST_comm_type_world;

/* Where should we write I/O?  Default is stdout, but could be any file,
   one would open it here */
  MPITEST_stdout = stdout;

/* Initialize user defined datatypes */
  MPITEST_datatype_init();

/* Get command line parameters */
  MPITEST_get_parameters(argc, argv);

/* Let errors be returned, since we are checking */
  error = MPI_Errhandler_set(MPI_COMM_WORLD,MPI_ERRORS_RETURN);

  return 0;
} /* MPITEST_init() */




int MPITEST_message(int level, char *error_string)
/****************************************************************
  Report a message.

  Arguments :
  int level       Specifies the circumstance under
                  which the message should be reported.
		  Choices :
		    MPITEST_FATAL: always report, abort app.
		    MPITEST_NONFATAL :always report, don't abort
		    MPITEST_VERIFY :always report, don't abort
		    MPITEST_INFO0 : always report
		    MPITEST_INFO1 : report if MPITEST_verbose != 2
		    MPITEST_INFO2 : report if MPITEST_verbose = 2
		    The above macros are defined in mpitest.h

  char *error_string  The message to be printed.

  History :

  10/13/95   gm  Created
   1/14/96   gt  Added MPITEST_VERIFY
****************************************************************/
{
  int ierr;

  switch (level)
    {
    case MPITEST_FATAL:
      fprintf(MPITEST_stdout, "MPITEST fatal error (%d): %s\n", MPITEST_me,
	      error_string);
      /* Flush in case MPI_Abort fails to flush */
      fflush( MPITEST_stdout );
      ierr = MPI_Abort(MPI_COMM_WORLD, 1);
      if (ierr != MPI_SUCCESS) {
        /* Can't abort, bailing out ! */
        exit(1);
      }
      break;
    case MPITEST_VERIFY:
      fprintf(MPITEST_stdout,"MPITEST verify (%d): %s\n", MPITEST_me,
	      error_string);
      fflush( MPITEST_stdout );
      break;
    case MPITEST_NONFATAL:
      fprintf(MPITEST_stdout,"MPITEST error (%d): %s\n", MPITEST_me,
	      error_string);
      fflush( MPITEST_stdout );
      ierr = MPI_Abort(MPI_COMM_WORLD, 1);
      if (ierr != MPI_SUCCESS) {
        /* Can't abort, bailing out ! */
        exit(1);
      }
      break;
    case MPITEST_INFO0:
	fprintf(MPITEST_stdout,"MPITEST info  (%d): %s\n", MPITEST_me,
	      error_string);
      fflush( MPITEST_stdout );
      break;
    case MPITEST_INFO1:
      if (MPITEST_verbose ) {
	fprintf(MPITEST_stdout,"MPITEST info1 (%d): %s\n", MPITEST_me,
	      error_string);
	fflush( MPITEST_stdout );
      }
      break;
    case MPITEST_INFO2:
      if (MPITEST_verbose==2 ) {
	fprintf(MPITEST_stdout,"MPITEST info2 (%d): %s\n", MPITEST_me,
	      error_string);
	fflush( MPITEST_stdout );
      }
    }
  return 0;
}



int MPITEST_report(int pass, int fail, int verify, char *testname)
/**************************************************************
  Report the final status of a test, based on the integer fail.

Arguments :
  int fail        The number of failures reported by the
                  calling process.

  char *testname  A pointer to a character buffer containing
                  the name of the test.
  History :

  10/13/95   gm  Created
   1/14/96   gt  Modified to gather results at rank 0, added pass, verify.
*************************************************************/
{
  int   fail_total,
	verify_total,
	pass_total,
	total,
	ierr,
	errsize;
  char  info_buf[256];
  char  err_msg[MPI_MAX_ERROR_STRING];

  sprintf(info_buf, "Node results: pass=%d, fail=%d, verify=%d",
	pass, fail, verify);
  MPITEST_message(MPITEST_INFO1, info_buf);
  ierr=MPI_Allreduce(&pass, &pass_total, 1, MPI_INT, MPI_SUM, MPI_COMM_WORLD);
  if (ierr != MPI_SUCCESS)
    {
      sprintf(info_buf, "Error (%d) from MPI_Allreduce() in MPITEST_report()", ierr);
      MPITEST_message(MPITEST_NONFATAL, info_buf);
      MPI_Error_string(ierr, err_msg, &errsize);
      MPITEST_message(MPITEST_FATAL, err_msg);
    }
  ierr=MPI_Allreduce(&fail, &fail_total, 1, MPI_INT, MPI_SUM, MPI_COMM_WORLD);
  if (ierr != MPI_SUCCESS)
    {
      sprintf(info_buf, "Error (%d) from MPI_Allreduce() in MPITEST_report()", ierr);
      MPITEST_message(MPITEST_NONFATAL, info_buf);
      MPI_Error_string(ierr, err_msg, &errsize);
      MPITEST_message(MPITEST_FATAL, err_msg);
    }
  ierr=MPI_Allreduce(&verify, &verify_total, 1, MPI_INT, MPI_SUM, MPI_COMM_WORLD);
  if (ierr != MPI_SUCCESS)
    {
      sprintf(info_buf, "Error (%d) from MPI_Allreduce() in MPITEST_report()", ierr);
      MPITEST_message(MPITEST_NONFATAL, info_buf);
      MPI_Error_string(ierr, err_msg, &errsize);
      MPITEST_message(MPITEST_FATAL, err_msg);
    }

  total = pass_total + fail_total + verify_total;

  if (MPITEST_me == 0) {
    if (fail_total == 0) {
      /* We do NOT include a flush for tests that passed because that
	 is itself part of the test (output must be flushed by a correct
	 MPI implementation) */
      if (verify_total == 0) {
	fprintf(MPITEST_stdout,"MPITEST_results: %s all tests PASSED (%d)\n",
		testname, total);
      } else {
	fprintf(MPITEST_stdout,"MPITEST_results: %s %d tests PASSED, %d must manually verify (of %d)\n",
		testname, pass_total, verify_total, total);
      }
    } else {
      if (verify_total == 0) {
	fprintf(MPITEST_stdout,"MPITEST_results: %s %d tests FAILED (of %d)\n",
		testname, fail_total, total);
      } else {
	fprintf(MPITEST_stdout,"MPITEST_results: %s %d tests FAILED, %d must manually verify (of %d)\n",
		testname, fail_total, verify_total, total);
      }
      fflush( MPITEST_stdout );
    }
  }

  if (MPITEST_stdout != stdout)
    {
      if (fclose(MPITEST_stdout) == EOF)
	{
	  sprintf(info_buf, "Cannot close file: stdout.%d", MPITEST_me);
	  MPITEST_stdout = stdout;
	  MPITEST_message(MPITEST_FATAL, info_buf);
	}
    }


  return 0;

}


static int parse_context_string(char *);

static int parse_context_string(char *s)
/****************************************************************
  Parse a context string from the command line.  Reset the values
in the configuration array MPITEST_comm_sizes[] as we go.  Do
error checking on input values.

Arguments :
INPUT char *s        The command-line string.  This should be the
                      next command-line argument after "-context".

Return value : 0

Algorithm :
Use the strtok() standard library function to split the argument
into substrings delimited by either commas or periods.  Notice
that spaces are not allowed, since argv[] is split up based on
spaces.

Then do token replacement in MPITEST_comm_sizes[] based
on the substrings.  If the substring does not match any of the
choices, it is assumed to be a number.  So it is converted to
an int via the standard library funtion atoi(), then checked
to make sure it is in bounds (i.e 0 <= num < MPITEST_nump).

Finally, insert the MPITEST_END_TOKEN after the last
element processed from the command line.

  History :

  10/13/95   gm  Created
****************************************************************/
{
  char toks[] =
    {
      ',' , '.', '\0'
      };
  char *subs;
  int index=0, temp;



  subs = strtok(s, toks);

  while (subs != NULL)
    {
      if (!strncmp(subs, "create", 6))
	{
	  MPITEST_comm_sizes[index++] = MPITEST_comm_type_create;
	}
      if (!strncmp(subs, "split", 5))
	{
	  MPITEST_comm_sizes[index++] = MPITEST_comm_type_split;
	}
      else if (!strncmp(subs, "last", 4))
	{
	  MPITEST_comm_sizes[index++] = MPITEST_comm_last_rank;
	}
      else if (!strncmp(subs, "dup", 3))
	{
	  MPITEST_comm_sizes[index++] = MPITEST_comm_type_dup;
	}
      else if (!strncmp(subs, "list", 4))
	{
	  MPITEST_comm_sizes[index++] = MPITEST_node_list;
	}
      else if (!strncmp(subs, "inc", 3))
	{
	  MPITEST_comm_sizes[index++] = MPITEST_comm_inc;
	}
      else if (!strncmp(subs, "worl", 4))
	{
	  MPITEST_comm_sizes[index++] = MPITEST_comm_type_world;
	}
      else if (!strncmp(subs, "self", 4))
	{
	  MPITEST_comm_sizes[index++] = MPITEST_comm_type_self;
	}
      else
	{
	  temp = atoi(subs);
	  if (temp < 0 || temp >= MPITEST_nump)
	    MPITEST_message(MPITEST_FATAL,"Illegal communicator specification on command line");
	  else
	    MPITEST_comm_sizes[index++] = temp;
	}

      subs = strtok(NULL, toks);
    }
  MPITEST_comm_sizes[ index] = MPITEST_END_TOKEN;
  return index;

}

static int parse_length_string(char *);

static int parse_length_string(char *s)
/****************************************************************
  Parse a message length string from the command line.  Reset the values
int the configuration array MPITEST_messaeg_lengths[] as we go.  Do
error checking on input values.

Arguments :
INPUT char *s        The command-line string.  This should be the
                      next command-line argument after "-length".

Return value : 0

Algorithm :
Use the strtok() standard library function to split the argument
into substrings delimited by either commas or periods.  Notice
that spaces are not allowed, since argv[] is split up based on
spaces.

Then do token replacement in MPITEST_message_lengths[] based
on the substrings.  If the substring does not match any of the
choices, it is assumed to be a number.  So it is converted to
an int via the standard library funtion atoi(), then checked
to make sure it is in bounds (i.e 0 <= num ).  Finally, the
MPITEST_END_TOKEN is inserted at the end of the
MPITEST_message_lengths[] array.

  History :

  10/13/95   gm  Created
****************************************************************/

{
  char toks[] =
    {
      ',' , '.' , '\0'
      };
  char *subs;
  int index=0, temp;

  subs = strtok(s, toks);

  while (subs != NULL)
    {
      if (!strncmp(subs, "mult", 4))
	{
	  MPITEST_message_lengths[index++] = MPITEST_MULT_INC;
	}
      else if (!strncmp(subs, "add", 3))
	{
	  MPITEST_message_lengths[index++] = MPITEST_ADD_INC;
	}
      else if (!strncmp(subs, "repeat", 5))
	{
	  MPITEST_message_lengths[index++] = MPITEST_REPEAT;
	}
      else
	{
	  temp = atoi(subs);
	  if (temp < 0 )
	    MPITEST_message(MPITEST_FATAL,"Illegal length specification on command line");
	  else
	    MPITEST_message_lengths[index++] = temp;
	}

      subs = strtok(NULL, toks);
    }
  MPITEST_message_lengths[ index] = MPITEST_END_TOKEN;
  return index;

}


static int parse_type_string(char *);
static int parse_type_string(char *s)
/****************************************************************
  Parse a datatype string from the command line.  Reset the values
int the configuration array MPITEST_types[] as we go.  Do
error checking on input values.

Arguments :
INPUT char *s        The command-line string.  This should be the
                      next command-line argument after "-type".

Return value : 0

Algorithm :
Use the strtok() standard library function to split the argument
into substrings delimited by either commas or periods.  Notice
that spaces are not allowed, since argv[] is split up based on
spaces.

Then do token replacement in MPITEST_types[] based
on the substrings.  If the substring does not match any of the
choices, it is an error.  Finally,
MPITEST_END_TOKEN is inserted at the end of the
MPITEST_types[] array.

  History :

  10/13/95   gm  Created
****************************************************************/

{
  char toks[] =
    {
      ',' , '.' , '\0'
      };
  char *subs;
  int index=0;

  subs = strtok(s, toks);

  while (subs != NULL)
    {
      if (!strncmp(subs, "int", 3))
	{
	  MPITEST_types[index++] = MPITEST_int;
	}
      else if (!strncmp(subs, "short_int", 9))
	{
	  MPITEST_types[index++] = MPITEST_short_int;
	}
      else if (!strncmp(subs, "unsigned_short", 14))
	{
	  MPITEST_types[index++] = MPITEST_unsigned_short;
	}
      else if (!strncmp(subs, "unsigned_long", 13))
	{
	  MPITEST_types[index++] = MPITEST_unsigned_long;
	}
      else if (!strncmp(subs, "unsigned_char", 14))
	{
	  MPITEST_types[index++] = MPITEST_unsigned_char;
	}
#if MPITEST_longlong_def
      else if (!strncmp(subs, "longlong", 8))
	{
	  MPITEST_types[index++] = MPITEST_longlong;
	}
#endif
      else if (!strncmp(subs, "unsigned", 8))
	{
	  MPITEST_types[index++] = MPITEST_unsigned;
	}
      else if (!strncmp(subs, "float", 5))
	{
	  MPITEST_types[index++] = MPITEST_float;
	}
      else if (!strncmp(subs, "double", 6))
	{
	  MPITEST_types[index++] = MPITEST_double;
	}
      else if (!strncmp(subs, "char", 4))
	{
	  MPITEST_types[index++] = MPITEST_char;
	}
#if MPITEST_long_double_def
      else if (!strncmp(subs, "long_double", 11))
	{
	  MPITEST_types[index++] = MPITEST_long_double;
	}
#endif
      else if (!strncmp(subs, "long", 4))
	{
	  MPITEST_types[index++] = MPITEST_long;
	}
      else if (!strncmp(subs, "byte", 4))
	{
	  MPITEST_types[index++] = MPITEST_byte;
	}
      else if (!strncmp(subs, "derived1", 8))
	{
	  MPITEST_types[index++] = MPITEST_derived1;
	}
      else if (!strncmp(subs, "derived2", 8))
	{
	  MPITEST_types[index++] = MPITEST_derived2;
	}
      else
	{
	  MPITEST_message(MPITEST_FATAL,"Unrecognized type parameter on command line");
	}
      subs = strtok(NULL, toks);
    }

  MPITEST_types[ index] = MPITEST_END_TOKEN;
  return index;

}




int MPITEST_get_parameters(int argc, char *argv[])
/*******************************************************************
  Parse the command-line options.  Reset the configuration arrays
based on any valid command-line options.

Arguments :
INPUT int argc            the number of command-line options
INPUT char *argv[]        the command-line options as strings

Return value : 0

Algorithm :
Look for known command-line options.  Take appropriate action
upon finding one.  Trigger a fatal error if an unrecognized
option is found.  Appropriate actions are :

"-help"                    call MPITEST_help_message()
"-verbose n"               set the MPITEST_verbose to n (0 <= n <= 2)
"-length length_string"    call parse_length_string(length_string);
"-context context_string"  call parse_context_string(context_string);
"-stdoutdir directory"
"-type type_string"        call parse_type_string(type_string);

  History :

  10/13/95   gm  Created
*******************************************************************/
{
  int
    arg_index,
    step=0,   /* To keep compiler happy */
    temp;

  char
       info_buf[MPI_MAX_ERROR_STRING],
       filename[MPITEST_FILENAME_MAX],
      *dir;

  for (arg_index=1; arg_index<argc; arg_index+=step)
    {
      if (!strcmp(argv[ arg_index], "-length"))
	{
	  parse_length_string(argv[ arg_index+1]);
	  step = 2;
	}
      else if (!strcmp(argv[ arg_index], "-context"))
	{
	  parse_context_string(argv[ arg_index+1]);
	  step = 2;
	}
      else if (!strcmp(argv[ arg_index], "-type"))
	{
	  parse_type_string(argv[ arg_index+1]);
	  step = 2;
	}
      else if (!strcmp(argv[arg_index], "-help"))
	{
	  MPITEST_help_message();
	  step = 1;
	}
      /* the verbose operation argument */
      else if (!strcmp(argv[arg_index], "-verbose"))
	{
	  temp = atoi(argv[ arg_index+1]);
	  if (temp==0 || temp==1 || temp==2)
	    {
	    MPITEST_verbose = temp;
	    step = 2;
	    }
	  else
	    MPITEST_message(MPITEST_FATAL,"Illegal verbosity argument on command line");
	}
      else if (!strcmp(argv[arg_index], "-stdoutdir"))
	{
	  if (arg_index+1 < argc)
	    {
	      if ((dir = argv[arg_index+1]) == NULL)
		{
		  MPITEST_message(MPITEST_FATAL, "invalid NULL directory argument for -stdoutdir\n");
		}
	      else
		{
		  sprintf(filename, "%s/stdout.%d", dir, MPITEST_me);

		  if ((MPITEST_stdout = fopen(filename, "w")) == NULL)
		    {
		      MPITEST_stdout = stdout;
		      sprintf(info_buf, "Cannot open file %d", MPITEST_me);
		      MPITEST_message(MPITEST_FATAL, info_buf);
		    }
		}
	    }
          else
            {
              MPITEST_message(MPITEST_FATAL, "directory argument required for -stdoutdir\n");
            }

          step = 2;
        }
    else
/*      {
          MPITEST_message(MPITEST_FATAL,"Unrecognized command line parameter");
        }
*/
      break;  /* Pass all unrecognised options to node executable */
    }
  return 0;
}



int MPITEST_help_message(void)
/*****************************************************************************
  Generate the MPITEST help message.

  Arguments : none

  Return value : none

  Prints out a help message detailing the command line parameters,
  then calls exit(1).  If one rank is calling this, then all are,
  so there is no need for a call to MPI_Abort().

  History :

  10/13/95   gm  Created


*****************************************************************************/
{
  if (MPITEST_me==0)
    {
      fprintf(MPITEST_stdout,"Usage:\t testname [-help] [-verbose n] [-type type_string]\n");
      fprintf(MPITEST_stdout,"\t\t [-context context_string] [-length length_string] \n");
      fprintf(MPITEST_stdout,"\t\t [-stdoutdir dirname] \n");
      fprintf(MPITEST_stdout,"\nOptions (order is not important) :\n");
      fprintf(MPITEST_stdout,"\t-help\t\t\tgenerate this message\n\n");
      fprintf(MPITEST_stdout,"\t-verbose n\t\tSet verbosity flag to n, 0 <= n <= 2\n\n");
      fprintf(MPITEST_stdout,"\t-length length_string\tuse message length(s) specified by length_string\n");
      fprintf(MPITEST_stdout,"\t\t\t\tlength_string = length_token[,length_token,...]\n");
      fprintf(MPITEST_stdout,"\t\t\t\tlength_token = length (0<=length) or\n");
      fprintf(MPITEST_stdout,"\t\t\t\t  \"mult,start,end,inc\" (0<start<=end,1<inc) or\n");
      fprintf(MPITEST_stdout,"\t\t\t\t  \"add,start,end,inc\" (0<=start<=end,1<=inc) or\n");
      fprintf(MPITEST_stdout,"\t\t\t\t  \"repeat,val,repitions\" (0<=val, 0<repititions)\n\n");
      fprintf(MPITEST_stdout,"\t-context context_string\tuse context(s) specified by context_string\n");
      fprintf(MPITEST_stdout,"\t\t\t\tcontext_string=context_token[,context_token,...]\n");
      fprintf(MPITEST_stdout,"\t\t\t\tcontext_token = context_type,size_token\n");
      fprintf(MPITEST_stdout,"\t\t\t\tcontext_type = \"world\" (no size token to follow)\n");
      fprintf(MPITEST_stdout,"\t\t\t\t  or \"self\" (no size token to follow)\n");
      fprintf(MPITEST_stdout,"\t\t\t\t  or \"split\" or \"duplicate\"\n");
      fprintf(MPITEST_stdout,"\t\t\t\tsize_token = size (1 <= size <= number of proc.)\n");
      fprintf(MPITEST_stdout,"\t\t\t\t  or \"inc,start_node,end_node,inc_amount\"\n");
      fprintf(MPITEST_stdout,"\t\t\t\t  (1<=start_node<=end_node<=nump,1<=inc_amount)\n");
      fprintf(MPITEST_stdout,"\t\t\t\t  or \"list,num_nodes,node1,node2,...,nodelast\"\n");
      fprintf(MPITEST_stdout,"\t\t\t\t  (0<=node1,node2,...,nodelast<=nump,1<=numnodes\n");
      fprintf(MPITEST_stdout,"\t-type type_string\tuse type(s) specified by type string\n");
      fprintf(MPITEST_stdout,"\t\t\t\ttype_string = buffer_type[,buffer_type,...]\n");
      fprintf(MPITEST_stdout,"\t\t\t\tbuffer_type = \n");
      fprintf(MPITEST_stdout,"\t\t\t\t\"int\", \"short_int\", \"long\", \"unsigned_short\",\n");
      fprintf(MPITEST_stdout,"\t\t\t\t\"unsigned\", \"unsigned_long\", \"float\", \"double\",\n");
      fprintf(MPITEST_stdout,"\t\t\t\t\"char\", \"unsigned_char\", \"longlong\",\n");
      fprintf(MPITEST_stdout,"\t\t\t\t\"long_double\", \"byte\", \"derived[12]\"\n");
      /* No flush should be required by a correct MPI implementation */
    }
  MPI_Finalize();
  exit(1);

}




int MPITEST_init_buffer(int buffer_type, int length,
			struct dataTemplate value, void *buffer)
/***********************************************************************
  Set the specified buffer of the specified type and the specified
  length to the specified value.

  Arguments :
  buffer_type         INPUT, integer specifying the type of the
                       buffer.  Legal values are defined in
		       include/mpitest_def.h .

  length              INPUT, integer length of the buffer.

  value               INPUT, value to be put into buffer.  This is
                       declared to be of type dataTemplate.  The
		       member of value corresponding to type buffer_type
		       needs to have been assigned a meaningful value
		       for this function to have meaningful results.

  buffer              OUTPUT, the buffer to be set.  Passed in as a void
                       pointer so that different types may all be
		       encompassed in one call.

  This function uses a switch statement to differentiate between the
  different allowed types.  The type casts are required so that the void
  pointers may be dereferenced.

  History :

  10/13/95   gm  Created
***********************************************************************/
  {
    int i,k;

    if (buffer_type == MPITEST_derived2)
	k = MPITEST_me%2;
    else
	k = 0;

    for (i=0; i< length; i++)
      {
	switch (buffer_type)
	  {
	  case MPITEST_int:
	    ((int *)buffer)[i] = value.Int;
	    break;
	  case MPITEST_short_int:
	    ((short int *)buffer)[i] = value.ShortInt;
	    break;
	  case MPITEST_long:
	    ((long *)buffer)[i] = value.Long;
	    break;
	  case MPITEST_unsigned_short:
	    ((unsigned short *)buffer)[i] = value.UnsignedShort;
	    break;
	  case MPITEST_unsigned:
	    ((unsigned *)buffer)[i] = value.Unsigned;
	    break;
	  case MPITEST_unsigned_long:
	    ((unsigned long *)buffer)[i] = value.UnsignedLong;
	    break;
	  case MPITEST_float:
	    ((float *)buffer)[i] = value.Float;
	    break;
	  case MPITEST_double:
	    ((double *)buffer)[i] = value.Double;
	    break;
	  case MPITEST_char:
	    ((signed char *)buffer)[i] = (signed char)value.Char;
	    break;
	  case MPITEST_unsigned_char:
	    ((unsigned char *)buffer)[i] = value.UnsignedChar;
	    break;
#if MPITEST_longlong_def
	  case MPITEST_longlong:
	    ((long long int *)buffer)[i] = value.LongLong;
	    break;
#endif
#if MPITEST_long_double_def
	  case MPITEST_long_double:
	    ((long double *)buffer)[i] = value.LongDouble;
	    break;
#endif
	  case MPITEST_byte:
	    ((MPITEST_byte_def *)buffer)[i] = value.Byte;
	    break;
	  case MPITEST_derived1:
	  case MPITEST_derived2:
	    ((derived1 *)buffer)[i].Int[k] = value.Int;
	    ((derived1 *)buffer)[i].ShortInt[k] = value.ShortInt;
	    ((derived1 *)buffer)[i].Long[k] = value.Long;
	    ((derived1 *)buffer)[i].UnsignedShort[k] = value.UnsignedShort;
	    ((derived1 *)buffer)[i].Unsigned[k] = value.Unsigned;
	    ((derived1 *)buffer)[i].UnsignedLong[k] = value.UnsignedLong;
	    ((derived1 *)buffer)[i].Float[k] = value.Float;
	    ((derived1 *)buffer)[i].Double[k] = value.Double;
	    ((derived1 *)buffer)[i].Char[k] = value.Char;
	    ((derived1 *)buffer)[i].UnsignedChar[k] = value.UnsignedChar;
#if MPITEST_longlong_def
	    ((derived1 *)buffer)[i].LongLong[k] = value.LongLong;
#endif
#if MPITEST_long_double_def
	    ((derived1 *)buffer)[i].LongDouble[k] = value.LongDouble;
#endif
	    break;
	  }
      }
    return 0;
  }


int MPITEST_init_buffer_inc(int buffer_type, int length,
			struct dataTemplate value, void *buffer)
/***********************************************************************
  Set the specified buffer of the specified type and the specified
  length to the specified value, incrementing by one per location.

  Arguments :
  buffer_type         INPUT, integer specifying the type of the
                       buffer.  Legal values are defined in
		       include/mpitest_def.h .

  length              INPUT, integer length of the buffer.

  value               INPUT, value to be put into buffer.  This is
                       declared to be of type dataTemplate.  The
		       member of value corresponding to type buffer_type
		       needs to have been assigned a meaningful value
		       for this function to have meaningful results.

  buffer              OUTPUT, the buffer to be set.  Passed in as a void
                       pointer so that different types may all be
		       encompassed in one call.

  This function uses a switch statement to differentiate between the
  different allowed types.  The type casts are required so that the void
  pointers may be dereferenced.

  History :

  10/13/95   gm  Created
***********************************************************************/
  {
    int i,k;

    if (buffer_type == MPITEST_derived2)
	k = MPITEST_me%2;
    else
	k = 0;

    for (i=0; i< length; i++)
      {
	switch (buffer_type)
	  {
	  case MPITEST_int:
	    ((int *)buffer)[i] = (int)(value.Int+i);
	    break;
	  case MPITEST_short_int:
	    ((short int *)buffer)[i] = (short int)(value.ShortInt+(short int)i);
	    break;
	  case MPITEST_long:
	    ((long *)buffer)[i] = (long)(value.Long+(long)i);
	    break;
	  case MPITEST_unsigned_short:
	    ((unsigned short *)buffer)[i] =
		(unsigned short)(value.UnsignedShort+(unsigned short)i);
	    break;
	  case MPITEST_unsigned:
	    ((unsigned *)buffer)[i] = (unsigned)(value.Unsigned+(unsigned)i);
	    break;
	  case MPITEST_unsigned_long:
	    ((unsigned long *)buffer)[i] =
		(unsigned long)(value.UnsignedLong+(unsigned long)i);
	    break;
	  case MPITEST_float:
	    ((float *)buffer)[i] = (float)(value.Float+(float)i);
	    break;
	  case MPITEST_double:
	    ((double *)buffer)[i] = (double)(value.Double+(double)i);
	    break;
	  case MPITEST_char:
	    ((signed char *)buffer)[i] = 
		(signed char)((signed char)value.Char+(signed char)i);
	    break;
	  case MPITEST_unsigned_char:
	    ((unsigned char *)buffer)[i] =
		(unsigned char)(value.UnsignedChar+(unsigned char)i);
	    break;
#if MPITEST_longlong_def
	  case MPITEST_longlong:
	    ((long long int *)buffer)[i] =
		(long long int)(value.LongLong+(long long int)i);
	    break;
#endif
#if MPITEST_long_double_def
	  case MPITEST_long_double:
	    ((long double *)buffer)[i] =
		(long double)(value.LongDouble+(long double)i);
	    break;
#endif
	  case MPITEST_byte:
	    ((MPITEST_byte_def *)buffer)[i] =
		(MPITEST_byte_def)(value.Byte+(MPITEST_byte_def)i);
	    break;
	  case MPITEST_derived1:
	  case MPITEST_derived2:
	    ((derived1 *)buffer)[i].Int[k] = (int)value.Int+i;
	    ((derived1 *)buffer)[i].ShortInt[k] = 
		(short int)(value.ShortInt+(short int)i);
	    ((derived1 *)buffer)[i].Long[k] =
		(long)(value.Long+(long)i);
	    ((derived1 *)buffer)[i].UnsignedShort[k] =
		(unsigned short)(value.UnsignedShort+(unsigned short)i);
	    ((derived1 *)buffer)[i].Unsigned[k] =
		 (unsigned)(value.Unsigned+(unsigned)i);
	    ((derived1 *)buffer)[i].UnsignedLong[k] =
		(unsigned long)(value.UnsignedLong+(unsigned long)i);
	    ((derived1 *)buffer)[i].Float[k] =
		(float)(value.Float+(float)i);
	    ((derived1 *)buffer)[i].Double[k] =
		(double)(value.Double+(double)i);
	    ((derived1 *)buffer)[i].Char[k] =
		(signed char)(value.Char+(signed char)i);
	    ((derived1 *)buffer)[i].UnsignedChar[k] =
		(unsigned char)(value.UnsignedChar+(unsigned char)i);
#if MPITEST_longlong_def
	    ((derived1 *)buffer)[i].LongLong[k] =
		(long long int)(value.LongLong+(long long int)i);
#endif
#if MPITEST_long_double_def
	    ((derived1 *)buffer)[i].LongDouble[k] =
		 (long double)(value.LongDouble+(long double)i);
#endif
	  }
      }
    return 0;
  }


int MPITEST_init_buffer_v(int buffer_type, int numblocks, int *counts,
			  int *displs, struct dataTemplate *values, void *buffer)
/***********************************************************************
  Sets the value of the memory pointed to by buffer.  The type of the
  data is specified by the buffer_type parameter.  There are numblocks
  different chunks of data, the ith chunk has length counts[i], is located
  at buffer+(displs[i]*extent(data type)), and gets set to values[i].

  Arguments :
  buffer_type         INPUT, integer specifying the type of the
                       buffer.  Legal values are defined in
		       include/mpitest_def.h .

  numblocks           INPUT, integer number of blocks of data to be set

  counts              INPUT, pointer to integer array containing the
                       lengths of blocks. counts[i] is the length of the
		       ith block

  displs              INPUT, pointer to integer array containing the
                       displacements from buffer of the blocks.  displs[i]
		       is the displacemnt of the ith block.

  values              INPUT, pointer to array of values to be put into
                       buffer.  This array is of type dataTemplate and of
		       length numblocks.  The member of values[i] corresponding
		       to buffer_type must have been set to something meaningful
		       for this routine to have meaningful results.

  buffer              OUTPUT, the buffer to be set.  Passed in as a void
                       pointer so that different types may all be
		       encompassed in one call.

  This function uses a switch statement to differentiate between the
  different allowed types.  The type casts are required so that the void
  pointers may be dereferenced.

  History :

  10/13/95   gm  Created
***********************************************************************/

  {
    int i, j, k;

    if (buffer_type == MPITEST_derived2)
	k = MPITEST_me%2;
    else
	k = 0;

    for (i=0; i< numblocks; i++)
      {
	for (j=displs[i];j<displs[i]+counts[i]; j++)
	  {
	    switch (buffer_type)
	      {
	      case MPITEST_int:
		((int *)buffer)[j] = values[i].Int;
		break;
	      case MPITEST_short_int:
		((short int *)buffer)[j] = values[i].ShortInt;
		break;
	      case MPITEST_long:
		((long *)buffer)[j] = values[i].Long;
		break;
	      case MPITEST_unsigned_short:
		((unsigned short *)buffer)[j] = values[i].UnsignedShort;
		break;
	      case MPITEST_unsigned:
		((unsigned *)buffer)[j] = values[i].Unsigned;
		break;
	      case MPITEST_unsigned_long:
		((unsigned long *)buffer)[j] = values[i].UnsignedLong;
		break;
	      case MPITEST_float:
		((float *)buffer)[j] = values[i].Float;
		break;
	      case MPITEST_double:
		((double *)buffer)[j] = values[i].Double;
		break;
	      case MPITEST_char:
		((signed char *)buffer)[j] = values[i].Char;
		break;
	      case MPITEST_unsigned_char:
		((unsigned char *)buffer)[j] = values[i].UnsignedChar;
		break;
#if MPITEST_longlong_def
	      case MPITEST_longlong:
		((long long int *)buffer)[j] = values[i].LongLong;
		break;
#endif
#if MPITEST_long_double_def
	      case MPITEST_long_double:
		((long double *)buffer)[j] = values[i].LongDouble;
		break;
#endif
	      case MPITEST_byte:
		((MPITEST_byte_def *)buffer)[j] = values[i].Byte;
		break;
	      case MPITEST_derived1:
	      case MPITEST_derived2:
		((derived1 *)buffer)[j].Int[k] = values[i].Int;
		((derived1 *)buffer)[j].ShortInt[k] = values[i].ShortInt;
		((derived1 *)buffer)[j].Long[k] = values[i].Long;
		((derived1 *)buffer)[j].UnsignedShort[k] =
			values[i].UnsignedShort;
		((derived1 *)buffer)[j].Unsigned[k] = values[i].Unsigned;
		((derived1 *)buffer)[j].UnsignedLong[k]
			= values[i].UnsignedLong;
		((derived1 *)buffer)[j].Float[k] = values[i].Float;
		((derived1 *)buffer)[j].Double[k] = values[i].Double;
		((derived1 *)buffer)[j].Char[k] = values[i].Char;
		((derived1 *)buffer)[j].UnsignedChar[k]
			= values[i].UnsignedChar;
#if MPITEST_longlong_def
		((derived1 *)buffer)[j].LongLong[k] = values[i].LongLong;
#endif
#if MPITEST_long_double_def
		((derived1 *)buffer)[j].LongDouble[k] = values[i].LongDouble;
#endif
		break;

	      }
	  }
      }

    return 0;
  }


int MPITEST_buffer_errors_inc(int buffer_type, int length,
			  struct dataTemplate value, void *buffer)
/***********************************************************************
  Checks that the specified buffer of the specified type and the specified
  length is set to the specified value.

  Arguments :
  buffer_type         INPUT, integer specifying the type of the
                       buffer.  Legal values are specified in the file
		       include/mpitest_def.h .

  length              INPUT, integer length of the buffer.

  value               INPUT, "correct" value that should be in buffer.
                       This argument is declared as struct dataTemplate
		       so that all types may be accomodated.

  buffer              OUTPUT, the buffer to be checked.  Passed in as a void
                       pointer so that different types may all be
		       encompassed in one call.

  This function uses a switch statement to differentiate between the
  different allowed types.  The type casts are required so that the void
  pointers may be dereferenced.

  History :

  10/13/95   gm  Created
***********************************************************************/

  {
    int   i, error;
    char  info_buf[MPI_MAX_ERROR_STRING];
    int   k;

    if (buffer_type == MPITEST_derived2)
	k = MPITEST_me%2;
    else
	k = 0;
    error = 0;

    for (i=0; i< length; i++)
      {
	switch (buffer_type)
	  {
	  case MPITEST_int:
	    if (((int *)buffer)[i] != (int)(value.Int+(int)i) )
	      {
		error++;
		if (error == 1)
		  {
		    sprintf(info_buf, "i=%d, int value=%d, expected %d",
			i, ((int *)buffer)[i], (int)(value.Int+(int)i));
		    MPITEST_message(MPITEST_NONFATAL, info_buf);
		  }
	      }
	    break;

	  case MPITEST_short_int:
	      /* The test also assumes that the compiler properly handles
		 overflow, i.e., as i >= 128, it wraps to -128.  Unfortunately,
		 some compilers do not do this (e.g., pgcc), and the pgcc
		 man page claims that ANSI C leaves the behavior in this
		 case undefined. */
	  {
	  int ii;
	  ii = 	value.ShortInt + i;
	  while (ii >= 32768) ii -= 65536;
	      
	  if (((short int *)buffer)[i] != ii )
	      {
		error++;
		if (error == 1)
		  {
		    sprintf(info_buf, "i=%d, (incr) short int value=%d, expected %d",
			    i, ((short int *)buffer)[i],ii);
		    MPITEST_message(MPITEST_NONFATAL, info_buf);
		  }
	      }
	  }
	    break;

	  case MPITEST_long:
	    if (((long *)buffer)[i] != (long)(value.Long+(long)i) )
	      {
		error++;
		if (error == 1)
		  {
		    sprintf(info_buf, "i=%d, long value=%ld, expected %ld",
			i, ((long *)buffer)[i], (long)(value.Long+(long)i));
		    MPITEST_message(MPITEST_NONFATAL, info_buf);
		  }
	      }
	    break;

	  case MPITEST_unsigned_short:
	    if (((unsigned short *)buffer)[i] !=
		(unsigned short)(value.UnsignedShort+(unsigned short)i) )
	      {
		error++;
		if (error == 1)
		  {
		    sprintf(info_buf, "i=%d, unsigned short value=%u, expected %u",
			i, ((unsigned short *)buffer)[i],
			(unsigned short)(value.UnsignedShort+(unsigned short)i));
		    MPITEST_message(MPITEST_NONFATAL, info_buf);
		  }
	      }
	    break;

	  case MPITEST_unsigned:
	    if (((unsigned *)buffer)[i] !=
		(unsigned)(value.Unsigned+(unsigned)i) )
	      {
		error++;
		if (error == 1)
		  {
		    sprintf(info_buf, "i=%d, unsigned value=%u, expected %u",
			i, ((unsigned *)buffer)[i],
			(unsigned)(value.Unsigned+(unsigned)i));
		    MPITEST_message(MPITEST_NONFATAL, info_buf);
		  }
	      }
	    break;

	  case MPITEST_unsigned_long:
	    if (((unsigned long *)buffer)[i] !=
		(unsigned long)(value.UnsignedLong+(unsigned long)i) )
	      {
		error++;
		if (error == 1)
		  {
		    sprintf(info_buf, "i=%d, unsigned long value=%lu, expected %lu",
			i, ((unsigned long *)buffer)[i],
			(unsigned long)(value.UnsignedLong+(unsigned long)i));
		    MPITEST_message(MPITEST_NONFATAL, info_buf);
		  }
	      }
	    break;

	  case MPITEST_float:
	    if (((float *)buffer)[i] != (float)(value.Float+(float)i) )
	      {
		error++;
		if (error == 1)
		  {
		    sprintf(info_buf, "i=%d, float value=%15.10f, expected %15.10f",
			i, ((float *)buffer)[i], (float)(value.Float+(float)i));
		    MPITEST_message(MPITEST_NONFATAL, info_buf);
		  }
	      }
	    break;

	  case MPITEST_double:
	    if (((double *)buffer)[i] != (double)(value.Double+(double)i) )
	      {
		error++;
		if (error == 1)
		  {
		    sprintf(info_buf, "i=%d, double value=%15.10f, expected %15.10f",
			i, ((double *)buffer)[i],
			(double)(value.Double+(double)i));
		    MPITEST_message(MPITEST_NONFATAL, info_buf);
		  }
	      }
	    break;

	  case MPITEST_char:
	      /* The test assumes that all chars are signed.  Some
		 C compilers use unsigned as the default.  We must
		 be careful here because of the use of arithmetic on the 
		 data. */
	      /* The test also assumes that the compiler properly handles
		 overflow, i.e., as i > 128, it wraps to -127.  Unfortunately,
		 some compilers do not do this (e.g., pgcc), and the pgcc
		 man page claims that ANSI C leaves the behavior in this
		 case undefined. */
	  {
	  int ii;
	  ii = 	value.Char + i;
	  while (ii >= 128) ii -= 256;
	  if ((signed char)(((signed char *)buffer)[i]) != (signed char)ii)
	      /*		(signed char)((signed char)(value.Char)+(signed char)i) ) */
	      {
		error++;
		if (error == 3)
		  {
		    sprintf(info_buf, "i=%d, (incr) char value=%d, expected %d",
			i, (signed char)((signed char *)buffer)[i], ii);
		    MPITEST_message(MPITEST_NONFATAL, info_buf);
		  }
	      }
	  }
	    break;

	  case MPITEST_unsigned_char:
	    if (((unsigned char *)buffer)[i] !=
		(unsigned char)(value.UnsignedChar+(unsigned char)i) )
	      {
		error++;
		if (error == 1)
		  {
		    sprintf(info_buf, "i=%d, unsigned char value=%u, expected %u",
			i, ((unsigned char *)buffer)[i],
			(unsigned char)(value.UnsignedChar+(unsigned char)i));
		    MPITEST_message(MPITEST_NONFATAL, info_buf);
		  }
	      }
	    break;

#if MPITEST_longlong_def
	  case MPITEST_longlong:
	    if (((long long int *)buffer)[i] !=
		(long long int)(value.LongLong+(long long int)i) )
	      {
		error++;
		if (error == 1)
		  {
		    sprintf(info_buf, "i=%d, longlong int value=%lld, expected %lld",
			i, ((long long int *)buffer)[i],
			(long long int)value.LongLong+(long long int)i);
		    MPITEST_message(MPITEST_NONFATAL, info_buf);
		  }
	      }
	    break;
#endif

#if MPITEST_long_double_def
	  case MPITEST_long_double:
	    if (((long double *)buffer)[i] !=
		(long double)(value.LongDouble+(long double)i) )
	      {
		error++;
		if (error == 1)
		  {
		    /* FIXME: Should be Lf where available */
		    sprintf(info_buf, "i=%d, long double value=%15.10Lf, expected %15.10Lf",
			i, ((long double *)buffer)[i],
			(long double)(value.LongDouble+(long double)i));
		    MPITEST_message(MPITEST_NONFATAL, info_buf);
		  }
	      }
	    break;
#endif

	  case MPITEST_byte:
	    if (((MPITEST_byte_def *)buffer)[i] !=
		(MPITEST_byte_def)(value.Byte+(MPITEST_byte_def)i) )
	      {
		error++;
		if (error == 1)
		  {
		    sprintf(info_buf, "i=%d, byte value=%u, expected %u",
			i, ((MPITEST_byte_def *)buffer)[i],
			(MPITEST_byte_def)(value.Byte+(MPITEST_byte_def)i));
		    MPITEST_message(MPITEST_NONFATAL, info_buf);
		  }
	      }
	    break;

	  case MPITEST_derived1:
	  case MPITEST_derived2:
	    if (((derived1 *)buffer)[i].Int[k] != (int)(value.Int+(int)i) )
	      {
		error++;
		if (error == 1)
		  {
		    sprintf(info_buf, "i=%d, int value=%d, expected %d",
			i, ((derived1 *)buffer)[i].Int[k],
			(int)(value.Int+(int)i));
		    MPITEST_message(MPITEST_NONFATAL, info_buf);
		  }
	      }
	    if (((derived1 *)buffer)[i].ShortInt[k] !=
		(short int)(value.ShortInt+(short int)i) )
	      {
		error++;
		if (error == 1)
		  {
		    sprintf(info_buf, "i=%d, (derived incr) short int value=%d, expected %d",
			i, ((derived1 *)buffer)[i].ShortInt[k],
			(short int)(value.ShortInt+(short int)i));
		    MPITEST_message(MPITEST_NONFATAL, info_buf);
		  }
	      }
	    if (((derived1 *)buffer)[i].Long[k] != (long)(value.Long+(long)i) )
	      {
		error++;
		if (error == 1)
		  {
		    sprintf(info_buf, "i=%d, long value=%ld, expected %ld",
			i, ((derived1 *)buffer)[i].Long[k],
			(long)(value.Long+(long)i));
		    MPITEST_message(MPITEST_NONFATAL, info_buf);
		  }
	      }
	    if (((derived1 *)buffer)[i].UnsignedShort[k] !=
		(unsigned short)(value.UnsignedShort+(unsigned short)i) )
	      {
		error++;
		if (error == 1)
		  {
		    sprintf(info_buf, "i=%d, unsigned short value=%u, expected %u",
			i, ((derived1 *)buffer)[i].UnsignedShort[k],
			(unsigned short)(value.UnsignedShort+(unsigned short)i))
;
		    MPITEST_message(MPITEST_NONFATAL, info_buf);
		  }
	      }
	    if (((derived1 *)buffer)[i].Unsigned[k] !=
		(unsigned)(value.Unsigned+(unsigned)i) )
	      {
		error++;
		if (error == 1)
		  {
		    sprintf(info_buf, "i=%d, unsigned value=%u, expected %u",
			i, ((derived1 *)buffer)[i].Unsigned[k],
			(unsigned)(value.Unsigned+(unsigned)i));
		    MPITEST_message(MPITEST_NONFATAL, info_buf);
		  }
	      }
	    if (((derived1 *)buffer)[i].UnsignedLong[k] !=
		(unsigned long)(value.UnsignedLong+(unsigned long)i) )
	      {
		error++;
		if (error == 1)
		  {
		    sprintf(info_buf, "i=%d, unsigned long value=%lu, expected %lu",
			i, ((derived1 *)buffer)[i].UnsignedLong[k],
			(unsigned long)(value.UnsignedLong+(unsigned long)i));
		    MPITEST_message(MPITEST_NONFATAL, info_buf);
		  }
	      }
	    if (((derived1 *)buffer)[i].Float[k] !=
		(float)(value.Float+(float)i) )
	      {
		error++;
		if (error == 1)
		  {
		    sprintf(info_buf, "i=%d, float value=%15.10f, expected %15.10f",
			i, ((derived1 *)buffer)[i].Float[k],
			(float)(value.Float+(float)i));
		    MPITEST_message(MPITEST_NONFATAL, info_buf);
		  }
	      }
	    if (((derived1 *)buffer)[i].Double[k] !=
			(double)(value.Double+(double)i) )
	      {
		error++;
		if (error == 1)
		  {
		    sprintf(info_buf, "i=%d, double value=%15.10f, expected %15.10f",
			i, ((derived1 *)buffer)[i].Double[k],
			(double)(value.Double+(double)i));
		    MPITEST_message(MPITEST_NONFATAL, info_buf);
		  }
	      }
	      /* The test also assumes that the compiler properly handles
		 overflow, i.e., as i > 128, it wraps to -127.  Unfortunately,
		 some compilers do not do this (e.g., pgcc), and the pgcc
		 man page claims that ANSI C leaves the behavior in this
		 case undefined. */
	  {
	  int ii;
	  ii = 	value.Char + i;
	  while (ii >= 128) ii -= 256;
	  if (((derived1 *)buffer)[i].Char[k] != ii)
	      {
		error++;
		if (error == 1)
		  {
		    sprintf(info_buf, "i=%d, (derived) char value=%d, expected %d",
			    i, ((derived1 *)buffer)[i].Char[k], ii );
		    MPITEST_message(MPITEST_NONFATAL, info_buf);
		  }
	      }
	  }
	    if (((derived1 *)buffer)[i].UnsignedChar[k] !=
		(unsigned char)(value.UnsignedChar+(unsigned char)i) )
	      {
		error++;
		if (error == 1)
		  {
		    sprintf(info_buf, "i=%d, (derived) unsigned char value=%u, expected %u",
			i, ((derived1 *)buffer)[i].UnsignedChar[k],
			(unsigned char)(value.UnsignedChar+(unsigned char)i));
		    MPITEST_message(MPITEST_NONFATAL, info_buf);
		  }
	      }
#if MPITEST_longlong_def
	    if (((derived1 *)buffer)[i].LongLong[k] !=
		(long long int)(value.LongLong+(long long int)i) )
	      {
		error++;
		if (error == 1)
		  {
		    sprintf(info_buf, "i=%d, longlong int value=%lld, expected %lld",
			i, ((derived1 *)buffer)[i].LongLong[k],
			(long long int)value.LongLong+(long long int)i);
		    MPITEST_message(MPITEST_NONFATAL, info_buf);
		  }
	      }
#endif

#if MPITEST_long_double_def
	    if (((derived1 *)buffer)[i].LongDouble[k] !=
		(long double)(value.LongDouble+(long double)i) )
	      {
		error++;
		if (error == 1)
		  {
		    /* FIXME: Should use Lf where available */
		    sprintf(info_buf, "i=%d, long double value=%15.10Lf, expected %15.10Lf",
			i, ((derived1 *)buffer)[i].LongDouble[k],
			(long double)(value.LongDouble+(long double)i));
		    MPITEST_message(MPITEST_NONFATAL, info_buf);
		  }
	      }
#endif



	  }

      }
    return error;
  }

int MPITEST_buffer_errors(int buffer_type, int length,
			  struct dataTemplate value, void *buffer)
/***********************************************************************
  Checks that the specified buffer of the specified type and the specified
  length is set to the specified value.

  Arguments :
  buffer_type         INPUT, integer specifying the type of the
                       buffer.  Legal values are specified in the file
		       include/mpitest_def.h .

  length              INPUT, integer length of the buffer.

  value               INPUT, "correct" value that should be in buffer.
                       This argument is declared as struct dataTemplate
		       so that all types may be accomodated.

  buffer              OUTPUT, the buffer to be checked.  Passed in as a void
                       pointer so that different types may all be
		       encompassed in one call.

  This function uses a switch statement to differentiate between the
  different allowed types.  The type casts are required so that the void
  pointers may be dereferenced.

  History :

  10/13/95   gm  Created
***********************************************************************/

  {
    int i, error, k;
    char   info_buf[MPI_MAX_ERROR_STRING];

    if (buffer_type == MPITEST_derived2)
	k = MPITEST_me%2;
    else
	k = 0;
    error = 0;

    for (i=0; i< length; i++)
      {
	switch (buffer_type)
	  {
	  case MPITEST_int:
	    if (((int *)buffer)[i] != value.Int )
	      {
		error++;
		if (error == 1)
		  {
		    sprintf(info_buf, "i=%d, int value=%d, expected %d",
			i, ((int *)buffer)[i], value.Int);
		    MPITEST_message(MPITEST_NONFATAL, info_buf);
		  }
	      }
	    break;

	  case MPITEST_short_int:
	    if (((short int *)buffer)[i] != value.ShortInt )
	      {
		error++;
		if (error == 1)
		  {
		    sprintf(info_buf, "i=%d, short int value=%d, expected %d",
			i, ((short int *)buffer)[i], value.ShortInt);
		    MPITEST_message(MPITEST_NONFATAL, info_buf);
		  }
	      }
	    break;

	  case MPITEST_long:
	    if (((long *)buffer)[i] != value.Long )
	      {
		error++;
		if (error == 1)
		  {
		    sprintf(info_buf, "i=%d, long value=%ld, expected %ld",
			i, ((long *)buffer)[i], value.Long);
		    MPITEST_message(MPITEST_NONFATAL, info_buf);
		  }
	      }
	    break;

	  case MPITEST_unsigned_short:
	    if (((unsigned short *)buffer)[i] != value.UnsignedShort )
	      {
		error++;
		if (error == 1)
		  {
		    sprintf(info_buf, "i=%d, unsigned short value=%u, expected %u",
			i, ((unsigned short *)buffer)[i], value.UnsignedShort);
		    MPITEST_message(MPITEST_NONFATAL, info_buf);
		  }
	      }
	    break;

	  case MPITEST_unsigned:
	    if (((unsigned *)buffer)[i] != value.Unsigned )
	      {
		error++;
		if (error == 1)
		  {
		    sprintf(info_buf, "i=%d, unsigned value=%u, expected %u",
			i, ((unsigned *)buffer)[i], value.Unsigned);
		    MPITEST_message(MPITEST_NONFATAL, info_buf);
		  }
	      }
	    break;

	  case MPITEST_unsigned_long:
	    if (((unsigned long *)buffer)[i] != value.UnsignedLong )
	      {
		error++;
		if (error == 1)
		  {
		    sprintf(info_buf, "i=%d, unsigned long value=%lu, expected %lu",
			i, ((unsigned long *)buffer)[i], value.UnsignedLong);
		    MPITEST_message(MPITEST_NONFATAL, info_buf);
		  }
	      }
	    break;

	  case MPITEST_float:
	    if (((float *)buffer)[i] != value.Float )
	      {
		error++;
		if (error == 1)
		  {
		    sprintf(info_buf, "i=%d, float value=%15.10f, expected %15.10f",
			i, ((float *)buffer)[i], value.Float);
		    MPITEST_message(MPITEST_NONFATAL, info_buf);
		  }
	      }
	    break;

	  case MPITEST_double:
	    if (((double *)buffer)[i] != value.Double )
	      {
		error++;
		if (error == 1)
		  {
		    sprintf(info_buf, "i=%d, double value=%15.10f, expected %15.10f",
			i, ((double *)buffer)[i], value.Double);
		    MPITEST_message(MPITEST_NONFATAL, info_buf);
		  }
	      }
	    break;

	  case MPITEST_char:
	    if (((signed char *)buffer)[i] != (signed char)value.Char )
	      {
		error++;
		if (error == 1)
		  {
		    sprintf(info_buf, "i=%d, (fixed) char value=%d, expected %d",
			i, ((signed char *)buffer)[i], 
			    (signed char)value.Char);
		    MPITEST_message(MPITEST_NONFATAL, info_buf);
		  }
	      }
	    break;

	  case MPITEST_unsigned_char:
	    if (((unsigned char *)buffer)[i] != value.UnsignedChar )
	      {
		error++;
		if (error == 1)
		  {
		    sprintf(info_buf, "i=%d, unsigned char value=%u, expected %u",
			i, ((unsigned char *)buffer)[i], value.UnsignedChar);
		    MPITEST_message(MPITEST_NONFATAL, info_buf);
		  }
	      }
	    break;

#if MPITEST_longlong_def
	  case MPITEST_longlong:
	    if (((long long int *)buffer)[i] != value.LongLong )
	      {
		error++;
		if (error == 1)
		  {
		    sprintf(info_buf, "i=%d, longlong int value=%lld, expected %lld",
			i, ((long long int *)buffer)[i], value.LongLong);
		    MPITEST_message(MPITEST_NONFATAL, info_buf);
		  }
	      }
	    break;
#endif

#if MPITEST_long_double_def
	  case MPITEST_long_double:
	    if (((long double *)buffer)[i] != value.LongDouble )
	      {
		error++;
		if (error == 1)
		  {
		    /* FIXME: Should be Lf where available */
		    sprintf(info_buf, "i=%d, long double value=%15.10Lf, expected %15.10Lf",
			i, ((long double *)buffer)[i], value.LongDouble);
		    MPITEST_message(MPITEST_NONFATAL, info_buf);
		  }
	      }
	    break;
#endif

	  case MPITEST_byte:
	    if (((MPITEST_byte_def *)buffer)[i] != value.Byte )
	      {
		error++;
		if (error == 1)
		  {
		    sprintf(info_buf, "i=%d, byte value=%u, expected %u",
			i, ((MPITEST_byte_def *)buffer)[i], value.Byte);
		    MPITEST_message(MPITEST_NONFATAL, info_buf);
		  }
	      }
	    break;

	  case MPITEST_derived1:
	  case MPITEST_derived2:
	    if (((derived1 *)buffer)[i].Int[k] != value.Int)
	      {
		error++;
		if (error == 1)
		  {
		    sprintf(info_buf, "i=%d, int value=%d, expected %d",
			i, ((derived1 *)buffer)[i].Int[k], value.Int);
		    MPITEST_message(MPITEST_NONFATAL, info_buf);
		  }
	      }
	    if (((derived1 *)buffer)[i].ShortInt[k] != value.ShortInt)
	      {
		error++;
		if (error == 1)
		  {
		    sprintf(info_buf, "i=%d, short int value=%d, expected %d",
			i, ((derived1 *)buffer)[i].ShortInt[k], value.ShortInt);
		    MPITEST_message(MPITEST_NONFATAL, info_buf);
		  }
	      }
	    if (((derived1 *)buffer)[i].Long[k] != value.Long)
	      {
		error++;
		if (error == 1)
		  {
		    sprintf(info_buf, "i=%d, long value=%ld, expected %ld",
			i, ((derived1 *)buffer)[i].Long[k], value.Long);
		    MPITEST_message(MPITEST_NONFATAL, info_buf);
		  }
	      }
	    if (((derived1 *)buffer)[i].UnsignedShort[k] != value.UnsignedShort)
	      {
		error++;
		if (error == 1)
		  {
		    sprintf(info_buf, "i=%d, unsigned short value=%u, expected %u",
			i, ((derived1 *)buffer)[i].UnsignedShort[k],
			value.UnsignedShort);
		    MPITEST_message(MPITEST_NONFATAL, info_buf);
		  }
	      }
	    if (((derived1 *)buffer)[i].Unsigned[k] != value.Unsigned)
	      {
		error++;
		if (error == 1)
		  {
		    sprintf(info_buf, "i=%d, unsigned value=%u, expected %u",
			i, ((derived1 *)buffer)[i].Unsigned[k], value.Unsigned);
		    MPITEST_message(MPITEST_NONFATAL, info_buf);
		  }
	      }
	    if (((derived1 *)buffer)[i].UnsignedLong[k] != value.UnsignedLong)
	      {
		error++;
		if (error == 1)
		  {
		    sprintf(info_buf, "i=%d, unsigned long value=%lu, expected %lu",
			i, ((derived1 *)buffer)[i].UnsignedLong[k],
			value.UnsignedLong);
		    MPITEST_message(MPITEST_NONFATAL, info_buf);
		  }
	      }
	    if (((derived1 *)buffer)[i].Float[k] != value.Float)
	      {
		error++;
		if (error == 1)
		  {
		    sprintf(info_buf, "i=%d, float value=%15.10f, expected %15.10f",
			i, ((derived1 *)buffer)[i].Float[k], value.Float);
		    MPITEST_message(MPITEST_NONFATAL, info_buf);
		  }
	      }
	    if (((derived1 *)buffer)[i].Double[k] != value.Double)
	      {
		error++;
		if (error == 1)
		  {
		    sprintf(info_buf, "i=%d, double value=%15.10f, expected %15.10f",
			i, ((derived1 *)buffer)[i].Double[k], value.Double);
		    MPITEST_message(MPITEST_NONFATAL, info_buf);
		  }
	      }
	    if (((derived1 *)buffer)[i].Char[k] != (signed char)value.Char)
	      {
		error++;
		if (error == 1)
		  {
		    sprintf(info_buf, "i=%d, (derived fixed) char value=%d, expected %d",
			i, ((derived1 *)buffer)[i].Char[k], (signed char)value.Char);
		    MPITEST_message(MPITEST_NONFATAL, info_buf);
		  }
	      }
	    if (((derived1 *)buffer)[i].UnsignedChar[k] != value.UnsignedChar)
	      {
		error++;
		if (error == 1)
		  {
		    sprintf(info_buf, "i=%d, unsigned char value=%u, expected %u",
			i, ((derived1 *)buffer)[i].UnsignedChar[k],
			value.UnsignedChar);
		    MPITEST_message(MPITEST_NONFATAL, info_buf);
		  }
	      }
#if MPITEST_longlong_def
	    if (((derived1 *)buffer)[i].LongLong[k] != value.LongLong)
	      {
		error++;
		if (error == 1)
		  {
		    sprintf(info_buf, "i=%d, longlong int value=%lld, expected %lld",
			i, ((derived1 *)buffer)[i].LongLong[k], value.LongLong);
		    MPITEST_message(MPITEST_NONFATAL, info_buf);
		  }
	      }
#endif

#if MPITEST_long_double_def
	    if (((derived1 *)buffer)[i].LongDouble[k] != value.LongDouble)
	      {
		error++;
		if (error == 1)
		  {
		    /* FIXME: should be Lf where available */
		    sprintf(info_buf, "i=%d, long double value=%15.10Lf, expected %15.10Lf",
                        i, ((derived1 *)buffer)[i].LongDouble[k],
			value.LongDouble);
                    MPITEST_message(MPITEST_NONFATAL, info_buf);
                  }
              }    
#endif
	    break;
	  }

      }
    return error;
  }


int MPITEST_buffer_errors_ov(int buffer_type, int length,
			  struct dataTemplate value, void *buffer)
/***********************************************************************
  Checks that the specified buffer of the specified type at the specified
  length is set to the specified value.  Typically called to check one
  past the message length to ensure there was no overflow.

  Arguments :
  buffer_type         INPUT, integer specifying the type of the
                       buffer.  Legal values are specified in the file
		       include/mpitest_def.h .

  length              INPUT, integer length of the buffer.

  value               INPUT, "correct" value that should be in buffer.
                       This argument is declared as struct dataTemplate
		       so that all types may be accomodated.

  buffer              OUTPUT, the buffer to be checked.  Passed in as a void
                       pointer so that different types may all be
		       encompassed in one call.

  This function uses a switch statement to differentiate between the
  different allowed types.  The type casts are required so that the void
  pointers may be dereferenced.

  History :

  01/28/96   gt  Created
***********************************************************************/

  {
    int i, error, k;
    char   info_buf[MPI_MAX_ERROR_STRING];


    if (buffer_type == MPITEST_derived2)
	k = MPITEST_me%2;
    else
	k = 0;
    error = 0;

    i = length;
	switch (buffer_type)
	  {
	  case MPITEST_int:
	    if (((int *)buffer)[i] != value.Int )
	      {
		error++;
		if (error == 1)
		  {
		    sprintf(info_buf, "i=%d, overflow value=%d, expected %d",
			i, ((int *)buffer)[i], value.Int);
		    MPITEST_message(MPITEST_NONFATAL, info_buf);
		  }
	      }
	    break;

	  case MPITEST_short_int:
	    if (((short int *)buffer)[i] != value.ShortInt )
	      {
		error++;
		if (error == 1)
		  {
		    sprintf(info_buf, "i=%d, overflow value=%d, expected %d",
			i, ((short int *)buffer)[i], value.ShortInt);
		    MPITEST_message(MPITEST_NONFATAL, info_buf);
		  }
	      }
	    break;

	  case MPITEST_long:
	    if (((long *)buffer)[i] != value.Long )
	      {
		error++;
		if (error == 1)
		  {
		    sprintf(info_buf, "i=%d, overflow value=%ld, expected %ld",
			i, ((long *)buffer)[i], value.Long);
		    MPITEST_message(MPITEST_NONFATAL, info_buf);
		  }
	      }
	    break;

	  case MPITEST_unsigned_short:
	    if (((unsigned short *)buffer)[i] != value.UnsignedShort )
	      {
		error++;
		if (error == 1)
		  {
		    sprintf(info_buf, "i=%d, overflow value=%u, expected %u",
			i, ((unsigned short *)buffer)[i], value.UnsignedShort);
		    MPITEST_message(MPITEST_NONFATAL, info_buf);
		  }
	      }
	    break;

	  case MPITEST_unsigned:
	    if (((unsigned *)buffer)[i] != value.Unsigned )
	      {
		error++;
		if (error == 1)
		  {
		    sprintf(info_buf, "i=%d, overflow value=%u, expected %u",
			i, ((unsigned *)buffer)[i], value.Unsigned);
		    MPITEST_message(MPITEST_NONFATAL, info_buf);
		  }
	      }
	    break;

	  case MPITEST_unsigned_long:
	    if (((unsigned long *)buffer)[i] != value.UnsignedLong )
	      {
		error++;
		if (error == 1)
		  {
		    sprintf(info_buf, "i=%d, overflow value=%lu, expected %lu",
			i, ((unsigned long *)buffer)[i], value.UnsignedLong);
		    MPITEST_message(MPITEST_NONFATAL, info_buf);
		  }
	      }
	    break;

	  case MPITEST_float:
	    if (((float *)buffer)[i] != value.Float )
	      {
		error++;
		if (error == 1)
		  {
		    sprintf(info_buf, "i=%d, overflow value=%15.10f, expected %15.10f",
			i, ((float *)buffer)[i], value.Float);
		    MPITEST_message(MPITEST_NONFATAL, info_buf);
		  }
	      }
	    break;

	  case MPITEST_double:
	    if (((double *)buffer)[i] != value.Double )
	      {
		error++;
		if (error == 1)
		  {
		    sprintf(info_buf, "i=%d, overflow value=%15.10f, expected %15.10f",
			i, ((double *)buffer)[i], value.Double);
		    MPITEST_message(MPITEST_NONFATAL, info_buf);
		  }
	      }
	    break;

	  case MPITEST_char:
	    if (((signed char *)buffer)[i] != (signed char)value.Char )
	      {
		error++;
		if (error == 1)
		  {
		    sprintf(info_buf, "i=%d, overflow value=%d, expected %d",
			i, ((signed char *)buffer)[i], value.Char);
		    MPITEST_message(MPITEST_NONFATAL, info_buf);
		  }
	      }
	    break;

	  case MPITEST_unsigned_char:
	    if (((unsigned char *)buffer)[i] != value.UnsignedChar )
	      {
		error++;
		if (error == 1)
		  {
		    sprintf(info_buf, "i=%d, overflow value=%u, expected %u",
			i, ((unsigned char *)buffer)[i], value.UnsignedChar);
		    MPITEST_message(MPITEST_NONFATAL, info_buf);
		  }
	      }
	    break;

#if MPITEST_longlong_def
	  case MPITEST_longlong:
	    if (((long long int *)buffer)[i] != value.LongLong )
	      {
		error++;
		if (error == 1)
		  {
		    sprintf(info_buf, "i=%d, overflow value=%lld, expected %lld",
			i, ((long long int *)buffer)[i], value.LongLong);
		    MPITEST_message(MPITEST_NONFATAL, info_buf);
		  }
	      }
	    break;
#endif

#if MPITEST_long_double_def
	  case MPITEST_long_double:
	    if (((long double *)buffer)[i] != value.LongDouble )
	      {
		error++;
		if (error == 1)
		  {
		    /* FIXME: should use Lf where available */
		    sprintf(info_buf, "i=%d, overflow value=%15.10Lf, expected %15.10Lf",
			i, ((long double *)buffer)[i], value.LongDouble);
		    MPITEST_message(MPITEST_NONFATAL, info_buf);
		  }
	      }
	    break;
#endif

	  case MPITEST_byte:
	    if (((MPITEST_byte_def *)buffer)[i] != value.Byte)
	      {
		error++;
		if (error == 1)
		  {
		    sprintf(info_buf, "i=%d, overflow value=%u, expected %u",
			i, ((MPITEST_byte_def *)buffer)[i], value.Byte);
		    MPITEST_message(MPITEST_NONFATAL, info_buf);
		  }
	      }
	    break;

	  case MPITEST_derived1:
	  case MPITEST_derived2:
	    if (((derived1 *)buffer)[i].Int[k] != value.Int)
	      {
		error++;
		if (error == 1)
		  {
		    sprintf(info_buf, "i=%d, int value=%d, expected %d",
			i, ((derived1 *)buffer)[i].Int[k], value.Int);
		    MPITEST_message(MPITEST_NONFATAL, info_buf);
		  }
	      }
	    if (((derived1 *)buffer)[i].ShortInt[k] != value.ShortInt)
	      {
		error++;
		if (error == 1)
		  {
		    sprintf(info_buf, "i=%d, short int value=%d, expected %d",
			i, ((derived1 *)buffer)[i].ShortInt[k], value.ShortInt);
		    MPITEST_message(MPITEST_NONFATAL, info_buf);
		  }
	      }
	    if (((derived1 *)buffer)[i].Long[k] != value.Long)
	      {
		error++;
		if (error == 1)
		  {
		    sprintf(info_buf, "i=%d, long value=%ld, expected %ld",
			i, ((derived1 *)buffer)[i].Long[k], value.Long);
		    MPITEST_message(MPITEST_NONFATAL, info_buf);
		  }
	      }
	    if (((derived1 *)buffer)[i].UnsignedShort[k] != value.UnsignedShort)
	      {
		error++;
		if (error == 1)
		  {
		    sprintf(info_buf, "i=%d, unsigned short value=%u, expected %u",
			i, ((derived1 *)buffer)[i].UnsignedShort[k],
			value.UnsignedShort);
		    MPITEST_message(MPITEST_NONFATAL, info_buf);
		  }
	      }
	    if (((derived1 *)buffer)[i].Unsigned[k] != value.Unsigned)
	      {
		error++;
		if (error == 1)
		  {
		    sprintf(info_buf, "i=%d, unsigned value=%u, expected %u",
			i, ((derived1 *)buffer)[i].Unsigned[k], value.Unsigned);
		    MPITEST_message(MPITEST_NONFATAL, info_buf);
		  }
	      }
	    if (((derived1 *)buffer)[i].UnsignedLong[k] != value.UnsignedLong)
	      {
		error++;
		if (error == 1)
		  {
		    sprintf(info_buf, "i=%d, unsigned long value=%lu, expected %lu",
			i, ((derived1 *)buffer)[i].UnsignedLong[k],
			value.UnsignedLong);
		    MPITEST_message(MPITEST_NONFATAL, info_buf);
		  }
	      }
	    if (((derived1 *)buffer)[i].Float[k] != value.Float)
	      {
		error++;
		if (error == 1)
		  {
		    sprintf(info_buf, "i=%d, float value=%15.10f, expected %15.10f",
			i, ((derived1 *)buffer)[i].Float[k], value.Float);
		    MPITEST_message(MPITEST_NONFATAL, info_buf);
		  }
	      }
	    if (((derived1 *)buffer)[i].Double[k] != value.Double)
	      {
		error++;
		if (error == 1)
		  {
		    sprintf(info_buf, "i=%d, double value=%15.10f, expected %15.10f",
			i, ((derived1 *)buffer)[i].Double[k], value.Double);
		    MPITEST_message(MPITEST_NONFATAL, info_buf);
		  }
	      }
	    if ((signed char)((derived1 *)buffer)[i].Char[k] != 
		(signed char)value.Char)
	      {
		error++;
		if (error == 1)
		  {
		    sprintf(info_buf, "i=%d, (derived fixed 2) char value=%d, expected %d",
			i, (signed char)((derived1 *)buffer)[i].Char[k], 
			    (signed char)value.Char);
		    MPITEST_message(MPITEST_NONFATAL, info_buf);
		  }
	      }
	    if (((derived1 *)buffer)[i].UnsignedChar[k] != value.UnsignedChar)
	      {
		error++;
		if (error == 1)
		  {
		    sprintf(info_buf, "i=%d, unsigned char value=%u, expected %u",
			i, ((derived1 *)buffer)[i].UnsignedChar[k],
			value.UnsignedChar);
		    MPITEST_message(MPITEST_NONFATAL, info_buf);
		  }
	      }
#if MPITEST_longlong_def
	    if (((derived1 *)buffer)[i].LongLong[k] != value.LongLong)
	      {
		error++;
		if (error == 1)
		  {
		    sprintf(info_buf, "i=%d, longlong int value=%lld, expected %lld",
			i, ((derived1 *)buffer)[i].LongLong[k], value.LongLong);
		    MPITEST_message(MPITEST_NONFATAL, info_buf);
		  }
	      }
#endif

#if MPITEST_long_double_def
	    if (((derived1 *)buffer)[i].LongDouble[k] != value.LongDouble)
	      {
		error++;
		if (error == 1)
		  {
		    sprintf(info_buf, "i=%d, long double value=%15.10Lf, expected %15.10Lf",
                        i, ((derived1 *)buffer)[i].LongDouble[k],
			value.LongDouble);
                    MPITEST_message(MPITEST_NONFATAL, info_buf);
                  }
              }    
#endif
	  }

    return error;
  }


int MPITEST_buffer_errors_v(int buffer_type, int numblocks,
			    int *counts, int *displs, struct dataTemplate *values,
			    void *buffer)
/***********************************************************************
  Checks the values of the memory pointed to by buffer.  The type of the
  data is specified by the buffer_type parameter.  There are numblocks
  different chunks of data, the ith chunk has length counts[i], is located
  at buffer+(displs[i]*extent(data type)), and should be equal to values[i].


  Arguments :
  buffer_type         INPUT, integer specifying the type of the
                       buffer.  Legal values are specified in the file
		       include/mpitest_def.h .

  numblocks           INPUT, integer number of blocks of data to be set

  counts              INPUT, pointer to integer array containing the
                       lengths of blocks. counts[i] is the length of the
		       ith block

  displs              INPUT, pointer to integer array containing the
                       displacements from buffer of the blocks.  displs[i]
		       is the displacemnt of the ith block.

  values              INPUT, pointer to array of values that should be in
                       buffer.  This parameter is declared as an array
		       of struct dataTemplate's so that all possible
		       buffer types may be accomodated.

  buffer              OUTPUT, the buffer to be set.  Passed in as a void
                       pointer so that different types may all be
		       encompassed in one call.

  This function uses a switch statement to differentiate between the
  different allowed types.  The type casts are required so that the void
  pointers may be dereferenced.


  History :

  10/13/95   gm  Created
***********************************************************************/

  {
    int i, j, k, error;
    char info_buf[MPI_MAX_ERROR_STRING];

    if (buffer_type == MPITEST_derived2)
	k = MPITEST_me%2;
    else
	k = 0;
    error = 0;

    for (i=0; i< numblocks; i++)
      {
	for (j=displs[i];j<displs[i]+counts[i]; j++)
	  {
	    switch (buffer_type)
	      {
	      case MPITEST_int:
		if (((int *)buffer)[j] != values[i].Int)
		  {
		    error++;
		    if (error == 1)
		      {
			sprintf(info_buf, "i,j=%d,%d, int value=%d, expected %d",
			    i, j, ((int *)buffer)[j], values[i].Int);
			MPITEST_message(MPITEST_NONFATAL, info_buf);
		      }
		  }
		break;

	      case MPITEST_short_int:
		if (((short int *)buffer)[j] != values[i].ShortInt)
		  {
		    error++;
		    if (error == 1)
		      {
			sprintf(info_buf, "i,j=%d,%d, short int value=%d, expected %d",
			    i, j, ((short int *)buffer)[j], values[i].ShortInt);
			MPITEST_message(MPITEST_NONFATAL, info_buf);
		      }
		  }
		break;

	      case MPITEST_long:
		if (((long *)buffer)[j] != values[i].Long)
		  {
		    error++;
		    if (error == 1)
		      {
			sprintf(info_buf, "i,j=%d,%d, long value=%ld, expected %ld",
			    i, j, ((long *)buffer)[j], values[i].Long);
			MPITEST_message(MPITEST_NONFATAL, info_buf);
		      }
		  }
		break;

	      case MPITEST_unsigned_short:
		if (((unsigned short *)buffer)[j] != values[i].UnsignedShort)
		  {
		    error++;
		    if (error == 1)
		      {
			sprintf(info_buf, "i,j=%d,%d, unsigned short value=%u, expected %u",
			    i, j, ((unsigned short *)buffer)[j], values[i].UnsignedShort);
			MPITEST_message(MPITEST_NONFATAL, info_buf);
		      }
		  }
		break;

	      case MPITEST_unsigned:
		if (((unsigned *)buffer)[j] != values[i].Unsigned)
		  {
		    error++;
		    if (error == 1)
		      {
			sprintf(info_buf, "i,j=%d,%d, unsigned value=%u, expected %u",
			    i, j, ((unsigned *)buffer)[j], values[i].Unsigned);
			MPITEST_message(MPITEST_NONFATAL, info_buf);
		      }
		  }
		break;

	      case MPITEST_unsigned_long:
		if (((unsigned long *)buffer)[j] != values[i].UnsignedLong)
		  {
		    error++;
		    if (error == 1)
		      {
			sprintf(info_buf, "i,j=%d,%d, unsigned long value=%lu, expected %lu",
			    i, j, ((unsigned long *)buffer)[j], values[i].UnsignedLong);
			MPITEST_message(MPITEST_NONFATAL, info_buf);
		      }
		  }
		break;

	      case MPITEST_float:
		if (((float *)buffer)[j] != values[i].Float)
		  {
		    error++;
		    if (error == 1)
		      {
			sprintf(info_buf, "i,j=%d,%d, float value=%15.10f, expected %15.10f",
			    i, j, ((float *)buffer)[j], values[i].Float);
			MPITEST_message(MPITEST_NONFATAL, info_buf);
		      }
		  }
		break;

	      case MPITEST_double:
		if (((double *)buffer)[j] != values[i].Double)
		  {
		    error++;
		    if (error == 1)
		      {
			sprintf(info_buf, "i,j=%d,%d, dounle value=%15.10f, expected %15.10f",
			    i, j, ((double *)buffer)[j], values[i].Double);
			MPITEST_message(MPITEST_NONFATAL, info_buf);
		      }
		  }
		break;

	      case MPITEST_char:
		if (((signed char *)buffer)[j] != (signed char)values[i].Char)
		  {
		    error++;
		    if (error == 1)
		      {
			sprintf(info_buf, "i,j=%d,%d, char value=%d, expected %d",
			    i, j, ((signed char *)buffer)[j], 
				(signed char)values[i].Char);
			MPITEST_message(MPITEST_NONFATAL, info_buf);
		      }
		  }
		break;

	      case MPITEST_unsigned_char:
		if (((unsigned char *)buffer)[j] != values[i].UnsignedChar)
		  {
		    error++;
		    if (error == 1)
		      {
			sprintf(info_buf, "i,j=%d,%d, unsigned char value=%u, expected %u",
			    i, j, ((unsigned char *)buffer)[j], values[i].UnsignedChar);
			MPITEST_message(MPITEST_NONFATAL, info_buf);
		      }
		  }
		break;

#if MPITEST_longlong_def
	      case MPITEST_longlong:
		if (((long long int *)buffer)[j] != values[i].LongLong )
		  {
		    error++;
		    if (error == 1)
		      {
			sprintf(info_buf, "i,j=%d,%d, longlong int value=%lld, expected %lld",
			    i, j, ((long long int *)buffer)[j], values[i].LongLong);
			MPITEST_message(MPITEST_NONFATAL, info_buf);
		      }
		  }
		break;
#endif

#if MPITEST_long_double_def
	      case MPITEST_long_double:
		if (((long double *)buffer)[j] != values[i].LongDouble )
		  {
		    error++;
		    if (error == 1)
		      {
			/* FIXME: Should be Lf format where available */
			sprintf(info_buf, "i,j=%d,%d, long double value=%15.10Lf, expected %15.10Lf",
			    i, j, ((long double *)buffer)[j], values[i].LongDouble);
			MPITEST_message(MPITEST_NONFATAL, info_buf);
		      }
		  }
		break;
#endif

	      case MPITEST_byte:
		if (((MPITEST_byte_def *)buffer)[j] != values[i].Byte)
		  {
		    error++;
		    if (error == 1)
		      {
			sprintf(info_buf, "i,j=%d,%d, byte value=%u, expected %u",
			    i, j, ((MPITEST_byte_def *)buffer)[j], values[i].Byte);
			MPITEST_message(MPITEST_NONFATAL, info_buf);
		      }
		  }
		break;

	      case MPITEST_derived1:
	      case MPITEST_derived2:
		if (((derived1 *)buffer)[j].Int[k] != values[i].Int)
		  {
		    error++;
		    if (error == 1)
		      {
			sprintf(info_buf, "i=%d, int value=%d, expected %d",
			    i, ((derived1 *)buffer)[j].Int[k], values[i].Int);
			MPITEST_message(MPITEST_NONFATAL, info_buf);
		      }
		  }
		if (((derived1 *)buffer)[j].ShortInt[k] != values[i].ShortInt)
		  {
		    error++;
		    if (error == 1)
		      {
			sprintf(info_buf, "i=%d, short int value=%d, expected %d",
			    i, ((derived1 *)buffer)[j].ShortInt[k],
			    values[i].ShortInt);
			MPITEST_message(MPITEST_NONFATAL, info_buf);
		      }
		  }
		if (((derived1 *)buffer)[j].Long[k] != values[i].Long)
		  {
		    error++;
		    if (error == 1)
		      {
			sprintf(info_buf, "i=%d, long value=%ld, expected %ld",
			    i, ((derived1 *)buffer)[j].Long[k], values[i].Long);
			MPITEST_message(MPITEST_NONFATAL, info_buf);
		      }
		  }
		if (((derived1 *)buffer)[j].UnsignedShort[k] !=
			values[i].UnsignedShort)
		  {
		    error++;
		    if (error == 1)
		      {
			sprintf(info_buf, "i=%d, unsigned short value=%u, expected %u",
			    i, ((derived1 *)buffer)[j].UnsignedShort[k],
			    values[i].UnsignedShort);
			MPITEST_message(MPITEST_NONFATAL, info_buf);
		      }
		  }
	    if (((derived1 *)buffer)[j].Unsigned[k] != values[i].Unsigned)
		  {
		    error++;
		    if (error == 1)
		      {
			sprintf(info_buf, "i=%d, unsigned value=%u, expected %u",
			    i, ((derived1 *)buffer)[j].Unsigned[k],
			    values[i].Unsigned);
			MPITEST_message(MPITEST_NONFATAL, info_buf);
		      }
		  }
		if (((derived1 *)buffer)[j].UnsignedLong[k] !=
			values[i].UnsignedLong)
		  {
		    error++;
		    if (error == 1)
		      {
			sprintf(info_buf, "i=%d, unsigned long value=%lu, expected %lu",
			    i, ((derived1 *)buffer)[j].UnsignedLong[k],
			    values[i].UnsignedLong);
			MPITEST_message(MPITEST_NONFATAL, info_buf);
		      }
		  }
		if (((derived1 *)buffer)[j].Float[k] != values[i].Float)
		  {
		    error++;
		    if (error == 1)
		      {
			sprintf(info_buf, "i=%d, float value=%15.10f, expected %15.10f",
			i, ((derived1 *)buffer)[j].Float[k], values[i].Float);
			MPITEST_message(MPITEST_NONFATAL, info_buf);
		      }
		  }
	    if (((derived1 *)buffer)[j].Double[k] != values[i].Double)
		  {
		    error++;
		    if (error == 1)
		      {
			sprintf(info_buf, "i=%d, double value=%15.10f, expected %15.10f",
			    i, ((derived1 *)buffer)[j].Double[k],
			    values[i].Double);
			MPITEST_message(MPITEST_NONFATAL, info_buf);
		      }
		  }
		if (((derived1 *)buffer)[j].Char[k] != (signed char)values[i].Char)
		  {
		    error++;
		    if (error == 1)
		      {
			sprintf(info_buf, "i=%d, (derived array) char value=%d, expected %d",
			    i, ((derived1 *)buffer)[j].Char[k], 
				(signed char)values[i].Char);
			MPITEST_message(MPITEST_NONFATAL, info_buf);
		      }
		  }
		if (((derived1 *)buffer)[j].UnsignedChar[k] !=
			values[i].UnsignedChar)
		  {
		    error++;
		    if (error == 1)
		      {
			sprintf(info_buf, "i=%d, unsigned char value=%u, expected %u",
			    i, ((derived1 *)buffer)[j].UnsignedChar[k],
			    values[i].UnsignedChar);
			MPITEST_message(MPITEST_NONFATAL, info_buf);
		      }
		  }
#if MPITEST_longlong_def
		if (((derived1 *)buffer)[j].LongLong[k] != values[i].LongLong)
		  {
		    error++;
		    if (error == 1)
		      {
			sprintf(info_buf, "i=%d, longlong int value=%lld, expected %lld",
			    i, ((derived1 *)buffer)[j].LongLong[k],
			    values[i].LongLong);
			MPITEST_message(MPITEST_NONFATAL, info_buf);
		      }
		  }
#endif

#if MPITEST_long_double_def
		if (((derived1 *)buffer)[j].LongDouble[k] !=
			values[i].LongDouble)
		  {
		    error++;
		    if (error == 1)
		      {
			/* FIXME: The formats here for the 4th and 5th arguments
			   do not match long double items.  Where supported,
			   they should be Lf instead of f */
			sprintf(info_buf, "i=%d, long double value=%15.10Lf, expected %15.10Lf",
                            i, ((derived1 *)buffer)[j].LongDouble[k],
			    values[i].LongDouble);
                        MPITEST_message(MPITEST_NONFATAL, info_buf);
                      }
                  }    
#endif
	        break;
	      }
	  }
      }

    return error;
  }


int MPITEST_get_buffer(int buffer_type, int length, void **buffer)
/*****************************************************************
  Allocate a buffer of type specified by buffer_type and number
  of elements given by length.

  Arguments :
  buffer_type         INPUT, integer specifying the type of the
                       buffer.  Legal values are specified in
		       include/mpitest_def.h .

  length              INPUT, integer number of elements to be
                       allocated for buffer.

  buffer              OUTPUT, pointer to a pointer to the
                       allocated buffer.  This parameter is
		       pointer to pointer to void so that
		       allocation of different types can be
		       encapsulated in one call.

  This function uses a switch statement to differentiate between
  the different types allowed by the buffer_type parameter.


  History :

  10/13/95   gm  Created
*****************************************************************/
{
  int error=0;

  switch (buffer_type)
    {
    case MPITEST_int:
      *buffer = (int *)malloc((length + 1)*sizeof(int));
      break;
    case MPITEST_short_int:
      *buffer = (short int *)malloc((length + 1)*sizeof(short int));
      break;
    case MPITEST_long:
      *buffer = (long *)malloc((length + 1)*sizeof(long));
      break;
    case MPITEST_unsigned_short:
      *buffer = (unsigned short *)malloc((length + 1)*sizeof(unsigned short));
      break;
    case MPITEST_unsigned:
      *buffer = (unsigned *)malloc((length + 1)*sizeof(unsigned));
      break;
    case MPITEST_unsigned_long:
      *buffer = (unsigned long *)malloc((length + 1)*sizeof(unsigned long));
      break;
    case MPITEST_float:
      *buffer = (float *)malloc((length + 1)*sizeof(float));
      break;
    case MPITEST_double:
      *buffer = (double *)malloc((length + 1)*sizeof(double));
      break;
    case MPITEST_char:
      *buffer = (signed char *)malloc((length + 1)*sizeof(signed char));
      break;
    case MPITEST_unsigned_char:
      *buffer = (unsigned char *)malloc((length + 1)*sizeof(unsigned char));
      break;
#if MPITEST_longlong_def
    case MPITEST_longlong:
      *buffer = (long long int *)malloc((length + 1)*sizeof(long long int));
      break;
#endif
#if MPITEST_long_double_def
    case MPITEST_long_double:
      *buffer = (long double *)malloc((length + 1)*sizeof(long double));
      break;
#endif
    case MPITEST_byte:
      *buffer = (MPITEST_byte_def *)malloc((length + 1)*sizeof(MPITEST_byte_def));
      break;
    case MPITEST_derived1:
    case MPITEST_derived2:
      *buffer = (derived1 *)malloc((length + 1)*sizeof(derived1));
      break;
    }
  if (*buffer==NULL )
    {
       MPITEST_message(MPITEST_FATAL,"Failed to allocate memory in MPITEST_get_buffer\n");
    }
  return error;
}





int MPITEST_num_message_lengths(void)
/********************************************************************
  Return the number of messages to be looped over as specified
in the configuration array MPITEST_message_lengths[] (defined
in include/mpitest_cfg.h.)

  Arguments : none

  Algorithm :
Using a while loop, step through the MPITEST_message_lengths[] array
until the MPITEST_END_TOKEN is found.  Each time through the while
loop, execute a switch statement whose key is the curent element
of the MPITEST_message_lengths[] array.  From here on in this comment
we refer to MPITEST_message_lengths[] as array[].

The body of the switch does the following.  (element is equal
to the current element of array[].)
1) Error checking.
  a) check to make sure that none of the operations
  specified below will read memory beyond the end of
  array[].
  b) For the case of element = magic number MPITEST_MULT_INC, first set
	  start = array[ ++index];
	  end = array[ ++index];
	  inc = array[ ++index];
  then check to see that end >= start > 0, and inc > 1.
  c) For the case element = magic number MPITEST_ADD_INC, first set
	  start = array[ ++index];
	  end = array[ ++index];
	  inc = array[ ++index];
  then check to see that end >= start >= 0, and inc > 0.
  d) For the case element = MPITEST_REPEAT, first set
          value = array[ ++index];
	  rep_factor = arrray[ ++index];
  then check to see that value > 0 and rep_factor > 0.
  e) For the default case, make sure that element is greater
  than or equal to 0.

2) Counting the number of messages.
Within the body of the aforementioned while loop, increment the
total variable (which starts out at 0) in the following way,
a) for element = MPITEST_MULT_INC,
total += 1 + log10((end/start))/log10(inc).
In this case, increment the index by 4 (to get past element, start,
end, and inc.)
b) for element = MPITEST_ADD_INC,
total += 1 + (end-start)/inc.
In this case, increment the index by 4 (to get past element, start,
end, and inc.)
c) for element = MPITEST_REPEAT,
total += rep_factor.
In this case, increment the index by 3 (to get past value and
rep_factor.)
d) for the default case, total+=1 and index += 1.

3) return total


  History :

  10/13/95   gm  Created
********************************************************************/
{
  int
    index = 0,
    element,
    total = 0,
    start,
    end,
    inc,
    value,
    rep_factor,
    *array = MPITEST_message_lengths;

  while ((element=array[index]) != MPITEST_END_TOKEN)
    {
      switch(element)
	{
	case MPITEST_MULT_INC:
	  if (index > MPITEST_CONFIG_ARRAY_SIZE-3)
	    MPITEST_message(MPITEST_FATAL,
			    "Index out of bounds in MPITEST_num_message_lengths()");
	  start = array[ ++index];
	  end = array[ ++index];
	  inc = array[ ++index];
	  if (start < 1)
	    MPITEST_message(MPITEST_FATAL,
			    "MPITEST_MULT_INC start value less than 1");
	  if (end < start)
	    MPITEST_message(MPITEST_FATAL,
			    "MPITEST_MULT_INC end value less than start value");
	  if (inc < 2)
	    MPITEST_message(MPITEST_FATAL,
			    "MPITEST_MULT_INC increment less than 2");
	  total += 1 + (log10(end/start )/log10(inc));
	  index ++;
	  break;
	case MPITEST_REPEAT:
	  if (index > MPITEST_CONFIG_ARRAY_SIZE-3)
	    MPITEST_message(MPITEST_FATAL,
			    "Index out of bounds in MPITEST_num_message_lengths()");
	  value = array[ ++index];
	  rep_factor = array[ ++index];
	  if (value < 0)
	    MPITEST_message(MPITEST_FATAL,
			    "MPITEST_REPEAT length value less than 0");
	  if (rep_factor < 0)
	    MPITEST_message(MPITEST_FATAL,
			    "MPITEST_REPEAT number of repititions less than 0");
	  total += rep_factor;
	  index ++;
	  break;
	case MPITEST_ADD_INC:
	  if (index > MPITEST_CONFIG_ARRAY_SIZE-3)
	    MPITEST_message(MPITEST_FATAL,
			    "Index out of bounds in MPITEST_num_message_lengths()");
	  start = array[ ++index];
	  end = array[ ++index];
	  inc = array[ ++index];
	  if (start < 0)
	    MPITEST_message(MPITEST_FATAL,
			    "MPITEST_ADD_INC start value less than 0");
	  if (end < start)
	    MPITEST_message(MPITEST_FATAL,
			    "MPITEST_ADD_INC end value less than start value");
	  if (inc < 1)
	    MPITEST_message(MPITEST_FATAL,
			    "MPITEST_ADD_INC increment less than 1");
	  total += 1 + ((end-start )/ inc);
	  index++;
	  break;
	default:
	  if (element < 0)
	    MPITEST_message(MPITEST_FATAL,
			    "Negative message length in MPITEST_message_lengths[]");
	  total++;
	  index++;
	  break;
	}
    }
  return total;
}


int MPITEST_get_max_message_length(void)
/***********************************************************************
  Return the largest message length specified in the
MPITEST_message_lengths[] array.

Repeatedly call MPITEST_get_message_length(i) for 'i' running from
0 to MPITEST_num_message_lengths()-1 .  Return the largest value found.

  History :

  10/13/95   gm  Created
************************************************************************/
{
  int
    i,
    element,
    max_element = 0;

  for (i=0; i< MPITEST_num_message_lengths();i++)
    {
      if ((element=MPITEST_get_message_length(i)) > max_element)
	{
	  max_element = element;
	}
    }

  return max_element;
}





int MPITEST_get_message_length(int i)
/***********************************************************************
  Return the length of the ith message as specified by the
configuration array MPITEST_message_lengths[] (hereafter referred
to as "array[]").

Arguments :
    integer i      INPUT, specifies which iteration we want the message
                    length for.

Algorithm :
1) Error checking.  This routine makes a call to
MPITEST_num_message_lengths(), which error checks array[].  The only
error checking done by this routine is that i is less than the
return value of MPITEST_num_message_lengths().

2) Static set-up
In the case that i==0, two steps are taken.
  a) the static int max_i is set to MPITEST_num_message_lengths(),
  and thereafter is used to error check the argument i.

  b) the breakpoints[] array is set up.  The element breakpoints[2*i]
  is set to the iteration at which the message length style changes from
  style "i-1" to style "i".  "Message length style" refers to either
  MPITEST_MULT_INC, MPITEST_ADD_INC, MPITEST_REPEAT,or a simple
  enumeration of a non-negative message length.  The element
  breakoints[2*i+1] gives the index into array[] at which the ith style
  begins.  The reason for setting these values should become clear as
  the algorithm is described further below.

3) Determination of the message length
  a) The current message length style is determined.  This is
  accomplished by stepping through the breakpoints[] array until
  (i < breakpoints[ 2*(breakpoint_index)]).  When this condition is
  satisfied, we set
     index = i - breakpoints[ 2*(breakpoint_index-1)]
     element = array[ breakpoints[ 2*(breakpoint_index-1)+1]
  Referring to the definitions of the breakpoint array above,
  we see that now index contains the rank of the current iteration
  in the current message style, and element contains the element of
  array[] which defines the current message style.  For example,
  if (i=17) and a new message style began at iteration 15, then index
  would equal 2.  If the message style which began at iteration 15 was
  MPITEST_ADD_INC, then element would now contain MPITEST_ADD_INC.  If
  the current message style were simple message length enumeration, then
  element would contain the current message length.

  b) The current message length is determined.
  This is accomplished by using a switch whose key is 'element'.
  If element is one of the magic numbers, then the appropriate number
  of increments are added to the starting value to get to the current
  value.  The 'appropriate number' is simply the index variable from
  step 3a).  Otherwise, current length is simply element.

  4) The current length is returned.

  History :

  10/13/95   gm  Created
***********************************************************************/
{
int
  index = 0,  /* to keep the compiler happy */
  element,
  start,
  end,
  inc,
  current_total=0,
  breakpoint_index=0,
  length,
  value,
  rep_factor,
  *array = MPITEST_message_lengths;

static int  breakpoints[ 2*MPITEST_CONFIG_ARRAY_SIZE];

static int max_i;

/* For first calls, set the maximum parameter value
   and the array of breakpoints */
  if (i==0)
    {
      max_i = MPITEST_num_message_lengths();
      /* set up the breakpoints array */
      index = 0;
      breakpoint_index = 0;
      breakpoints[ 2*breakpoint_index] = 0;
      breakpoints[ 2*breakpoint_index+1] = 0;
      breakpoint_index++;

      while ((element=array[index]) != MPITEST_END_TOKEN)
	{
	  switch (element)
	    {
	    case MPITEST_MULT_INC:
	      start = array[ index+1];
	      end = array[ index+2];
	      inc = array[ index+3];
	      current_total += 1 + (log10(end/start )/log10(inc));
	      breakpoints[ 2*breakpoint_index] = current_total;
	      breakpoints[ 2*breakpoint_index+1] = index;
	      breakpoint_index++;
	      index +=4;
	      break;
	    case MPITEST_ADD_INC:
	      start = array[ index+1];
	      end = array[ index+2];
	      inc = array[ index+3];
 	      current_total += 1 + ((end-start )/ inc);
	      breakpoints[ 2*breakpoint_index] = current_total;
	      breakpoints[ 2*breakpoint_index+1] = index;
	      breakpoint_index++;
	      index += 4;
	      break;
	    case MPITEST_REPEAT:
	      value = array[index + 1];
	      rep_factor = array[ index + 2];
	      current_total += rep_factor;
	      breakpoints[ 2*breakpoint_index] = current_total;
	      breakpoints[ 2*breakpoint_index+1] = index;
	      breakpoint_index++;
	      index += 3;
	      break;
	    default:
	      current_total++;
	      breakpoints[ 2*breakpoint_index] = current_total;
	      breakpoints[ 2*breakpoint_index+1] = index;
	      breakpoint_index++;
	      index++;
	      break;
	    }
	}
    }

/*  Error Checking */
if (i >= max_i)
    MPITEST_message(MPITEST_FATAL, "Iteration parameter out of bounds in \
routine MPITEST_get_message_length()\n");

for (breakpoint_index = 0;breakpoint_index< MPITEST_CONFIG_ARRAY_SIZE;breakpoint_index++)
  {
    if (i< breakpoints[ 2*breakpoint_index])
      {
	index = i - breakpoints[ 2*(breakpoint_index-1)];
	break;
      }
  }

element = array[ breakpoints[ 2*breakpoint_index+1]];

switch(element)
  {
  case MPITEST_MULT_INC:
    start = array[ 1 + breakpoints[ 2*breakpoint_index+1]];
    inc = array[ 3 + breakpoints[ 2*breakpoint_index+1]];
    for (i=0;i<index;i++) start *= inc;
    length = start;
    break;

  case MPITEST_ADD_INC:
    start = array[ 1 + breakpoints[ 2*breakpoint_index+1]];
    inc = array[ 3 + breakpoints[ 2*breakpoint_index+1]];
    for (i=0;i<index;i++) start += inc;
    length = start;
    break;

  case MPITEST_REPEAT:
    length = array[ 1 + breakpoints[ 2*breakpoint_index+1]];
    break;

  default:
    length = element;
    break;
  }

return length;

}


int MPITEST_byte_to_element(int buffer_type, int byte_length)
/****************************************************************
  Return the number of elements of a given type which would
comprise a given number of bytes.  Generate a non-fatal message
if the size of the data type does not divide evenly into the
number of bytes.

  History :

  10/13/95   gm  Created
****************************************************************/
{
  int
    type_size=0,   /* to keep the compiler happy */
    size;
  char info_buf[MPI_MAX_ERROR_STRING];

  switch (buffer_type)
    {
    case MPITEST_int:
      type_size = sizeof(int);
      break;
    case MPITEST_short_int:
      type_size = sizeof(short int);
      break;
    case MPITEST_long:
      type_size = sizeof(long);
      break;
    case MPITEST_unsigned_short:
      type_size = sizeof(unsigned short);
      break;
    case MPITEST_unsigned:
      type_size = sizeof(unsigned);
      break;
    case MPITEST_unsigned_long:
      type_size = sizeof(unsigned long);
      break;
    case MPITEST_float:
      type_size = sizeof(float);
      break;
    case MPITEST_double:
      type_size = sizeof(double);
      break;
    case MPITEST_char:
      type_size = sizeof(signed char);
      break;
    case MPITEST_unsigned_char:
      type_size = sizeof(unsigned char);
      break;
#if MPITEST_longlong_def
    case MPITEST_longlong:
      type_size = sizeof(long long int);
      break;
#endif
#if MPITEST_long_double_def
    case MPITEST_long_double:
      type_size = sizeof(long double);
      break;
#endif
    case MPITEST_byte:
      type_size = sizeof(MPITEST_byte_def);
      break;
    case MPITEST_derived1:
    case MPITEST_derived2:
      type_size = sizeof(derived1);
      break;
    }
  size = byte_length/type_size;

  if ((size * type_size != byte_length) && (MPITEST_me == 0))
    {
      sprintf(info_buf, "Byte length %d does not divide evenly by sizeof type %d.  Using %d elements", byte_length, buffer_type, size);
      MPITEST_message(MPITEST_INFO1, info_buf);
    }
  return size;

}




int MPITEST_num_datatypes(void)
/********************************************************************
  Returns the number of different data types in the default
data-type loop.

  Arguments : none

  Return value : integer number of different datatypes to loop over
as defined in the file include/mpitest_cfg.h .  The default datatypes
are given in the array MPITEST_types[], and this array is a simple
enumeration, so the number of types is simply the number of integers
in that array.

  Error checking :
  This routine checks that all elements of MPITEST_types[] are valid
indices into MPITEST_mpi_datatypes[].  This is done by first setting
num_types to the length of MPITEST_mpi_datatypes[], then making sure
that each element of MPITEST_types[] is less than num_types.  If an
invalid entry is found, a fatal error is triggered.

  History :

  10/13/95   gm  Created
********************************************************************/
{
  int num_mpi_types, i, num_types=0, error=0;

/* MPITEST_datatype_max is defined (in mpitest_cfg.h) to be the largest
   legal index into the array of MPI datatypes. */
  num_mpi_types = MPITEST_datatype_max;

  for (i=0; i < MPITEST_CONFIG_ARRAY_SIZE; i++)
    {
      if (MPITEST_types[i] == MPITEST_END_TOKEN)
	{
	  num_types = i;
	  break;
	}

      if ((MPITEST_types[i] < 0) || (MPITEST_types[i] > num_mpi_types))
	error = 1;
    }


  if (error)
    MPITEST_message(MPITEST_FATAL,"Illegal datatype in array MPITEST_types[]\n");

  return num_types;
}


int MPITEST_get_datatype(int i)
/******************************************************************
  Return the index into the MPITEST_mpi_datatypes[] array of the
datatype for the ith iteration of the datatypes loop.

  Arguments :
     integer i         The iteration for which the datatype
                        is desired.

  Return value : Integer index into the MPITEST_mpi_datatypes[]
array (defined in mpitest_cfg.h) of the datatype appropriate for the
ith loop of the datatype iterator.

  Algorithm : Just return the ith element of MPITEST_types[].

  History :

  10/13/95   gm  Created
******************************************************************/
{
  static int max_i;


/* Set the static variable holding the maximum iteration number
   (and thereby do error checking on the MPITEST_types[] array),
   or check that the iteration parameter is less than the maximum */
  if (i == 0)
    {
      max_i = MPITEST_num_datatypes();
    }
  else if (i >= max_i)
    MPITEST_message(MPITEST_FATAL,"Iteration number parameter too large in MPITEST_get_datatype()\n");

  if (printDatatype) {
    printf( "Returning datatype index %d\n", i ); fflush(stdout);
  }
  return MPITEST_types[ i];

}



int MPITEST_dataTemplate_init(struct dataTemplate *value, int val)
/****************************************************************

  History :

  10/13/95   gm  Created
****************************************************************/
{
  value->Int = val;
  value->ShortInt = (short int) val;
  value->Long = (long int) val;
  value->UnsignedShort = (unsigned short) val;
  value->Unsigned = (unsigned) val;
  value->UnsignedLong = (unsigned long) val;
  value->Float = (float) val;
  value->Double = (double) val;
  value->Char =  (signed char) val;
  value->UnsignedChar = (unsigned char) val;
#if MPITEST_longlong_def
  value->LongLong = (long long int) val;
#endif
#if MPITEST_long_double_def
  value->LongDouble = (long double) val;
#endif
  value->Byte = (MPITEST_byte_def) val;
  return 0;
}



/********************************************************************
********  Communicator functions ***********************************
*******************************************************************/

static int count_elements_increment_index(int *);

static int count_elements_increment_index(int *start_index)
/**********************************************************************
  Subsidiary function for MPITEST_num_comm_sizes() and
MPITEST_get_comm_index().  Counts the number of comms
associated with the current comm_type and increments
the array index to the next element which gives a comm
type.

  Arguments :

  start_index       INPUT/OUTPUT integer * which points to the
                     current index.  On entry, this points
		     to the index in array of the current
		     comm_type specifier.  On exit, points
		     to the index in array of the next comm_type
		     specifier.

  Algorithm :
Steps through array element by element in a while loop.  Uses
a switch to discriminate between MPITEST_comm_inc specifiers
(which cause the array index to jump by 4) and all other numbers
(which are absolute sizes and so just increase the index by 1.)

  Return value :
Sets *start_index to the index of the next comm_type token.
Returns total, which is the running count of the number of
communicators of this type.


  History :

  10/13/95   gm  Created
*********************************************************************/
{
  int
    index = *start_index ,
    total=0,
    *array = MPITEST_comm_sizes,
    element = array[ index];

  index++;  /* array[index] is type */
  element = array[index];

  while((element!=MPITEST_END_TOKEN)&&(element>=MPITEST_comm_size_min))
    {
      total++;
      if (element == MPITEST_comm_inc)
	index +=4;
      else if (element == MPITEST_node_list)
	index += 2 + array[index+1];
      else
	index++;

      element = array[index];
    }

  *start_index = index;
  return total;
}





int MPITEST_num_comm_sizes()
/**************************************************************
  Determine the number of communicators specified by the
configuration array MPITEST_comm_sizes[].  Also
reset elements of the MPITEST_comm_sizes[] array which
are specified as 'particular node' tokens to the meaning of
those tokens in this application.

  Arguments : none

  Algorithm :
Using a while loop, step through the MPITEST_comm_sizes[] array,
(hereafter referred to as array[]), but step through it in such
a way that each step "lands" on a comm_type token each time.
For a given comm_type, count how many communicators of that
type there are.  This counting is done by calling
count_elemnets_increment_index() [see above].
Once the current element becomes MPITEST_END_TOKEN,
return the running total of the number of comms.

  History :

  10/13/95   gm  Created
***************************************************************/
{
  int
    index = 0,
    element,
    total = 0,
    type_is_inc=0,
    type_is_list=0,
    new_element,
    *array = MPITEST_comm_sizes,
    inter = 0;

  char message[80];


  /* replace occurences of 'particular node' tokens and do
     error checking on the elements of MPITEST_comm_sizes[] */

  for (index = 0; (element=array[index])!=MPITEST_END_TOKEN;index++)
    {
      /* we need to know whether we are in an MPITEST_comm_inc
	 size token or not.  If so, then integers
	 refer to node numbers, so MPITEST_nump is
	 not allowed.  If we are not in a comm_inc
	 size token, then integers refer to numbers
	 of nodes, so MPITEST_nump is permissible */
      switch (element)
	{
	case MPITEST_comm_type_self:
	  type_is_inc = 0;
	  type_is_list = 0;
	  break;
	case MPITEST_comm_type_world:
	  type_is_inc = 0;
	  type_is_list = 0;
	  break;
	case MPITEST_comm_type_dup:
	case MPITEST_comm_type_split:
	case MPITEST_comm_type_create:
	case MPITEST_comm_type_inter:
	case MPITEST_comm_type_merge:
	  if (array[index+1]==MPITEST_comm_inc)
	    type_is_inc = 1;
	  else
	    type_is_inc = 0;
	  if (array[index+1]==MPITEST_node_list)
	    type_is_list = 1;
	  else
	    type_is_list = 0;
	  break;

	}


      if (element==MPITEST_comm_last_rank)
	{
	  array[index] = MPITEST_nump-1;
	}
      /* this is never allowed */
      else if (((element > MPITEST_nump) && (type_is_inc != 5)) ||
	       ((element == MPITEST_nump) &&
		((type_is_inc==3) || (type_is_inc==4) || (type_is_list > 3) )))
	{
	  new_element=MPITEST_nump-1;

	  sprintf(message,"Node spec MPITEST_comm_sizes[%d]=%d too large, using %d", index, element, new_element);

	  if (MPITEST_me == 0)
	    MPITEST_message(MPITEST_INFO0, message);
	  array[ index] = new_element;
	}
#if 0
      /* this is allowed as long as the type is not first element of
	 MPITEST_comm_inc */
      else if ((element == MPITEST_nump) &&
		 ((type_is_inc==3) || (type_is_list > 3) ))
	{
	  sprintf(message,"Node spec MPITEST_comm_sizes[%d]=%d is too large, using %d", index, element, MPITEST_nump-1);
	  if (MPITEST_me == 0)
	    MPITEST_message(MPITEST_INFO0, message);
	  array[ index] = MPITEST_nump-1;
	}
#endif

    if (type_is_inc != 0) type_is_inc++;
    if (type_is_list != 0) type_is_list++;
    }

  index = 0;
  while((element=array[ index]) != MPITEST_END_TOKEN)
    {
      switch (element)
	{
	case MPITEST_comm_type_world:
	  total++;
	  index++;
	  break;
	case MPITEST_comm_type_self:
	  total++;
	  index++;
	  break;
	case MPITEST_comm_type_dup:
	  total += count_elements_increment_index(&index);
	  break;
	case MPITEST_comm_type_create:
	  total += count_elements_increment_index(&index);
	  break;
	case MPITEST_comm_type_split:
	  total += count_elements_increment_index(&index);
	  break;
	case MPITEST_comm_type_inter:
	  inter++;
	  total += count_elements_increment_index(&index);
	  total += count_elements_increment_index(&index);
	  break;
	case MPITEST_comm_type_merge:
	  inter++;
	  total += count_elements_increment_index(&index);
	  total += count_elements_increment_index(&index);
	  break;
	default:
	  sprintf(message, "Improperly defined configuration array MPITEST_comm_sizes[] in comm_size : %d", element);
	  MPITEST_message(MPITEST_FATAL, message);
	  break;
	}
    }
  return total-inter;

}

static int find_comm_index(int, int);

static int find_comm_index(int index, int count)
/*****************************************************************
  Given an index into the MPITEST_comm_sizes[] array (herafter
referred to as array[]) which is the index of a communicator type
token in array[], and a count which is the rank among comms
of the type array[index] whose size is desired, return the
index in array[] of the specified communicator.

Arguments :
INPUT int index : the index in MPITEST_comm_sizes[] of the
  current comm_type token.

INPUT int count : the rank among communicators in the current
  incantation of the current comm_type whose size is desired.

Algorithm :
Count through the communicators until we are on the one
specified by count.  Then return its index.

  History :

  10/13/95   gm  Created
******************************************************************/
{
  int
    step = 0,
    size,
    element,
    type,
    *array = MPITEST_comm_sizes;

  /* This is the current comm type token */
  type = array[ index];

  /* go to the first size token */
  index++;
  element = array[ index];

  /* step through comms of this type */
  while (step < count)
    {
      if (element==MPITEST_comm_inc )
	{
	  index += 4;
	}
      else if (element == MPITEST_node_list)
	{
	  size = array[++index];
	  /* now index points to size of this nodelist */
	  index += size+1;
	}
      else
	{
	  index++;
	}
      step++;
      element = array[ index];
    }
  return index;
}


int MPITEST_get_comm_size(int type, int index)
/****************************************************************
  Given a communicator type token and an index into the
MPITEST_comm_sizes[] array, determine the communicator size.

Arguments :
INPUT int type      the communicator type token for the comm
                     in question
INPUT int index     the index into MPITEST_comm_sizes[] of the
                     comm in question

Algorithm :
Use two nested switch statements.  The outer one tests for type.
If the type in MPITEST_comm_type_world the size is set
immediately to MPITEST_nump.  If the size token is
MPITEST_comm_inc, the size is calculated as
(end-start)/inc + 1.  In the case of either MPITEST_comm_type_dup
or MPITEST_comm_type_split, the array element must be
checked for magic number, and this is accomplished by the
inner switch statement.

  History :

  10/13/95   gm  Created
***************************************************************/
{
  int
    start,
    end,
    inc,
    size,
    element;
  int *array = MPITEST_comm_sizes;


/* this switch sets the size using a method which depends on type */
  switch (type)
    {
    case MPITEST_comm_type_world:
      size = MPITEST_nump;
      break;
    case MPITEST_comm_type_self:
      size = 1;
      break;
    default:
      /* for cases MPITEST_comm_type_dup, create and split,
	 must possibly convert a comm size token.  'element' is
	 the token in question */
      element = MPITEST_comm_sizes[ index];
      /* distinguish between comm size tokens and just plain integer sizes */
      switch (element)
	{
	case MPITEST_comm_inc:
	  start = array[index+1];
	  end = array[index +2];
	  inc = array[ index+3];
	  size = 1 + (end-start)/inc;
	  break;
	case MPITEST_node_list:
	  size = array[index+1];
	  break;
	case MPITEST_comm_all:
	  size = MPITEST_nump;
	  break;
	case MPITEST_comm_all_but_one:
	  size = MPITEST_nump-1;
	  break;
	case MPITEST_comm_half_of_all:
	  size = MPITEST_nump/2;
	  break;
	case MPITEST_comm_one:
	  size = 1;
	  break;
	default:  /* this is just an absolute comm size */
	  size = element;
	  break;
	}
      break;
    }

  return size;

}

int MPITEST_get_comm_index(int i)
/********************************************************************
  Determine the index in the MPITEST_comm_sizes[] array of the
communicator for iteration i of the communicator
loop.

Arguments :
INPUT int i     The iteration for which we want the communicator size.

Return value :  The ith communicator's index in MPITEST_comm_sizes[].
Note that if the size token is MPITEST_comm_inc, then the index
returned is that of the 'start' element (i.e. the first element
after the MPITEST_comm_inc token.)

Algorithm :
The MPITEST_comm_sizes[] array (hereafter referred to as "array[]")
consists of a positive number of sequences, each of which consists of
a communicator type token followed by one or more communicator size
tokens.  (See mpitest_cfg.h .)  This function steps through the
communicator type tokens until it gets to the one corresponding to
iteration i.  In order to step between type tokens, calls to the
function count_elements_increment_index(int *index) are made.  [See
the header for that function.] This function returns the number of
communicator size tokens of type indexed by MPITEST_comm_sizes[*index]
which follow directly after MPITEST_comm_sizes[ *index] in
array[].  Upon return, *index is incremented to the next communicator
type token (or MPITEST_END_TOKEN if the current call treated the final
type/size sequence.)

If it is determined that the comm size associated with iteration i
falls under the current type token, then the function
find_comm_index(index, count) is called to determine the index of the
communicator which is the count'th communicator of the current
type.  The return value of that call is then the return
value of MPITEST_get_comm_index().

  History :

  10/13/95   gm  Created
*******************************************************************/
{
  int
    index = 0,
    temp_index = 0,
    iteration = 0,
    element,
    increment,
    the_index = -1,

    *array = MPITEST_comm_sizes;

  static int max_i;

  if (i==0 )
    {
      max_i = MPITEST_num_comm_sizes();
    }
  else if (i >= max_i)
    MPITEST_message(MPITEST_FATAL,"Iteration count too large in MPITEST_get_comm_index()");

  while ((the_index==-1) && (iteration<=i) && ((element=array[ index]) != MPITEST_END_TOKEN))
    {
      switch (element)
	{
	case MPITEST_comm_type_world:
	  if (i == iteration) the_index = index;
	  index++;
	  iteration++;
	  break;
	case MPITEST_comm_type_self:
	  if (i == iteration) the_index = index;
	  index++;
	  iteration++;
	  break;
	case MPITEST_comm_type_dup:
	case MPITEST_comm_type_split:
	case MPITEST_comm_type_create:
	  temp_index = index;
	  increment = count_elements_increment_index(&temp_index);
	  if (i < iteration + increment)
	    the_index = find_comm_index(index, i-iteration);
	  else
	    {
	      iteration += increment;
	      index = temp_index;
	    }
	  break;
	case MPITEST_comm_type_inter :
	case MPITEST_comm_type_merge :
	  if (iteration == i) the_index = index;
	  index++;
	  iteration --;
	  break;

	default:
	  MPITEST_message(MPITEST_FATAL,"Improperly defined configuration array MPITEST_comm_sizes[] in get_index");
	  break;

	}
    }
  return the_index;

}



int MPITEST_get_comm_type(int i)
/********************************************************************
  Determine the communicator type for iteration i of the communicator
loop, as specified in the array MPITEST_comm_sizes[], defined in
mpitest_cfg.h .

Arguments :
INPUT int i     The iteration for which we want the communicator type.

Return value :  The MPITEST communicator type token
corresponding to the ith communicator.

Algorithm :
The MPITEST_comm_sizes[] array (hereafter referred to as "array[]")
consists of a positive number of sequences, each of which consists
of a communicator type token followed by one or more communicator
size tokens.  (See mpitest_cfg.h .)  This function steps through the
communicator type tokens until it gets to the one corresponding to
iteration i.  In order to step between type tokens, calls to the
function count_elements_increment_index(int *index) is called.
[See above.] This function returns the number of communicator size
tokens of type indexed by MPITEST_comm_sizes[ *index] which follow
directly after MPITEST_comm_sizes[ *index] in array[].  Upon
return, *index is incremented to the next communicator type token
(or MPITEST_END_TOKEN if the current call treated the final
type/size sequence.)

If it is determined that the comm size associated with iteration i
falls under the current type token, then the current type token is
returned.

  10/13/95   gm  Created
*******************************************************************/
{
  int
    index = 0,
    temp_index = 0,
    iteration = 0,
    element,
    increment,
    type = 0,
    *array = MPITEST_comm_sizes;

  static int max_i;

  if (i==0 )
    {
      max_i = MPITEST_num_comm_sizes();
    }
  else if (i >= max_i)
    MPITEST_message(MPITEST_FATAL,"Iteration count too large in MPITEST_get_comm_type()");

  while ((type==0) && (iteration<=i) && ((element=array[ index]) != MPITEST_END_TOKEN))
    {
      switch (element)
	{
	case MPITEST_comm_type_world:
	  if (i == iteration) type = MPITEST_comm_type_world;
	  index++;
	  iteration++;
	  break;
	case MPITEST_comm_type_self:
	  if (i == iteration) type = MPITEST_comm_type_self;
	  index++;
	  iteration++;
	  break;
	case MPITEST_comm_type_dup:
	  temp_index = index;
	  increment = count_elements_increment_index(&temp_index);
	  if (i < iteration + increment)
	    type = MPITEST_comm_type_dup;
	  else
	    {
	      iteration += increment;
	      index = temp_index;
	    }
	  break;
	case MPITEST_comm_type_split:
	  temp_index = index;
	  increment = count_elements_increment_index(&temp_index);
	  if (i < iteration + increment)
	    type = MPITEST_comm_type_split;
	  else
	    {
	      iteration += increment;
	      index = temp_index;
	    }
	  break;
	case MPITEST_comm_type_create:
	  temp_index = index;
	  increment = count_elements_increment_index(&temp_index);
	  if (i < iteration + increment)
	    type = MPITEST_comm_type_create;
	  else
	    {
	      iteration += increment;
	      index = temp_index;
	    }
	  break;
	case MPITEST_comm_type_inter:
	  if (i == iteration) type = MPITEST_comm_type_inter;
	  index++;
	  iteration --;
	  break;
	case MPITEST_comm_type_merge:
	  if (i == iteration) type = MPITEST_comm_type_merge;
	  index++;
	  iteration --;
	  break;

	default:
	  MPITEST_message(MPITEST_FATAL,"Improperly defined configuration array MPITEST_comm_sizes[]");
	  break;
	}
    }
  return type;

}


int MPITEST_get_communicator(int context, int index, MPI_Comm *comm)
/********************************************************************
  Get a new communicator (either split, duped, inter, or
trivially, world)

Arguments
  context         INPUT, integer specifying communicator type
                    MPITEST_comm_type_world : MPI_COMM_WORLD
                    MPITEST_comm_type_self :  MPI_COMM_SELF
		    MPITEST_comm_type_dup :   duped comm
		    MPITEST_comm_type_create: new communicator
		    MPITEST_comm_type_split : split comm
		    MPITEST_comm_type_inter : intercommunicator
		    MPITEST_comm_type_merge : merged intercommunicator

  index           INPUT, integer giving index in MPITEST_comm_sizes[]
                   of the beginning of the size token of this comm

  comm            OUTPUT, pointer to new MPI_Comm.  If context is world,
                   *comm is set to MPI_COMM_NULL.  Also, if a node
		   is not a member of the created comm, then the return
		   value for that node is *COMM=MPI_COMM_NULL.

   Return value : integer giving the size of the communicator.

  History :

  10/13/95   gm  Created
   1/15/95   gt  Set MPI_ERRORS_RETURN for the new communicator.

********************************************************************/
{
  int size,
      errsize,
      err,
      rank;
  MPI_Comm comm1;
  MPI_Comm_rank(MPI_COMM_WORLD, &rank);
  char info_buf[MPI_MAX_ERROR_STRING];

  if (printComm) {
    printf( "Creating an intracomm with id %d\n", context ); fflush(stdout);
  }
  switch (context)
    {
    case MPITEST_comm_type_world:   /* MPI_COMM_WORLD */
      if (printComm) {
	printf( "comm world (%d)\n", context ); fflush(stdout);
      }

      *comm = MPI_COMM_WORLD;
      size =  MPITEST_nump;
      MPITEST_current_rank = MPITEST_me;
      MPITEST_inter = MPITEST_NOT_INTER;
      break;

    case MPITEST_comm_type_self:   /* MPI_COMM_SELF */
      if (printComm) {
	printf( "comm_self (%d)\n", context ); fflush(stdout);
      }
      if (MPITEST_me == 0)
	{
	  *comm = MPI_COMM_SELF;
	  MPITEST_current_rank = 0;
	}
      else
	{
	  *comm = MPI_COMM_NULL;
	  MPITEST_current_rank = MPI_UNDEFINED;
	}
      size =  1;
      MPITEST_inter = MPITEST_NOT_INTER;
      break;

    case MPITEST_comm_type_dup:  /* duped comm */
      size = MPITEST_get_duped_communicator(index, comm);
      if (printComm) { 
	printf( "Comm duped of size %d (%d)\n", size, context ); fflush(stdout);
      }
      MPITEST_inter = MPITEST_NOT_INTER;
      break;

    case MPITEST_comm_type_create:  /* create comm */
      size = MPITEST_create_communicator(index, comm, 0);
      if (printComm) {
	printf( "comm create of size %d (%d)\n", size, context ); fflush(stdout);
      }
      MPITEST_inter = MPITEST_NOT_INTER;
      break;

    case MPITEST_comm_type_split:  /* split comm */
      size = MPITEST_create_communicator(index, comm, 1);
      if (printComm) {
	printf( "comm split (%d) of size %d\n", context, size ); fflush(stdout);
      }
      MPITEST_inter = MPITEST_NOT_INTER;
      break;

    case MPITEST_comm_type_inter:  /* intercommunicator */
      if (printComm) {
	printf( "Comm from get_intercommunicator (%d)\n", context ); 
	fflush(stdout);
      }
      size = MPITEST_get_intercommunicator(index, comm);
      break;

    case MPITEST_comm_type_merge:  /* intercommunicator merged */
      if (printComm) {
	printf( "Getting an intercomm to merge (%d)\n", context ); 
	fflush(stdout);
      }
      size = MPITEST_get_intercommunicator(index, &comm1);
       //~ fprintf( stderr, "intercomm size is %d\n", size ); 

      if (comm1 != MPI_COMM_NULL)
	{
	    int s1,s2;
	    MPI_Comm_size( comm1, &s1 );
	    MPI_Comm_remote_size( comm1, &s2 );
	     //~ fprintf( stderr, "Sizeof comm1 = %d,%d for index %d : rank %d\n", s1,s2,index, rank); 
	  err= MPI_Intercomm_merge(comm1, FALSE, comm);
	  if (err != MPI_SUCCESS)
	    {
	      sprintf(info_buf, "Error (%d) from MPI_Intercomm_merge() in MPITEST_get_communicator()", err);
	      MPITEST_message(MPITEST_NONFATAL, info_buf);
	      MPI_Error_string(err, &info_buf[0], &errsize);
	      MPITEST_message(MPITEST_FATAL, info_buf);
	    }
	  err=MPI_Comm_free(&comm1);
	  if (err != MPI_SUCCESS)
	    {
	      sprintf(info_buf, "Error (%d) from MPI_Intercomm_merge() in MPITEST_get_communicator()", err);
	      MPITEST_message(MPITEST_NONFATAL, info_buf);
	      MPI_Error_string(err, &info_buf[0], &errsize);
	      MPITEST_message(MPITEST_FATAL, info_buf);
	    }
          err=MPI_Comm_rank(*comm, &MPITEST_current_rank);
          if (err != MPI_SUCCESS)
	    {
	      sprintf(info_buf, "Error (%d) from MPI_Comm_rank() in MPITEST_get_communicator()", err);
	      MPITEST_message(MPITEST_NONFATAL, info_buf);
	      MPI_Error_string(err, &info_buf[0], &errsize);
	      MPITEST_message(MPITEST_FATAL, info_buf);
	    }
          err=MPI_Comm_size(*comm, &size);
          if (err != MPI_SUCCESS)
	    {
	      sprintf(info_buf, "Error (%d) from MPI_Comm_size() in MPITEST_get_communicator()", err);
	      MPITEST_message(MPITEST_NONFATAL, info_buf);
	      MPI_Error_string(err, &info_buf[0], &errsize);
	      MPITEST_message(MPITEST_FATAL, info_buf);
	    }
	}
      else
	*comm = MPI_COMM_NULL;

      MPITEST_inter = MPITEST_NOT_INTER;

      break;
    }

  if (*comm != MPI_COMM_NULL)
    MPI_Errhandler_set(*comm, MPI_ERRORS_RETURN);
  return size;

}

int MPITEST_get_intercommunicator(int index, MPI_Comm *comm)
/*****************************************************************
  Create a new intercommunicator.  The two intracommunicators
necessary for its construction are listed after the
communicator type token MPITEST_comm_type_inter in the
configuration array MPITEST_comm_sizes[].

Arguments :
  int index            INPUT, the index in the MPITEST_comm_sizes[]
                        array of the MPITEST_comm_type_inter token.

  MPI_Comm *comm       OUTPUT, pointer to the newly created comm.

Return value :
  Integer specifying the size of the local group.  Processes not
belonging to the new comm return 0.
*****************************************************************/
{

  MPI_Comm comm1, comm2, peer_comm;
  MPI_Group group1, group2, group_world;

  int
    i,
    index1,
    index2,
    rank1, rank2,
    remote1, remote2,
    *ranks,
    INTERCOMM_TAG=12365,
    ierr = 0,
    errsize,
    size,
    size2,
    type1,
    type2 = 0,   /* default value to keep the compiler happy */
    temp;
  char
    info_buf[MPI_MAX_ERROR_STRING];

  rank1 = rank2 = -1;
  remote1 = remote2 = -1;

  if (printComm) {
    printf( "Creating intercomm; peer_comm is a dup of MPI_COMM_WORLD\n" );
    fflush(stdout);
  }
  ierr=MPI_Comm_dup(MPI_COMM_WORLD, &peer_comm);
  if (ierr != MPI_SUCCESS)
    {
      sprintf(info_buf, "Error (%d) from MPI_Comm_dup() in MPITEST_get_intercommunicator()", ierr);
      MPITEST_message(MPITEST_NONFATAL, info_buf);
      MPI_Error_string(ierr, &info_buf[0], &errsize);
      MPITEST_message(MPITEST_FATAL, info_buf);
    }

  if ((ranks=(int *)malloc(MPITEST_nump*sizeof(int)))==NULL)
    {
      MPITEST_message(MPITEST_FATAL, "Could not allocate memory for ranks[] in MPITEST_get_intercommunicator()");
    }

  /* find out what type of local communicators we're using */
  index1 = index2 = index+1;
  type1 = MPITEST_comm_sizes[ index1];

  temp = count_elements_increment_index(&index2);

  index1 = find_comm_index(index1, 0);

  /* now index2 points to the next communicator type token.
     If temp = 1, then this token is the second type token,
     if temp = 2, then both local comms are of type1 */
  if (temp == 1 )
    {
      type2 = MPITEST_comm_sizes[ index2];
      index2 = find_comm_index(index2, 0);
    }
  else if (temp == 2)
    {
      type2 = type1;
      index2 = find_comm_index(index, 2);
    }

  ierr = MPI_Comm_group(MPI_COMM_WORLD, &group_world);
  if (ierr != MPI_SUCCESS)
    {
      sprintf(info_buf, "Error (%d) from MPI_Comm_group() in MPITEST_get_intercommunicator()", ierr);
      MPITEST_message(MPITEST_NONFATAL, info_buf);
      MPI_Error_string(ierr, &info_buf[0], &errsize);
      MPITEST_message(MPITEST_FATAL, info_buf);
    }


  MPITEST_get_communicator(type1, index1, &comm1);
  if (comm1 != MPI_COMM_NULL)
    {
      MPI_Comm_group(comm1, &group1);
      MPI_Group_rank(group1, &rank1);
      ierr = MPI_Group_free(&group1);
      if (ierr != MPI_SUCCESS)
        {
          sprintf(info_buf, "MPITEST_get_intercommunicator() had error (%d) in MPI_Group_free()", ierr);
          MPITEST_message(MPITEST_NONFATAL, info_buf);
          MPI_Error_string(ierr, &info_buf[0], &size); 
          MPITEST_message(MPITEST_FATAL, info_buf); 
        }
    }


  MPITEST_get_communicator(type2, index2, &comm2);
  if (comm2 != MPI_COMM_NULL)
    {
      MPI_Comm_group(comm2, &group2);
      MPI_Group_rank(group2, &rank2);
      ierr = MPI_Group_free(&group2);
      if (ierr != MPI_SUCCESS)
        {
          sprintf(info_buf, "MPITEST_get_intercommunicator() had error (%d) in MPI_Group_free()", ierr);
          MPITEST_message(MPITEST_NONFATAL, info_buf);
          MPI_Error_string(ierr, &info_buf[0], &size); 
          MPITEST_message(MPITEST_FATAL, info_buf); 
        }
    }

  if (rank1 == 0)
      remote1 = MPITEST_me;
  else
      remote1 = 0;

  ierr=MPI_Allgather(&remote1, 1, MPI_INT, ranks, 1, MPI_INT, MPI_COMM_WORLD);
  if (ierr != MPI_SUCCESS)
    {
      sprintf(info_buf, "Error (%d) from MPI_Allgather() in MPITEST_get_intercommunicator()", ierr);
      MPITEST_message(MPITEST_NONFATAL, info_buf);
      MPI_Error_string(ierr, &info_buf[0], &errsize);
      MPITEST_message(MPITEST_FATAL, info_buf);
    }

  for (i=0; i< MPITEST_nump; i++)
    if (ranks[i] != 0)
      {
	remote1 = ranks[i];
	break;
      }

  if (rank2 == 0)
      remote2 = MPITEST_me;
  else
      remote2 = 0;

  MPI_Allgather(&remote2, 1, MPI_INT, ranks, 1, MPI_INT, MPI_COMM_WORLD);
  if (ierr != MPI_SUCCESS)
    {
      sprintf(info_buf, "Error (%d) from MPI_Allgather() in MPITEST_get_intercommunicator()", ierr);
      MPITEST_message(MPITEST_NONFATAL, info_buf);
      MPI_Error_string(ierr, &info_buf[0], &errsize);
      MPITEST_message(MPITEST_FATAL, info_buf);
    }


  for (i=0; i<MPITEST_nump; i++)
    if (ranks[i] != 0)
      {
	remote2 = ranks[i];
	break;
      }

  if (printComm) {
    printf( "Creating an intercommunicator\n" ); fflush(stdout);
  }
  if (comm1 != MPI_COMM_NULL)
    {
      if (printComm) {
	printf( "comm1 (%d) not null, 0 to %d\n", index1, remote2 ); 
	fflush(stdout);
      }
      ierr = MPI_Intercomm_create(comm1, 0, peer_comm,
			   remote2, INTERCOMM_TAG, comm);
      if (ierr != MPI_SUCCESS) {
          MPITEST_message(MPITEST_NONFATAL, info_buf);
          MPI_Error_string(ierr, &info_buf[0], &size); 
	  printf( "%s\n", &info_buf[0] ); fflush(stdout);
          MPITEST_message(MPITEST_FATAL, info_buf); 
      }
      MPI_Comm_remote_size(*comm, &size);
      MPI_Comm_size(*comm, &size2);
      MPI_Comm_rank(*comm, &MPITEST_current_rank);
      MPI_Comm_free(&comm1);
      MPITEST_inter = 0;
    }
  else if (comm2 != MPI_COMM_NULL)
    {
      if (printComm) {
	printf( "comm2 (%d) not null, 0 to %d\n", index2, remote1 ); 
	fflush(stdout);
      }
      ierr = MPI_Intercomm_create(comm2, 0, peer_comm,
			   remote1, INTERCOMM_TAG, comm);
      if (ierr != MPI_SUCCESS) {
          MPITEST_message(MPITEST_NONFATAL, info_buf);
          MPI_Error_string(ierr, &info_buf[0], &size); 
	  printf( "%s\n", &info_buf[0] ); fflush(stdout);
          MPITEST_message(MPITEST_FATAL, info_buf); 
      }
      MPI_Comm_remote_size(*comm, &size);
      MPI_Comm_size(*comm, &size2);
      MPI_Comm_rank(*comm, &MPITEST_current_rank);
      MPI_Comm_free(&comm2);
      MPITEST_inter = 1;
    }
  else
    {
      size = 0;
      size2 = 0;
      *comm = MPI_COMM_NULL;
      MPITEST_current_rank = MPI_UNDEFINED;
      MPITEST_inter = MPI_UNDEFINED;
    }

  if (printComm) {
    printf( "MPI_Intercomm_create complete, about to allreduce\n" );
    fflush(stdout);
  }
  size = size + size2;
  fflush(stdout);
  MPI_Allgather(&size, 1, MPI_INT, ranks, 1, MPI_INT, MPI_COMM_WORLD);
  fflush(stdout);
  if (ierr != MPI_SUCCESS)
    {
      sprintf(info_buf, "Error (%d) from MPI_Allgather() in MPITEST_get_intercommunicator()", ierr);
      MPITEST_message(MPITEST_NONFATAL, info_buf);
      MPI_Error_string(ierr, &info_buf[0], &errsize);
      MPITEST_message(MPITEST_FATAL, info_buf);
    }
  for (i=0; i<MPITEST_nump; i++)
    if (ranks[i] != 0)
      {
	size = ranks[i];
	break;
      }

  MPI_Comm_free(&peer_comm);
  if (ierr != MPI_SUCCESS)
    {
      sprintf(info_buf, "Error (%d) from MPI_Intercomm_create() in MPITEST_get_intercommunicator()", ierr);
      MPITEST_message(MPITEST_NONFATAL, info_buf);
      MPI_Error_string(ierr, &info_buf[0], &errsize);
      MPITEST_message(MPITEST_FATAL, info_buf);
    }

  ierr = MPI_Group_free(&group_world);
  if (ierr != MPI_SUCCESS)
    {
      sprintf(info_buf, "MPITEST_get_intercommunicator() had error (%d) in MPI_Group_free()", ierr);
      MPITEST_message(MPITEST_NONFATAL, info_buf);
      MPI_Error_string(ierr, &info_buf[0], &size); 
      MPITEST_message(MPITEST_FATAL, info_buf); 
    }
  free(ranks);

  if (printComm) {
    printf( "Returning an intercomm\n" ); fflush(stdout); 
  }
  return size;
}



int MPITEST_create_communicator(int index, MPI_Comm *comm, int split)
/********************************************************************
  Create a new communicator.

  Arguments :
    index                INPUT, integer index into MPITEST_comm_sizes[]
                          of beginning of desired comm's size token

    comm                 OUTPUT, pointer to the new communicator

  Return value :  The integer size of the new communicator.

  This function allocates an integer ranks array, then sets
  ranks[i] for 0 <= i < comm size.  The method of setting the
  ranks array (and hence the membership in the new communicator)
  is determined by the "size token" that is found at
  MPITEST_comm_sizes[index].

  If the size token is a positive integer, then that is an absolute
  size, and ranks[i] is set to i, for 0 <= i < comm_size.  This
  results in the (global) ranks {0, 1, ..., comm_size-1} being
  in the new communicator.

  If the size token is MPITEST_comm_inc, then the participating
  ranks are (start, start+inc, start+2*inc, ...).  'start', 'end'
  and 'inc' are read from the MPITEST_comm_sizes[] array, in the
  three positions following the size token.

  If the size token is MPITEST_node_list, then the ranks are explicitly
  listed in MPITEST_comm_sizes[], and these numbers are simply copied
  into the ranks array.  In this case, the array element immediately
  following the size token is the communicator size (N, say) , and the
  next N elements are the global ranks of the participating processes.


  The new communicator is allocated with a call to MPI_Comm_create().

  History :

  10/13/95   gm  Created

********************************************************************/
{
  int *ranks, i, err, comm_size, color;
  MPI_Group old_group, new_group;
  int *array = MPITEST_comm_sizes;
  char info_buf[MPI_MAX_ERROR_STRING];
  int size;

  /* get the comm_size */
  comm_size = MPITEST_get_comm_size(MPITEST_comm_type_create, index);


  err = MPI_Comm_group(MPI_COMM_WORLD, &old_group);
  if (err != MPI_SUCCESS)
    {
      sprintf(info_buf,"MPITEST_create_communicator() had error (%d) in MPI_Comm_group()", err);
      MPITEST_message(MPITEST_NONFATAL, info_buf); 
      MPI_Error_string(err, &info_buf[0], &size); 
      MPITEST_message(MPITEST_FATAL, info_buf);
    }


  if ((ranks=(int *)malloc(MPITEST_nump*sizeof(int)))==NULL)
    {
      MPITEST_message(MPITEST_FATAL,"No memory for ranks in MPITEST_create_communicator\n");
    }

  for (i=0; i< comm_size; i++)
    {
    switch (array[index])
      {
      case MPITEST_comm_inc:
	ranks[i] = array[index+1]+i*array[index+3];
	break;
      case MPITEST_node_list:
	ranks[i] = array[index+2+i];
	break;
      default:
	ranks[i] = i;
	break;
      }
    }
  err = MPI_Group_incl(old_group, comm_size, ranks, &new_group);
  if (err != MPI_SUCCESS)
    {
      sprintf(info_buf,"MPITEST_create_communicator() had error (%d) in MPI_Group_incl()", err);
      MPITEST_message(MPITEST_NONFATAL, info_buf);
      MPI_Error_string(err, &info_buf[0], &size);
      MPITEST_message(MPITEST_FATAL, info_buf);
    }
/* set the new current rank */
  err = MPI_Group_rank(new_group, &MPITEST_current_rank);
  if (err != MPI_SUCCESS)
    {
      sprintf(info_buf, "MPITEST_create_communicator() had error (%d) in MPI_Group_rank()", err);
      MPITEST_message(MPITEST_NONFATAL, info_buf);
      MPI_Error_string(err, &info_buf[0], &size);
      MPITEST_message(MPITEST_FATAL, info_buf);
    }

  if (split == 0)
    {
      err = MPI_Comm_create(MPI_COMM_WORLD, new_group, comm);
      if (err != MPI_SUCCESS)
        {
          sprintf(info_buf, "MPITEST_create_communicator() had error (%d) in MPI_Comm_create()", err);
          MPITEST_message(MPITEST_NONFATAL, info_buf);
          MPI_Error_string(err, &info_buf[0], &size);
          MPITEST_message(MPITEST_FATAL, info_buf);
        }
    }
  else
    {
      if (MPITEST_current_rank != MPI_UNDEFINED)
	{
	  color=0;
	}
      else
	{
	  color=1;
	  comm_size = MPITEST_nump - comm_size;
	}
      err = MPI_Comm_split(MPI_COMM_WORLD, color, MPITEST_me, comm);
      if (err != MPI_SUCCESS)
        {
          sprintf(info_buf, "MPITEST_create_communicator() had error (%d) in MPI_Comm_split()", err);
          MPITEST_message(MPITEST_NONFATAL, info_buf);
          MPI_Error_string(err, &info_buf[0], &size);
          MPITEST_message(MPITEST_FATAL, info_buf);
	}
      err=MPI_Comm_rank(*comm, &MPITEST_current_rank);
      if (err != MPI_SUCCESS)
        {
          sprintf(info_buf, "MPITEST_create_communicator() had error (%d) in MPI_Comm_rank()", err);
          MPITEST_message(MPITEST_NONFATAL, info_buf);
          MPI_Error_string(err, &info_buf[0], &size);
          MPITEST_message(MPITEST_FATAL, info_buf);
	}
      
    }

  free(ranks);
  err = MPI_Group_free(&old_group);
  if (err != MPI_SUCCESS)
    {
      sprintf(info_buf, "MPITEST_create_communicator() had error (%d) in MPI_Group_free()", err);
      MPITEST_message(MPITEST_NONFATAL, info_buf);
      MPI_Error_string(err, &info_buf[0], &size); 
      MPITEST_message(MPITEST_FATAL, info_buf); 
    }
  err = MPI_Group_free(&new_group);
  if (err != MPI_SUCCESS)
     {
      sprintf(info_buf, "MPITEST_create_communicator() had error (%d) in MPI_Group_free()", err);
      MPITEST_message(MPITEST_NONFATAL, info_buf);
      MPI_Error_string(err, &info_buf[0], &size); 
      MPITEST_message(MPITEST_FATAL, info_buf); 
    }
  return comm_size;

}





int MPITEST_get_duped_communicator(int index, MPI_Comm *comm)
/********************************************************************
  Get a duplicated communicator.

  Arguments :
    index               INPUT, integer giving the index into
                         MPITEST_comm_sizes[] of the beginning
			 of the current comm's size token

    comm                OUTPUT, pointer to the new communicator

  Return value :  The integer size of the new communicator.

  This function first gets a split communicator of the size
  communicator_size, then duplicates it with a call to MPI_Comm_dup().

  History :

  10/13/95   gm  Created
********************************************************************/
{
  MPI_Comm temp_comm;
  int comm_size, err;
  char info_buf[MPI_MAX_ERROR_STRING]; 
  int size;

  comm_size = MPITEST_create_communicator(index, &temp_comm, 0);

  if (MPITEST_current_rank != MPI_UNDEFINED)
    {
      err = MPI_Comm_dup(temp_comm, comm);
      if (err != MPI_SUCCESS)
        {
	  sprintf(info_buf, "MPITEST_get_duped_communicator() had error (%d) in MPI_Comm_dup()", err);
	  MPITEST_message(MPITEST_NONFATAL, info_buf); 
	  MPI_Error_string(err, &info_buf[0], &size); 
	  MPITEST_message(MPITEST_FATAL, info_buf);
        }
      err = MPI_Comm_free(&temp_comm);
      if (err != MPI_SUCCESS)
        {
          sprintf(info_buf, "MPITEST_get_duped_communicator() had error (%d) in MPI_Comm_free()", err);
          MPITEST_message(MPITEST_NONFATAL, info_buf); 
          MPI_Error_string(err, &info_buf[0], &size);  
          MPITEST_message(MPITEST_FATAL, info_buf);
        }
    }
  else *comm = MPI_COMM_NULL;

  return comm_size;
}





int MPITEST_free_communicator (int comm_type, MPI_Comm *comm)
/********************************************************************
  Free an MPI communicator.

  Arguments :
  int comm_type           The communicator type, as defined in mpitest_cfg.h

  MPI_Comm *comm          The communicator to be freed.

  If comm_type = MPITEST_comm_type_world (i.e. *comm=MPI_COMM_WORLD),
  then it is not freed.  For the other type of communicators, comm
  is only freed if it is not MPI_COMM_NULL.

  History :

  10/13/95   gm  Created
********************************************************************/
{
  int err=MPI_SUCCESS;
  char info_buf[MPI_MAX_ERROR_STRING]; 
  int size;

  switch(comm_type)
    {
    case MPITEST_comm_type_world:   /* MPI_COMM_WORLD */
      *comm = MPI_COMM_NULL;
      break;

    case MPITEST_comm_type_self:   /* MPI_COMM_SELF */
      *comm = MPI_COMM_NULL;
      break;

    case MPITEST_comm_type_split:  /* split comm */
    case MPITEST_comm_type_create:  /* split comm */
    case MPITEST_comm_type_inter:  /* inter comm */
    case MPITEST_comm_type_dup:  /* duped comm */
    case MPITEST_comm_type_merge:  /* mergeed comm */
      if (*comm != MPI_COMM_NULL)
	{
	  err = MPI_Comm_free(comm);
	  if (err != MPI_SUCCESS)
	    {
	      sprintf(info_buf, "MPITEST_free_communicator() had error (%d) in MPI_Comm_free()", err);
	      MPITEST_message(MPITEST_NONFATAL, info_buf); 
	      MPI_Error_string(err, &info_buf[0], &size);  
	      MPITEST_message(MPITEST_FATAL, info_buf); 
	    }
	}
      break;

    }

  return 0;

}

int MPITEST_datatype_has_sticky_lb(MPI_Datatype type, MPI_Aint *lb_off_p)
/***********************************************************************
  Test to see if a datatype has a sticky LB, and if so return the offset

  Arguments:
  MPI_Datatype type       MPI datatype to test for presence of sticky LB
  MPI_Aint *lb_off_p      pointer to region to store sticky LB value if
                          there is one.

  Return value: 0 if no sticky LB, -1 on error, 1 otherwise.  If there is
                a sticky LB, its offset in the type is returned in the
                region pointed to by lb_off_p.

  We grab the LB of the type as is, then we explicitly set a sticky LB at
  an offset one above this value.  If there is a sticky LB on the type
  already, then the LB of the resulting type will be identical to the
  original type; otherwise we know that there is no sticky LB on the type.

  History:

  11/11/2002   Rob Ross (ANL) Created to fix broken LB tests (they make
               poor assumptions about the types that MPITEST_get_datatypes
	       will return, assuming that they do not have sticky LBs).
************************************************************************/
{
  MPI_Datatype tmptype;
  MPI_Aint orig_lb, new_lb;
  int err, blks[2] = { 1, 1 };
  MPI_Aint disps[2];
  MPI_Datatype types[2];

  err = MPI_Type_lb(type, &orig_lb);
  if (err != MPI_SUCCESS) return -1;

  types[0] = MPI_LB;
  types[1] = type;
  disps[0] = orig_lb + 1;
  disps[1] = 0;

  err = MPI_Type_struct(2, blks, disps, types, &tmptype);
  if (err != MPI_SUCCESS) return -1;

  err = MPI_Type_lb(tmptype, &new_lb);
  MPI_Type_free(&tmptype);
  if (err != MPI_SUCCESS) return -1;
  if (new_lb == orig_lb) {
      /* there is a sticky LB */
      *lb_off_p = orig_lb;
      return 1;
  }
  else return 0;
}


int MPITEST_datatype_has_sticky_ub(MPI_Datatype type, MPI_Aint *ub_off_p)
/***********************************************************************
  Test to see if a datatype has a sticky UB, and if so return the offset

  Arguments:
  MPI_Datatype type       MPI datatype to test for presence of sticky UB
  MPI_Aint *ub_off_p      pointer to region to store sticky UB value if
                          there is one.

  Return value: 0 if no sticky UB, -1 on error, 1 otherwise.  If there is
                a sticky UB, its offset in the type is returned in the
                region pointed to by ub_off_p.

  We grab the UB of the type as is, then we explicitly set a sticky UB at
  an offset one below this value.  If there is a sticky UB on the type
  already, then the UB of the resulting type will be identical to the
  original type; otherwise we know that there is no sticky UB on the type.

  History:

  11/11/2002   Rob Ross (ANL) Created to fix broken UB tests (they make
               poor assumptions about the types that MPITEST_get_datatypes
	       will return, assuming that they do not have sticky UBs).
************************************************************************/
{
  MPI_Datatype tmptype;
  MPI_Aint orig_ub, new_ub;
  int err, blks[2] = { 1, 1 };
  MPI_Aint disps[2];
  MPI_Datatype types[2];

  err = MPI_Type_ub(type, &orig_ub);
  if (err != MPI_SUCCESS) return -1;

  types[0] = MPI_UB;
  types[1] = type;
  disps[0] = orig_ub - 1;
  disps[1] = 0;

  err = MPI_Type_struct(2, blks, disps, types, &tmptype);
  if (err != MPI_SUCCESS) return -1;
    
  err = MPI_Type_ub(tmptype, &new_ub);
  MPI_Type_free(&tmptype);
  if (err != MPI_SUCCESS) return -1;
  if (new_ub == orig_ub) {
      /* there is a sticky UB */
      *ub_off_p = orig_ub;
      return 1;
  }
  else return 0;
}






