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
#include "mpitest_def.h"
#include "mpitest.h"
#include "reduce.h"


/*************************************************************************
  Special functions and definitions for the reduce family of functions.
*************************************************************************/

void addop(void *invec, void *inoutvec, int *len, MPI_Datatype *dtype)
/*************************************************************************
  Commutative user Op (addition)
*************************************************************************/
{
  int i;
  for (i=0; i<*len; i++ )
   {
	  if (*dtype == MPI_INT)
	    ((int *)inoutvec)[i] += ((int *)invec)[i];
	  else if (*dtype == MPI_SHORT)
	    ((short int *)inoutvec)[i] += (short int)((short int *)invec)[i];
	  else if (*dtype == MPI_LONG)
	    ((long *)inoutvec)[i] += ((long *)invec)[i];
	  else if (*dtype == MPI_UNSIGNED_SHORT)
	    ((unsigned short *)inoutvec)[i] += ((unsigned short *)invec)[i];
	  else if (*dtype == MPI_UNSIGNED)
	    ((unsigned *)inoutvec)[i] += ((unsigned *)invec)[i];
	  else if (*dtype == MPI_UNSIGNED_LONG)
	    ((unsigned long *)inoutvec)[i] += ((unsigned long *)invec)[i];
	  else if (*dtype == MPI_FLOAT)
	    ((float *)inoutvec)[i] += ((float *)invec)[i];
	  else if (*dtype == MPI_DOUBLE)
	    ((double *)inoutvec)[i] += ((double *)invec)[i];
	  else if (*dtype == MPI_CHAR)
	    ((char *)inoutvec)[i] += ((char *)invec)[i];
	  else if (*dtype == MPI_UNSIGNED_CHAR)
	    ((unsigned char *)inoutvec)[i] += ((unsigned char *)invec)[i];
#if MPITEST_longlong_def
	  else if (*dtype == MPI_LONG_LONG_INT)
	    ((long long int *)inoutvec)[i] += ((long long int *)invec)[i];
#endif
#if MPITEST_long_double_def
	  else if (*dtype == MPI_LONG_DOUBLE)
	    ((long double *)inoutvec)[i] += ((long double *)invec)[i];
#endif
	  else ((short int *)inoutvec)[i] = -1;
	  }
}

void incop(void *invec, void *inoutvec, int *len, MPI_Datatype *dtype)
/*************************************************************************
  Non-commutative user Op.
*************************************************************************/
{
  int i;
  for (i=0; i<*len; i++ )  {
	if (*dtype == MPI_INT)
	  {
	    if ( (int)((int *)inoutvec)[i] > (int)((int *)invec)[i] )
	      ((int *)inoutvec)[i] = ((int *)invec)[i];
	    else
	      ((int *)inoutvec)[i] = (int)(MPITEST_nump+2);
	  }
	else if (*dtype == MPI_SHORT)
	  {
	    if ( ((short int *)inoutvec)[i] > ((short int *)invec)[i] )
	      ((short int *)inoutvec)[i] = ((short int *)invec)[i];
	    else
	      ((short int *)inoutvec)[i] = (short int)(MPITEST_nump+2);
	  }
	else if (*dtype == MPI_LONG)
	  {
	    if ( ((long *)inoutvec)[i] > ((long *)invec)[i] )
	      ((long *)inoutvec)[i] = ((long *)invec)[i];
	    else
	      ((long *)inoutvec)[i] = (long)(MPITEST_nump+2);
	  }
	else if (*dtype == MPI_UNSIGNED_SHORT)
	  {
	    if ( ((unsigned short *)inoutvec)[i] > ((unsigned short *)invec)[i] )
	      ((unsigned short *)inoutvec)[i] = ((unsigned short *)invec)[i];
	    else
	      ((unsigned short *)inoutvec)[i] = (unsigned char)(MPITEST_nump+2);
	  }
	else if (*dtype == MPI_UNSIGNED)
	  {
	    if ( ((unsigned *)inoutvec)[i] > ((unsigned *)invec)[i] )
	      	((unsigned *)inoutvec)[i] = ((unsigned *)invec)[i];
	    else
	      ((unsigned *)inoutvec)[i] = (unsigned)(MPITEST_nump+2);
	  }
	else if (*dtype == MPI_UNSIGNED_LONG)
	  {
	    if ( ((unsigned long *)inoutvec)[i] > ((unsigned long *)invec)[i] )
	      ((unsigned long *)inoutvec)[i] = ((unsigned long *)invec)[i];
	    else
	      ((unsigned long *)inoutvec)[i] = (unsigned long)(MPITEST_nump+2);
	  }
	else if (*dtype == MPI_FLOAT)
	  {
	    if ( ((float *)inoutvec)[i] > ((float *)invec)[i] )
	      ((float *)inoutvec)[i] = ((float *)invec)[i];
	    else
	      ((float *)inoutvec)[i] = (float)(MPITEST_nump+2);
	  }
	else if (*dtype == MPI_DOUBLE)
	  {
	    if ( ((double *)inoutvec)[i] > ((double *)invec)[i] )
	      ((double *)inoutvec)[i] = ((double *)invec)[i];
	    else
	      ((double *)inoutvec)[i] = (double)(MPITEST_nump+2);
	  }
	else if (*dtype == MPI_CHAR)
	  {
	    if ( ((char *)inoutvec)[i] > ((char *)invec)[i] )
	      ((char *)inoutvec)[i] = ((char *)invec)[i];
	    else
	      ((char *)inoutvec)[i] = (char)(MPITEST_nump+2);
	  }
	else if (*dtype == MPI_UNSIGNED_CHAR)
	  {
	    if ( ((unsigned char *)inoutvec)[i] > ((unsigned char *)invec)[i] )
	      ((unsigned char *)inoutvec)[i] = ((unsigned char *)invec)[i];
	    else
	      ((unsigned char *)inoutvec)[i] = (unsigned char)(MPITEST_nump+2);
	  }
#if MPITEST_longlong_def
	else if (*dtype == MPI_LONG_LONG_INT)
	  {
	    if ( ((long long int *)inoutvec)[i] > ((long long int *)invec)[i] )
	      ((long long int *)inoutvec)[i] = ((long long int *)invec)[i];
	    else
	      ((long long int *)inoutvec)[i] = (long long int)(MPITEST_nump+2);
	  }
#endif
#if MPITEST_long_double_def
	else if (*dtype == MPI_LONG_DOUBLE)
	  {
	    if ( ((long double *)inoutvec)[i] > ((long double *)invec)[i] )
	      ((long double *)inoutvec)[i] = ((long double *)invec)[i];
	    else
	      ((long double *)inoutvec)[i] = (long double)(MPITEST_nump+2);
	  }
#endif
	else ((short int *)inoutvec)[i] = -1;
  }
}



int set_default_ops(MPI_Op *op_array)
/**********************************************************************
Sets up the default operation array.  Returns the number of operations.
***********************************************************************/
{
int  ierr,
     size;
char info_buf[256];
char error_string[MPI_MAX_ERROR_STRING];

  ierr = MPI_Op_create((MPI_User_function *)addop, TRUE, &op_array[0] );
  if (ierr!=MPI_SUCCESS)
    {
      sprintf(info_buf, "Non-zero return code (%d) from MPI_Op_create()", ierr);
      MPITEST_message(MPITEST_NONFATAL, info_buf);
      MPI_Error_string(ierr, error_string, &size);
      MPITEST_message(MPITEST_FATAL, error_string);
    }

  ierr = MPI_Op_create((MPI_User_function *)incop, FALSE, &op_array[1] );
  if (ierr!=MPI_SUCCESS)
    {
      sprintf(info_buf, "Non-zero return code (%d) from MPI_Op_create()", ierr);
      MPITEST_message(MPITEST_NONFATAL, info_buf);
      MPI_Error_string(ierr, error_string, &size);
      MPITEST_message(MPITEST_FATAL, error_string);
    }


  return 2;
}

int has_op(int op, int test_type)
/***********************************************************************
Determines whether a particular operation may be applied to a particular
data type, as specified in section 4.9.2 of the MPI Standard.
*************************************************************************/
{
  switch(test_type) {

  case MPITEST_int:
  case MPITEST_short_int:
  case MPITEST_long:
  case MPITEST_unsigned_short:
  case MPITEST_unsigned:
  case MPITEST_unsigned_long:
#if MPITEST_longlong_def
  case MPITEST_longlong:
#endif
  case MPITEST_char:
  case MPITEST_unsigned_char:
  case MPITEST_float:
  case MPITEST_double:
#if MPITEST_long_double_def
  case MPITEST_long_double:
#endif
    return 1;

  case MPITEST_byte:
    return 0;

  default:
      return 0;

  }

return 0;

}



long apply_int_op(int op_index, long x1, long x2)
/***************************************************************************
Applies a binary operator to the two integers x1 and x2, returning the
result.  The binary operation is determined by the integer op_index.  The
mapping of op_index to operation is determined by the array
MPITEST_default_ops[], which is set at runtime in the main test code.
**************************************************************************/
{
  long value = 0;
  switch (op_index)
    {
    case 0: /* addop */
      value = x1 + x2;
      break;
    case 1: /* comop */
      value = 1;
      break;
    }
  return value;
}


#if MPITEST_longlong_def
long long int apply_longlong_op(int op_index, long long int x1, long long int x2)
/***************************************************************************
Applies a binary operator to the two integers x1 and x2, returning the
result.  The binary operation is determined by the integer op_index.  The
mapping of op_index to operation is determined by the array
MPITEST_default_ops[], which is set at runtime in the main test code.
**************************************************************************/
{
  long long int value = 0;
  switch (op_index)
    {
    case 0: /* addop */
      value = x1 + x2;
      break;
    case 1: /* comop */
      value = 1.;
      break;
    }
  return value;
}
#endif


double apply_double_op(int op_index, double x1, double x2)
/***************************************************************************
Applies a binary operator to the two doubles x1 and x2, returning the
result.  The binary operation is determined by the integer op_index.  The
mapping of op_index to operation is determined by the array
MPITEST_default_ops[], which is set at runtime in the main test code.
**************************************************************************/
{
  double value = 0;
  switch(op_index)
    {
    case 0: /* addop */
      value = x1 + x2;
      break;
    case 1: /* comop */
      value = 1.;
      break;
    }
  return value;
}


#if MPITEST_long_double_def
long double apply_long_double_op(int op_index, long double x1, long double x2)
/***************************************************************************
Applies a binary operator to the two long doubles x1 and x2, returning the
result.  The binary operation is determined by the integer op_index.  The
mapping of op_index to operation is determined by the array
MPITEST_default_ops[], which is set at runtime in the main test code.
**************************************************************************/
{
  long double value = 0;
  switch(op_index)
    {
    case 0: /* addop */
      value = x1 + x2;
      break;
    case 1: /* comop */
      value = 1.;
      break;
    }
  return value;
}
#endif



int get_reduce_answer(int op_index, int nump, struct dataTemplate *answer)
/************************************************************************
Apply the binary operation specified by op_index to the numbers
(0, 1, 2, ..., nump-1), and fill in the dataTamplate object based on the
results.  The mapping of op_index to operation is determined by the array
MPITEST_default_ops[], which is set at runtime in the main test code.
In order for the answer produced by this routine to match the
answer generated by the MPI_Reduce() operation in the test code, the
send buffer of process with rank "i" must have been initialized with "i".

This routine applies the operation to both integers and to doubles,
in case the double and float buffers are initialized differently than the
integer buffers.
***********************************************************************/
{
  long x1 = 1, x2 = 2, ianswer;
#if MPITEST_longlong_def
  long long int lx1 = 1, lx2 = 2, lanswer;
#endif
  double dx1 = 1.0, dx2 = 2.0, danswer;
#if MPITEST_long_double_def
  long double ldx1 = 1.0, ldx2 = 2.0, ldanswer;
#endif

  if (nump==1)
    {
      MPITEST_dataTemplate_init(answer, 1);
      return 0;
    }

  ianswer = apply_int_op(op_index, x1, x2);

  for (x1=3, x2=3; x2 <= nump; x1++, x2++)
    {
      if ((x1 > 2) && (op_index == 3)) x1=1;
      ianswer = apply_int_op(op_index, ianswer, x1);
    }

  MPITEST_dataTemplate_init(answer, ianswer);

#if MPITEST_longlong_def
  lanswer = apply_longlong_op(op_index, lx1, lx2);
  for (lx2=3, x2=3 ; x2 <= nump; x2++, lx2+=1)
    {
      if ((lx2 > 2) && (op_index == 3)) lx2=1;
      lanswer = apply_longlong_op(op_index, lanswer, lx2);
    }
  answer->LongLong = lanswer;
#endif

  /* now take care of the real datatypes */
  danswer = apply_double_op(op_index, dx1, dx2);
  for (dx2=3.0, x2=3 ; x2 <= nump; x2++, dx2+=1.0)
    {
      danswer = apply_double_op(op_index, danswer, dx2);
    }
  answer->Float = (float) danswer;
  answer->Double = danswer;

#if MPITEST_long_double_def
  ldanswer = apply_long_double_op(op_index, ldx1, ldx2);
  for (ldx2 = 3.0, x2=3 ; x2 <= nump; x2 ++, ldx2+=1.0)
    {
      ldanswer = apply_long_double_op(op_index, ldanswer, ldx2);
    }
  answer->LongDouble = ldanswer;
#endif

  return 0;

}

