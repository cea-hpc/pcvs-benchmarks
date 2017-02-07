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


int set_default_ops(MPI_Op *op_array)
/**********************************************************************
Sets up the default operation array.  Returns the number of default 
operations.
***********************************************************************/
{
  op_array[ 0] =  MPI_MAXLOC;
  op_array[ 1] =  MPI_MINLOC;
  
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
    return 1;
    break;


  case MPITEST_unsigned_short:
  case MPITEST_unsigned:
  case MPITEST_unsigned_long:
  case MPITEST_char:
  case MPITEST_unsigned_char:
  case MPITEST_byte:
    return 0;
    break;


  case MPITEST_float:
  case MPITEST_double:
#if MPITEST_long_double_def
  case MPITEST_long_double:
#endif
    /* only the first four operations (min, max, sum, product) apply
     to float and double */
    if (op < 4)
      return 1;
    else
      return 0;

  default:
      return 0;
  }
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
    case 0:  /* maximum */
      value = (x1 > x2 ? x1 : x2);
      break;
    case 1: /* minimum */
      value = (x1 < x2 ? x1 : x2);
      break;
    }
  return value;
}


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
    case 0:  /* maximum */
      value = (x1 > x2 ? x1 : x2);
      break;
    case 1: /* minimum */
      value = (x1 < x2 ? x1 : x2);
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
    case 0:  /* maximum */
      value = (x1 > x2 ? x1 : x2);
      break;
    case 1: /* minimum */
      value = (x1 < x2 ? x1 : x2);
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
  long x1 = 0, x2 = 1, ianswer;
  double dx1 = 0.0, dx2 = 1.0, danswer;
#if MPITEST_long_double_def
  long double ldx1 = 0.0, ldx2 = 1.0, ldanswer;
#endif


  if (nump==1) 
    {
      MPITEST_dataTemplate_init(answer, 0);
      return 0;
    }
  
  ianswer = apply_int_op(op_index, x1, x2);

  for (x2 = 2; x2 < nump; x2++)
    {
      ianswer = apply_int_op(op_index, ianswer, x2);
    }
  
  MPITEST_dataTemplate_init(answer, ianswer);
  
  /* now take care of the real datatypes */
  danswer = apply_double_op(op_index, dx1, dx2);
  for (dx2 = 2.0, x2=2 ; x2 < nump; x2++, dx2+=1.0)
    {
       danswer = apply_double_op(op_index, danswer, dx2);
    }
  answer->Float = (float) danswer;
  answer->Double = danswer;
#if MPITEST_long_double_def
  ldanswer = apply_long_double_op(op_index, ldx1, ldx2);
  for (ldx2 = 2.0, x2=2 ; x2 < nump; x2++, ldx2+=1.0)
    {
      ldanswer = apply_long_double_op(op_index, ldanswer, ldx2);
    }
  answer->LongDouble = ldanswer;
#endif

  return 0;
}
