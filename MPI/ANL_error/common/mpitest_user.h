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
/*  Include file to be included in user codes (i.e. test codes) but not 
    in library functions */

/* Include the main mpitest include file */
#include "mpitest.h"

/* This array may be indexed to pull out an MPI_Datatype
   for use in MPI calls.  The indices are given symbolic
   definitions in config.h .  The index should be obtained
   by a call to MPITEST_get_datatype(i), where i is
   an iteration count between 0 and MPITEST_num_datatypes() */

MPI_Datatype MPITEST_mpi_datatypes[ ]=
{
0, /* MPI_INT */
0, /* MPI_SHORT */
0, /* MPI_LONG */
0, /* MPI_UNSIGNED_SHORT */
0, /* MPI_UNSIGNED */
0, /* MPI_UNSIGNED_LONG */
0, /* MPI_FLOAT */
0, /* MPI_DOUBLE */
0, /* MPI_CHAR */
0, /* MPI_UNSIGNED_CHAR, optional */
0, /* MPI_LONG_LONG_INT, optional */
0, /* MPI_LONG_DOUBLE */
0, /* MPI_BYTE */
0, /* derived 1 */
0, /* derived 2 */
0,                      /* other user-defined data types */
0,
0,
0,
0,
0
};

void MPITEST_datatype_init()
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
    ierr = MPI_Type_commit(&MPITEST_mpi_datatypes[MPITEST_derived1+k]);

  }

if (MPITEST_me%2 == 0)
    MPITEST_mpi_datatypes[MPITEST_derived2] =
	MPITEST_mpi_datatypes[MPITEST_derived1];

}

