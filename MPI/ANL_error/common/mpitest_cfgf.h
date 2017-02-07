      BLOCK DATA CONFIG
C     ******************************************************************
C     Block data construct to initialize the configuration data.
C
C     Note that the number of tokens up to and including MPITEST_END_TOKEN
C     must be given by the parameter numtok.  This is so that
C     FORTRAN can know how many zeros to add to the end of the array.
C
C     This file should be included in one and only one module
C     in the application.  This module initializes the MPITEST_comms()
C     configuration array.
C
C     History :
C     12/07/95        Created       Greg Morrow
C     ******************************************************************
      INCLUDE 'mpitestf.h'

      INTEGER KILO
      PARAMETER(KILO=1024)

      INTEGER KILOMIN8, KILOADD8
      PARAMETER(KILOMIN8 = KILO - 8, KILOADD8 = KILO + 8)

C     Each array is MPITEST_CFGSIZ entries long.  Since Fortran
C     requires an entire array to be initialized, we have to pad
C     the end.  Thus NUMTOKx is the number of elements in the array,
C     so NUMZERx is how much padding is required.
C
C     Use caution when creating these arrays.  The must be valid
C     for the size and type of your machine; needless test errors
C     will occur if (for example) you configure a communicator
C     with more ranks than exist.  This example requires 3 ranks.
C 11-01-02
C   Note in particular that the communicators used to build the 
C   result of MPI_INTERCOMM_MERGE require at least 3 processes.
C   Using any fewer will result in the code attempting to build 
C   an intercommunicator with overlapping groups.
      INTEGER NUMTOK1
      INTEGER NUMZER1
      PARAMETER( NUMTOK1 = 36, NUMZER1=MPITEST_CFGSIZ-NUMTOK1)

      DATA MPITEST_COMMS /                                                  &
     & MPITEST_COMM_WORLD,                                                  &
     & MPITEST_COMM_SELF,                                                   &
     & MPITEST_COMM_CREATE, MPITEST_COMM_HALF,                              &
     & MPITEST_COMM_CREATE, 2,                                              &
     & MPITEST_COMM_SPLIT, MPITEST_COMM_HALF,                               &
     & MPITEST_COMM_MERGE,                                                  &
     &  MPITEST_COMM_DUP,                                                   &
     &   MPITEST_COMM_RNKLST, 2, 1, 0,                                      &
     &  MPITEST_COMM_DUP,                                                   &
     &   MPITEST_COMM_COMINC, 2, MPITEST_COMM_LASTRNK, 3,                   &
     & MPITEST_COMM_INTER,                                                  &
     &  MPITEST_COMM_DUP,                                                   &
     &   MPITEST_COMM_RNKLST, 2, 1, 0,                                      &
     &  MPITEST_COMM_CREATE,                                                &
     &   MPITEST_COMM_COMINC, 2, MPITEST_COMM_LASTRNK, 3,                   &
     & MPITEST_COMM_CREATE,                                                 &
     &  MPITEST_COMM_RNKLST, 2, 1, 0,                                       &
     &  MPITEST_END_TOKEN, NUMZER1*0/

C
C     Message lengths.  CAUTION:  the larges message length allowed
C     in the array is MPITEST_BUFF_EXTENT (see foptions.h
C     times MAX_BUFF_SIZE (see mpitestf.h).  To do otherwise
C     will cause data to overflow pre-allocated buffers (we do
C     not try to use some form of malloc in Fortran).
C
      INTEGER NUMTOK2
      INTEGER NUMZER2
      PARAMETER( NUMTOK2 = 14, NUMZER2=MPITEST_CFGSIZ-NUMTOK2)

      DATA MPITEST_MESSAGE_LENGTHS /                                        &
     & 0,                                                                   &
     & MPITEST_MULT_INC, 8, 8000, 10,                                       &
     & MPITEST_REPEAT, 320, 8,                                              &
     & 65536,                                                               &
     & MPITEST_ADD_INC, KILOMIN8, KILOADD8, 8,                              &
     & MPITEST_END_TOKEN, NUMZER2*0 /

C
C     Data types.  Do not use an optional type unless you have
C     allowed it in foptions.h or link errors will occur.
C
      INTEGER NUMTOK3
      INTEGER NUMZER3
      PARAMETER( NUMTOK3 = 13, NUMZER3=MPITEST_CFGSIZ-NUMTOK3)

      DATA MPITEST_TYPES /                                                  &
     & MPITEST_INTEGER, MPITEST_REAL, MPITEST_DOUBLE_PRECISION,             &
     & MPITEST_COMPLEX, MPITEST_LOGICAL, MPITEST_CHARACTER,                 &
     & MPITEST_INTEGER1,                                                    & 
     & MPITEST_INTEGER2,                                                    &
     & MPITEST_INTEGER4,                                                    &
     & MPITEST_REAL4,                                                       &
     & MPITEST_REAL8,                                                       &
     & MPITEST_DOUBLE_COMPLEX,                                              &
     & MPITEST_END_TOKEN, NUMZER3*0 /
C
C  Add MPITEST_REAL2, if supported.
C
      END
C     end of 'BLOCK DATA CONFIG'      
