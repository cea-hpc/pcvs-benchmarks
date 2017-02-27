C ----------------------------------------------------------------------
C MESSAGE PASSING INTERFACE TEST CASE SUITE
C
C Copyright - 1996 Intel Corporation
C
C Intel Corporation hereby grants a non-exclusive license under Intel's
C copyright to copy, modify and distribute this software for any purpose
C and without fee, provided that the above copyright notice and the
C following paragraphs appear on all copies.
C
C Intel Corporation makes no representation that the test cases
C comprising this suite are correct or are an accurate representation
C of any standard.
C
C IN NO EVENT SHALL INTEL HAVE ANY LIABILITY FOR ANY DIRECT, INDIRECT
C OR SPECULATIVE DAMAGES, (INCLUDING WITHOUT LIMITING THE FOREGOING,
C CONSEQUENTIAL, INCIDENTAL AND SPECIAL DAMAGES) INCLUDING, BUT NOT
C LIMITED TO INFRINGEMENT, LOSS OF USE, BUSINESS INTERRUPTIONS, AND
C LOSS OF PROFITS, IRRESPECTIVE OF WHETHER INTEL HAS ADVANCE NOTICE OF
C THE POSSIBILITY OF ANY SUCH DAMAGES.
C
C INTEL CORPORATION SPECIFICALLY DISCLAIMS ANY WARRANTIES INCLUDING,
C BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY, FITNESS
C FOR A PARTICULAR PURPOSE AND NON-INFRINGEMENT.  THE SOFTWARE PROVIDED
C HEREUNDER IS ON AN "AS IS" BASIS AND INTEL CORPORATION HAS NO
C OBLIGATION TO PROVIDE MAINTENANCE, SUPPORT, UPDATES, ENHANCEMENTS OR
C MODIFICATIONS.
C
C ----------------------------------------------------------------------
C ***************************************************************************
C Fortran library functions.
C
C Most common customization is probably for I/O functions - see
C MPITEST_INIT, MPITEST_MESSAGE and MPITEST_REPORT.
C
C Otherwise, this handles buffer initialization & checking, and
C decoding the configuration array information.
C ***************************************************************************
#include "foptions.h"

      SUBROUTINE MPITEST_INT_TO_LOGICAL(I, L)
C     ***********************************************************************
C     Convert an integer to logical
C     ***********************************************************************
      INCLUDE 'mpitestf.h'

      INTEGER I, REMAINDER
      LOGICAL L

      REMAINDER = MOD(ABS(I), 2)
      IF (REMAINDER .EQ. 1) THEN
         L = .TRUE.
      ELSE
         L = .FALSE.
      END IF

      END


      SUBROUTINE MPITEST_INIT_DATATYPES()
C     ***********************************************************************
C     Initialize the datatype array
C     ***********************************************************************
      INCLUDE 'mpitestf.h'
      INCLUDE 'mpif.h'

      MPITEST_MPI_DATATYPES(MPITEST_INTEGER)=MPI_INTEGER
      MPITEST_MPI_DATATYPES(MPITEST_REAL)=MPI_REAL
      MPITEST_MPI_DATATYPES(MPITEST_COMPLEX)=MPI_COMPLEX
      MPITEST_MPI_DATATYPES(MPITEST_DOUBLE_PRECISION)=
     & MPI_DOUBLE_PRECISION
      MPITEST_MPI_DATATYPES(MPITEST_LOGICAL)=MPI_LOGICAL
      MPITEST_MPI_DATATYPES(MPITEST_CHARACTER)=MPI_CHARACTER


C
C     Optional Fortran datatypes
C
#ifdef MPITEST_FDOUBLE_COMPLEX_DEF
      MPITEST_MPI_DATATYPES(MPITEST_DOUBLE_COMPLEX)=
     & MPI_DOUBLE_COMPLEX
#endif

#ifdef MPITEST_FREAL2_DEF
      MPITEST_MPI_DATATYPES(MPITEST_REAL2)=MPI_REAL2
#endif

#ifdef MPITEST_FREAL4_DEF
      MPITEST_MPI_DATATYPES(MPITEST_REAL4)=MPI_REAL4
#endif

#ifdef MPITEST_FREAL8_DEF
      MPITEST_MPI_DATATYPES(MPITEST_REAL8)=MPI_REAL8
#endif

#ifdef MPITEST_FINTEGER1_DEF
      MPITEST_MPI_DATATYPES(MPITEST_INTEGER1)=MPI_INTEGER1
#endif

#ifdef MPITEST_FINTEGER2_DEF
      MPITEST_MPI_DATATYPES(MPITEST_INTEGER2)=MPI_INTEGER2
#endif

#ifdef MPITEST_FINTEGER4_DEF
      MPITEST_MPI_DATATYPES(MPITEST_INTEGER4)=MPI_INTEGER4
#endif

      END


      SUBROUTINE MPITEST_NUM_MESSAGE_LENGTHS(RTN)
C     ***********************************************************************
C     Return the number of messages to be looped over as specified
C     in the configuration array MPITEST_message_lengths[] (defined
C     in include/mpitest_cfgf.h.)
C   
C     Arguments : none
C   
C     Algorithm :
C     Using a while loop, step through the MPITEST_message_lengths[] array
C     until the MPITEST_END_TOKEN is found.  Each time through the while
C     loop, execute a switch statement whose key is the curent element
C     of the MPITEST_message_lengths[] array.  From here on in this comment
C     we refer to MPITEST_message_lengths[] as array[].
C   
C     The body of the switch does the following.  (element is equal
C     to the current element of array[].)
C     1) Error checking.
C     a) check to make sure that none of the operations
C     specified below will read memory beyond the end of
C     array[].
C     b) For the case of element = magic number MPITEST_MULT_INC, first set
C            start = array[ ++index];
C            end = array[ ++index];
C            inc = array[ ++index];
C     then check to see that end >= start > 0, and inc > 1.
C     c) For the case element = magic number MPITEST_ADD_INC, first set
C            start = array[ ++index];
C            end = array[ ++index];
C            inc = array[ ++index];
C     then check to see that end >= start >= 0, and inc > 0.
C     d) For the case element = MPITEST_REPEAT, first set
C            value = array[ ++index];
C            rep_factor = arrray[ ++index];
C     then check to see that value > 0 and rep_factor > 0.
C     e) For the default case, make sure that element is greater
C     than or equal to 0.
C   
C     2) Counting the number of messages.
C     Within the body of the aforementioned while loop, increment the
C     total variable (which starts out at 0) in the following way,
C     a) for element = MPITEST_MULT_INC,
C     total += 1 + log10((end/start))/log10(inc).
C     In this case, increment the index by 4 (to get past element, start,
C     end, and inc.)
C     b) for element = MPITEST_ADD_INC,
C     total += 1 + (end-start)/inc.
C     In this case, increment the index by 4 (to get past element, start,
C     end, and inc.)
C     c) for element = MPITEST_REPEAT,
C     total += rep_factor.
C     In this case, increment the index by 3 (to get past value and
C     rep_factor.)
C     d) for the default case, total+=1 and index += 1.
C   
C     3) return total
C     ***********************************************************************
C
      INCLUDE 'mpitestf.h'

      INTEGER RTN

      INTEGER INDEX, ELEMENT, TOTAL, START, ENDD, INC, VALUE,
     & REP_FACTOR
      CHARACTER*(IOSIZE) INFOBUF

      TOTAL = 0
      INDEX = 1

100   ELEMENT = MPITEST_MESSAGE_LENGTHS(INDEX)

      IF (ELEMENT .EQ. MPITEST_END_TOKEN) THEN
            GO TO 200
      ELSE IF (ELEMENT .EQ. MPITEST_MULT_INC) THEN
         INDEX = INDEX + 1
         START = MPITEST_MESSAGE_LENGTHS(INDEX)
         INDEX = INDEX + 1
         ENDD = MPITEST_MESSAGE_LENGTHS(INDEX)
         INDEX = INDEX + 1
         INC = MPITEST_MESSAGE_LENGTHS(INDEX)

         IF (START .LT. 0) THEN
            INFOBUF='MPITEST_NULT_INC start value less than 1'
            CALL MPITEST_MESSAGE(MPITEST_FATAL, INFOBUF)
         END IF

         IF (ENDD .LT. START) THEN
            INFOBUF='MPITEST_MULT_INC end value less than start value'
            CALL MPITEST_MESSAGE(MPITEST_FATAL, INFOBUF)
         END IF

         IF (INC .LT. 2) THEN
            INFOBUF='MPITEST_MULT_INC increment less than 2'
            CALL MPITEST_MESSAGE(MPITEST_FATAL, INFOBUF)
         END IF

         TOTAL = TOTAL + 1 + INT(ALOG10(REAL((ENDD / START)
     & / ALOG10(REAL(INC)))))
         INDEX = INDEX + 1
      ELSE IF (ELEMENT .EQ. MPITEST_REPEAT) THEN
         INDEX = INDEX + 1
         VALUE = MPITEST_MESSAGE_LENGTHS(INDEX)
         INDEX = INDEX + 1
         REP_FACTOR = MPITEST_MESSAGE_LENGTHS(INDEX)

         IF (VALUE .LT. 0) THEN
            INFOBUF='MPITEST_REPEAT length value less than 0'
            CALL MPITEST_MESSAGE(MPITEST_FATAL, INFOBUF)
         END IF

         IF (REP_FACTOR .LT. 0) THEN
            INFOBUF='MPITEST_REPEAT number of repititions less than 0'
            CALL MPITEST_MESSAGE(MPITEST_FATAL, INFOBUF)
         END IF

         TOTAL = TOTAL + REP_FACTOR
         INDEX = INDEX + 1
      ELSE IF (ELEMENT .EQ. MPITEST_ADD_INC) THEN
         INDEX = INDEX + 1
         START = MPITEST_MESSAGE_LENGTHS(INDEX)
         INDEX = INDEX + 1
         ENDD = MPITEST_MESSAGE_LENGTHS(INDEX)
         INDEX = INDEX + 1
         INC = MPITEST_MESSAGE_LENGTHS(INDEX)

         IF (START .LT. 0) THEN
            INFOBUF='MPITEST_NULT_INC start value less than 1'
            CALL MPITEST_MESSAGE(MPITEST_FATAL, INFOBUF)
         END IF

         IF (ENDD .LT. START) THEN
            INFOBUF='MPITEST_MULT_INC end value less than start value'
            CALL MPITEST_MESSAGE(MPITEST_FATAL, INFOBUF)
         END IF

         IF (INC .LT. 1) THEN
            INFOBUF='MPITEST_MULT_INC increment less than 2'
            CALL MPITEST_MESSAGE(MPITEST_FATAL, INFOBUF)
         END IF

         TOTAL = TOTAL + 1 + (ENDD - START) / INC
         INDEX = INDEX + 1
      ELSE
         IF (ELEMENT .LT. 0) THEN
            INFOBUF=
     &         'Negative message length in MPITEST_message_lengths()'
            CALL MPITEST_MESSAGE(MPITEST_FATAL, INFOBUF)
         END IF

         TOTAL = TOTAL + 1
         INDEX = INDEX + 1
      END IF

      GO TO 100

200   RTN = TOTAL
      END


      SUBROUTINE MPITEST_GET_MESSAGE_LENGTH(I, RTN)
C     ***********************************************************************
C     Return the length of the ith message as specified by the
C     configuration array MPITEST_message_lengths[] (hereafter referred
C     to as "array[]").
C      
C     Arguments :
C         integer i      INPUT, specifies which iteration we want the message
C                         length for.
C      
C     Algorithm :
C     1) Error checking.  This routine makes a call to
C     MPITEST_num_message_lengths(), which error checks array[].  The only
C     error checking done by this routine is that i is less than the
C     return value of MPITEST_num_message_lengths().
C      
C     2) Static set-up
C     In the case that i==0, two steps are taken.
C       a) the static int max_i is set to MPITEST_num_message_lengths(),
C       and thereafter is used to error check the argument i.
C 
C     b) the breakpoints[] array is set up.  The element breakpoints[2*i]
C     is set to the iteration at which the message length style changes from
C     style "i-1" to style "i".  "Message length style" refers to either
C     MPITEST_MULT_INC, MPITEST_ADD_INC, MPITEST_REPEAT,or a simple
C     enumeration of a non-negative message length.  The element
C     breakoints[2*i+1] gives the index into array[] at which the ith style
C     begins.  The reason for setting these values should become clear as
C     the algorithm is described further below.
C    
C     3) Determination of the message length
C     a) The current message length style is determined.  This is
C     accomplished by stepping through the breakpoints[] array until
C     (i < breakpoints[ 2*(breakpoint_index)]).  When this condition is
C     satisfied, we set
C       index = i - breakpoints[ 2*(breakpoint_index-1)]
C       element = array[ breakpoints[ 2*(breakpoint_index-1)+1]
C     Referring to the definitions of the breakpoint array above,
C     we see that now index contains the rank of the current iteration
C     in the current message style, and element contains the element of
C     array[] which defines the current message style.  For example,
C     if (i=17) and a new message style began at iteration 15, then index
C     would equal 2.  If the message style which began at iteration 15 was
C     MPITEST_ADD_INC, then element would now contain MPITEST_ADD_INC.  If
C     the current message style were simple message length enumeration, then
C     element would contain the current message length.
C
C     b) The current message length is determined.
C     This is accomplished by using a switch whose key is 'element'.
C     If element is one of the magic numbers, then the appropriate number
C     of increments are added to the starting value to get to the current
C     value.  The 'appropriate number' is simply the index variable from
C     step 3a).  Otherwise, current length is simply element.
C    
C     4) The current length is returned.
C
C     ***********************************************************************
C
      INCLUDE 'mpitestf.h'
      INTEGER I, RTN

      INTEGER INDEX, ELEMENT, START, ENDD, INC, CURRENT_TOTAL
      INTEGER BREAKPOINT_INDEX, LENGTH, VALUE, REP_FACTOR
      INTEGER MAX_I, J

      CHARACTER*(IOSIZE) INFOBUF

      SAVE MAX_I

999   FORMAT ('MPITEST_get_message_length(): Iteration ',
     & 'parameter out of bounds in routine',
     & ' (i: ', INT_FMT, ', max_i: ', INT_FMT, ')')

      CURRENT_TOTAL = 0
      BREAKPOINT_INDEX = 1

      IF (I .EQ. 1) THEN
         CALL MPITEST_NUM_MESSAGE_LENGTHS(MAX_I)
         INDEX = 1
         BREAKPOINTS(1) = 0
         BREAKPOINTS(2) = 0

         BREAKPOINT_INDEX = BREAKPOINT_INDEX + 1

100      ELEMENT = MPITEST_MESSAGE_LENGTHS(INDEX)

         IF (ELEMENT .EQ. MPITEST_END_TOKEN) THEN
            GO TO 200
         ELSE IF (ELEMENT .EQ. MPITEST_MULT_INC) THEN
            START = MPITEST_MESSAGE_LENGTHS(INDEX + 1)
            ENDD = MPITEST_MESSAGE_LENGTHS(INDEX + 2)
            INC = MPITEST_MESSAGE_LENGTHS(INDEX + 3)

            CURRENT_TOTAL = CURRENT_TOTAL + 1 +
     &         INT(ALOG10(REAL((ENDD / START) / ALOG10(REAL(INC)))))
            BREAKPOINTS(2 * BREAKPOINT_INDEX - 1) =
     &         CURRENT_TOTAL
            BREAKPOINTS(2 * BREAKPOINT_INDEX) =
     &         INDEX
            BREAKPOINT_INDEX = BREAKPOINT_INDEX + 1
            INDEX = INDEX + 4
         ELSE IF (ELEMENT .EQ. MPITEST_ADD_INC) THEN
            START = MPITEST_MESSAGE_LENGTHS(INDEX + 1)
            ENDD = MPITEST_MESSAGE_LENGTHS(INDEX + 2)
            INC = MPITEST_MESSAGE_LENGTHS(INDEX + 3)

            CURRENT_TOTAL = CURRENT_TOTAL + 1 +
     &         ((ENDD - START) / INC)

            BREAKPOINTS(2 * BREAKPOINT_INDEX - 1) =
     &         CURRENT_TOTAL
            BREAKPOINTS(2 * BREAKPOINT_INDEX) =
     &         INDEX
            BREAKPOINT_INDEX = BREAKPOINT_INDEX + 1
            INDEX = INDEX + 4
         ELSE IF (ELEMENT .EQ. MPITEST_REPEAT) THEN
            VALUE = MPITEST_MESSAGE_LENGTHS(INDEX + 1)
            REP_FACTOR = MPITEST_MESSAGE_LENGTHS(INDEX + 2)
            CURRENT_TOTAL = CURRENT_TOTAL + REP_FACTOR

            BREAKPOINTS(2 * BREAKPOINT_INDEX - 1) =
     &         CURRENT_TOTAL
            BREAKPOINTS(2 * BREAKPOINT_INDEX) =
     &         INDEX
            BREAKPOINT_INDEX = BREAKPOINT_INDEX + 1
            INDEX = INDEX + 3
         ELSE
            CURRENT_TOTAL = CURRENT_TOTAL + 1
            BREAKPOINTS(2 * BREAKPOINT_INDEX - 1) =
     &         CURRENT_TOTAL
            BREAKPOINTS(2 * BREAKPOINT_INDEX) =
     &         INDEX
            BREAKPOINT_INDEX = BREAKPOINT_INDEX + 1
            INDEX = INDEX + 1
         END IF

         GO TO 100
 200     CONTINUE
      END IF

C     ERROR CHECKING

      IF (I .GT. MAX_I) THEN
         WRITE (INFOBUF, 999) I, MAX_I
         CALL MPITEST_MESSAGE(MPITEST_FATAL, INFOBUF)
      END IF

      DO 300 BREAKPOINT_INDEX = 1, MPITEST_CFGSIZ, 1
         IF (I .LE. BREAKPOINTS(2 * BREAKPOINT_INDEX - 1)) THEN
            INDEX = I - BREAKPOINTS(2 * (BREAKPOINT_INDEX - 1) - 1)

            ELEMENT = MPITEST_MESSAGE_LENGTHS(BREAKPOINTS(
     &         2 * BREAKPOINT_INDEX))

            GO TO 350
         END IF

300   CONTINUE

350   IF (ELEMENT .EQ. MPITEST_MULT_INC) THEN
         START = MPITEST_MESSAGE_LENGTHS(1 +
     &      BREAKPOINTS(2 * BREAKPOINT_INDEX))
         INC = MPITEST_MESSAGE_LENGTHS(3 +
     &      BREAKPOINTS(2 * BREAKPOINT_INDEX))
         J = 1
400      IF (J .LT. INDEX) THEN
            START = START * INC
         ELSE
            GO TO 500
         END IF

         J = J + 1
         GO TO 400

500      LENGTH = START
      ELSE IF (ELEMENT .EQ. MPITEST_ADD_INC) THEN
         START = MPITEST_MESSAGE_LENGTHS(1 +
     &      BREAKPOINTS(2 * BREAKPOINT_INDEX))
         INC = MPITEST_MESSAGE_LENGTHS(3 +
     &      BREAKPOINTS(2 * BREAKPOINT_INDEX))
         J = 1
600      IF (J .LT. INDEX) THEN
            START = START + INC
         ELSE
            GO TO 700
         END IF

         J = J + 1
         GO TO 600

700      LENGTH = START
      ELSE IF (ELEMENT .EQ. MPITEST_REPEAT) THEN
         LENGTH = MPITEST_MESSAGE_LENGTHS(1 +
     &      BREAKPOINTS(2 * BREAKPOINT_INDEX))
      ELSE
         LENGTH = ELEMENT
      END IF

      RTN = LENGTH

      END


      SUBROUTINE MPITEST_GET_MAX_MESSAGE_LENGTH(RTN)
C     ***********************************************************************
C     Return the largest message length specified in the
C     MPITEST_message_lengths[] array.
C   
C     Repeatedly call MPITEST_get_message_length(i) for 'i' running from
C     0 to MPITEST_num_message_lengths()-1 .  Return the largest value found.
C     ***********************************************************************
      INCLUDE 'mpitestf.h'

      INTEGER RTN

      INTEGER I, ELEMENT, MAX_ELEMENT, MSG_LEN

      MAX_ELEMENT = 0

      CALL MPITEST_NUM_MESSAGE_LENGTHS(MSG_LEN)

      DO 100 I = 1, MSG_LEN, 1
         CALL MPITEST_GET_MESSAGE_LENGTH(I, ELEMENT)
         IF (ELEMENT .GT. MAX_ELEMENT) THEN
            MAX_ELEMENT = ELEMENT
         END IF

100   CONTINUE

      RTN = MAX_ELEMENT
      END


      SUBROUTINE MPITEST_BYTE_TO_ELEMENT(BUFFER_TYPE, BYTE_LENGTH, RTN)
C     ***********************************************************************
C     Return the number of elements of a given type which would
C     comprise a given number of bytes.  Generate a non-fatal message
C     if the size of the data type does not divide evenly into the
C     number of bytes.
C     ***********************************************************************
C 
      INCLUDE 'mpitestf.h'
      INCLUDE 'mpif.h'

      INTEGER BUFFER_TYPE, BYTE_LENGTH, RTN
C WDG - 2/10/04 - hold on to the type name to provide better 
C error messages
      CHARACTER*50 BUF_TYPE_NAME

C WDG - 4/28/03 - The original used MPITEST_AINT for TYPE_SIZE, which 
C is incorrect - these are INTEGER in the Fortran binding for 
C MPI_TYPE_EXTENT
      INTEGER TYPE_SIZE

      INTEGER IERR, ERRSIZE, ERR

      CHARACTER*(IOSIZE) INFOBUF

100   FORMAT('MPITEST_BYTE_TO_ELEMENT(): INVALID BUFFER TYPE',
     &       INT_FMT)
200   FORMAT('MPITEST_BYTE_TO_ELEMENT(): ',
     &       'Byte length ', INT_FMT, ' does not divide evenly by type',
     &       INT_FMT, '(',a,')',' Using ', INT_FMT, ' elements')

300   FORMAT('Message byte length ', INT_FMT, 
     &       ' exceeds maximum buffer length ', INT_FMT,
     &       ', Using ', INT_FMT)

      IF (BUFFER_TYPE .EQ. MPITEST_INTEGER) THEN
         CALL MPI_TYPE_EXTENT(MPI_INTEGER, TYPE_SIZE, IERR)
         BUF_TYPE_NAME = 'INTEGER'
      ELSE IF (BUFFER_TYPE .EQ. MPITEST_REAL) THEN
         CALL MPI_TYPE_EXTENT(MPI_REAL, TYPE_SIZE, IERR)
         BUF_TYPE_NAME = 'REAL'
      ELSE IF (BUFFER_TYPE .EQ. MPITEST_DOUBLE_PRECISION) THEN
         CALL MPI_TYPE_EXTENT(MPI_DOUBLE_PRECISION, TYPE_SIZE, IERR)
         BUF_TYPE_NAME = 'DOUBLE PRECISION'
      ELSE IF (BUFFER_TYPE .EQ. MPITEST_COMPLEX) THEN
         CALL MPI_TYPE_EXTENT(MPI_COMPLEX, TYPE_SIZE, IERR)
         BUF_TYPE_NAME = 'COMPLEX'
      ELSE IF (BUFFER_TYPE .EQ. MPITEST_LOGICAL) THEN
         CALL MPI_TYPE_EXTENT(MPI_LOGICAL, TYPE_SIZE, IERR)
         BUF_TYPE_NAME = 'LOGICAL'
      ELSE IF (BUFFER_TYPE .EQ. MPITEST_CHARACTER) THEN
         CALL MPI_TYPE_EXTENT(MPI_CHARACTER, TYPE_SIZE, IERR)
         BUF_TYPE_NAME = 'CHARACTER'

#ifdef MPITEST_FINTEGER1_DEF
      ELSE IF (BUFFER_TYPE .EQ. MPITEST_INTEGER1) THEN
         CALL MPI_TYPE_EXTENT(MPI_INTEGER1, TYPE_SIZE, IERR)
         BUF_TYPE_NAME = 'INTEGER1'
#endif

#ifdef MPITEST_FINTEGER2_DEF
      ELSE IF (BUFFER_TYPE .EQ. MPITEST_INTEGER2) THEN
         CALL MPI_TYPE_EXTENT(MPI_INTEGER2, TYPE_SIZE, IERR)
         BUF_TYPE_NAME = 'INTEGER2'
#endif

#ifdef MPITEST_FINTEGER4_DEF
      ELSE IF (BUFFER_TYPE .EQ. MPITEST_INTEGER4) THEN
         CALL MPI_TYPE_EXTENT(MPI_INTEGER4, TYPE_SIZE, IERR)
         BUF_TYPE_NAME = 'INTEGER4'
#endif

#ifdef MPITEST_FREAL2_DEF
      ELSE IF (BUFFER_TYPE .EQ. MPITEST_REAL2) THEN
         CALL MPI_TYPE_EXTENT(MPI_REAL2, TYPE_SIZE, IERR)
         BUF_TYPE_NAME = 'REAL2'
#endif

#ifdef MPITEST_FREAL4_DEF
      ELSE IF (BUFFER_TYPE .EQ. MPITEST_REAL4) THEN
         CALL MPI_TYPE_EXTENT(MPI_REAL4, TYPE_SIZE, IERR)
         BUF_TYPE_NAME = 'REAL4'
#endif

#ifdef MPITEST_FREAL8_DEF
      ELSE IF (BUFFER_TYPE .EQ. MPITEST_REAL8) THEN
         CALL MPI_TYPE_EXTENT(MPI_REAL8, TYPE_SIZE, IERR)
         BUF_TYPE_NAME = 'REAL8'
#endif

#ifdef MPITEST_FDOUBLE_COMPLEX_DEF
      ELSE IF (BUFFER_TYPE .EQ. MPITEST_DOUBLE_COMPLEX) THEN
         CALL MPI_TYPE_EXTENT(MPI_DOUBLE_COMPLEX, TYPE_SIZE, IERR)
         BUF_TYPE_NAME = 'DOUBLE COMPLEX'
#endif

      ELSE
         WRITE (INFOBUF, 100) BUFFER_TYPE
         CALL MPITEST_MESSAGE(MPITEST_FATAL, INFOBUF)
         BUF_TYPE_NAME = 'Unrecognized'
      END IF

      IF (IERR .NE. MPI_SUCCESS) THEN
         CALL MPI_ERROR_STRING(IERR, INFOBUF, ERRSIZE, ERR)
         CALL MPITEST_MESSAGE(MPITEST_FATAL, INFOBUF)
      END IF

      RTN = INT(BYTE_LENGTH / TYPE_SIZE)

      IF (RTN .GT. MAX_BUFF_SIZE * MPITEST_BUF_EXTENT) THEN
         IF (MPITEST_ME .EQ. 0) THEN
            WRITE (INFOBUF, 300)RTN, MAX_BUFF_SIZE * MPITEST_BUF_EXTENT,
     &      MAX_BUFF_SIZE * MPITEST_BUF_EXTENT
            CALL MPITEST_MESSAGE(MPITEST_NONFATAL, INFOBUF)
         END IF
         RTN = MAX_BUFF_SIZE * MPITEST_BUF_EXTENT
      ELSE IF ((RTN * TYPE_SIZE .NE. BYTE_LENGTH) .AND.
     & (MPITEST_ME .EQ. 0)) THEN
         WRITE (INFOBUF, 200) BYTE_LENGTH, BUFFER_TYPE,BUF_TYPE_NAME,RTN
         CALL MPITEST_MESSAGE(MPITEST_INFO0, INFOBUF)
      END IF

      END


      SUBROUTINE MPITEST_NUM_DATATYPES(RTN)
C     ***********************************************************************
C     Returns the number of different data types in the default
C     data-type loop.
C      
C     Arguments : none
C      
C     Return value : integer number of different datatypes to loop over
C     as defined in the file include/mpitest_cfgf.h .  The default datatypes
C     are given in the array MPITEST_types[], and this array is a simple
C     enumeration, so the number of types is simply the number of integers
C     in that array.
C      
C     Error checking :
C     This routine checks that all elements of MPITEST_types[] are valid
C     indices into MPITEST_mpi_datatypes[].  This is done by first setting
C     num_types to the length of MPITEST_mpi_datatypes[], then making sure
C     that each element of MPITEST_types[] is less than num_types.  If an
C     invalid entry is found, a fatal error is triggered
C     ***********************************************************************
C
      INCLUDE 'mpitestf.h'

      INTEGER RTN
      INTEGER NUM_MPI_TYPES, I, NUM_TYPES, ERROR
      CHARACTER*(IOSIZE) INFOBUF

50    FORMAT('Illegal datatype in array MPITEST_types()',
     &       INT_FMT)

C     MPITEST_datatype_max is defined (in mpitest_cfgf.h) to be the largest
C     legal index into the array of MPI datatypes.

      NUM_MPI_TYPES = MPITEST_DATATYPE_MAX
      NUM_TYPES = MPITEST_DATATYPE_MAX
      ERROR = 0

      DO 100 I = 1, MPITEST_CFGSIZ, 1
         IF (MPITEST_TYPES(I) .EQ. MPITEST_END_TOKEN) THEN
            NUM_TYPES = I - 1
            GO TO 150
         END IF

         IF ((MPITEST_TYPES(I) .LT. 0) .OR.
     & (MPITEST_TYPES(I) .GT. NUM_MPI_TYPES)) THEN
            ERROR = 1
         END IF
100   CONTINUE

150   IF (ERROR .GT. 0) THEN
         WRITE (INFOBUF, 50)
         CALL MPITEST_MESSAGE(MPITEST_FATAL, INFOBUF)
      END IF

      RTN = NUM_TYPES
      END


      SUBROUTINE MPITEST_GET_DATATYPE(I, RTN)
C     ***********************************************************************
C     Return the index into the MPITEST_mpi_datatypes[] array of the
C     datatype for the ith iteration of the datatypes loop.
C      
C     Arguments :
C       integer i         The iteration for which the datatype
C                             is desired.
C      
C     Return value : Integer index into the MPITEST_mpi_datatypes[]
C     array (defined in mpitest_cfgf.h) of the datatype appropriate for the
C     ith loop of the datatype iterator.
C      
C     Algorithm : Just return the ith element of MPITEST_types[].
C     ***********************************************************************
C
      INCLUDE 'mpitestf.h'

      INTEGER I, RTN
      CHARACTER*(IOSIZE) INFOBUF

      INTEGER MAX_I

      SAVE MAX_I

999   FORMAT ('MPITEST_get_message_length(): Iteration ',
     & 'parameter out of bounds in routine',
     & ' (i: ', INT_FMT, ', max_i: ', INT_FMT, ')')


      IF (I .EQ. 1) THEN
         CALL MPITEST_NUM_DATATYPES(MAX_I)
      ELSE IF (I .GT. MAX_I) THEN
         WRITE (INFOBUF, 999) I, MAX_I
         CALL MPITEST_MESSAGE(MPITEST_FATAL, INFOBUF)
      END IF

      RTN = MPITEST_TYPES(I)

      END


      SUBROUTINE MPITEST_INIT_EACH_INT_BUF(BUFFER, VALUE, I)
C     ***********************************************************************
C     Assigning an integer buffer
C     ***********************************************************************
C
      INTEGER BUFFER(*), VALUE, I

      BUFFER(I) = VALUE

      END


#ifdef MPITEST_FINTEGER1_DEF
      SUBROUTINE MPITEST_INIT_EACH_INT1_BUF(BUFFER, VALUE, I)
C     ***********************************************************************
C     Assigning an integer*1 buffer
C     ***********************************************************************
C
      INTEGER*1 BUFFER(*), VALUE
      INTEGER I

      BUFFER(I) = VALUE

      END
#endif


#ifdef MPITEST_FINTEGER2_DEF
      SUBROUTINE MPITEST_INIT_EACH_INT2_BUF(BUFFER, VALUE, I)
C     ***********************************************************************
C     Assigning an integer*2 buffer
C     ***********************************************************************
C
      INTEGER*2 BUFFER(*), VALUE
      INTEGER I

      BUFFER(I) = VALUE

      END
#endif


#ifdef MPITEST_FINTEGER4_DEF
      SUBROUTINE MPITEST_INIT_EACH_INT4_BUF(BUFFER, VALUE, I)
C     ***********************************************************************
C     Assigning an integer*4 buffer
C     ***********************************************************************
C
      INTEGER*4 BUFFER(*), VALUE
      INTEGER I

      BUFFER(I) = VALUE

      END
#endif


      SUBROUTINE MPITEST_INIT_EACH_DOUBLE_BUF(BUFFER, VALUE, I)
C     ***********************************************************************
C     Assigning a double buffer
C     ***********************************************************************
C
      DOUBLE PRECISION BUFFER(*), VALUE
      INTEGER I

      BUFFER(I) = VALUE

      END


      SUBROUTINE MPITEST_INIT_EACH_REAL_BUF(BUFFER, VALUE, I)
C     ***********************************************************************
C     Assigning an real buffer
C     ***********************************************************************
C
      REAL BUFFER(*), VALUE
      INTEGER I

      BUFFER(I) = VALUE

      END


#ifdef MPITEST_FREAL2_DEF
      SUBROUTINE MPITEST_INIT_EACH_REAL2_BUF(BUFFER, VALUE, I)
C     ***********************************************************************
C     Assigning an real*2 buffer
C     ***********************************************************************
C
      REAL*2 BUFFER(*), VALUE
      INTEGER I

      BUFFER(I) = VALUE

      END
#endif


#ifdef MPITEST_FREAL4_DEF
      SUBROUTINE MPITEST_INIT_EACH_REAL4_BUF(BUFFER, VALUE, I)
C     ***********************************************************************
C     Assigning an real*4 buffer
C     ***********************************************************************
C
      REAL*4 BUFFER(*), VALUE
      INTEGER I

      BUFFER(I) = VALUE

      END
#endif


#ifdef MPITEST_FREAL8_DEF
      SUBROUTINE MPITEST_INIT_EACH_REAL8_BUF(BUFFER, VALUE, I)
C     ***********************************************************************
C     Assigning an real*8 buffer
C     ***********************************************************************
C
      REAL*8 BUFFER(*), VALUE
      INTEGER I

      BUFFER(I) = VALUE

      END
#endif


      SUBROUTINE MPITEST_INIT_EACH_COMPLEX_BUF(BUFFER, VALUE, I)
C     ***********************************************************************
C     Assigning a complex buffer
C     ***********************************************************************
C
      COMPLEX BUFFER(*), VALUE
      INTEGER I

      BUFFER(I) = VALUE

      END


#ifdef MPITEST_FDOUBLE_COMPLEX_DEF
      SUBROUTINE MPITEST_INIT_EACH_DC_BUF(BUFFER, VALUE, I)
C     ***********************************************************************
C     Assigning a double complex buffer
C     ***********************************************************************
C
      DOUBLE COMPLEX BUFFER(*), VALUE
      INTEGER I

      BUFFER(I) = VALUE

      END
#endif


      SUBROUTINE MPITEST_INIT_EACH_LOGICAL_BUF(BUFFER, VALUE, I)
C     ***********************************************************************
C     Assigning an logical buffer
C     ***********************************************************************
C
      LOGICAL BUFFER(*), VALUE
      INTEGER I

      BUFFER(I) = VALUE

      END


      SUBROUTINE MPITEST_INIT_EACH_CHAR_BUF(BUFFER, VALUE, I)
C     ***********************************************************************
C     Assigning an character buffer
C     ***********************************************************************
C
      CHARACTER BUFFER *(*), VALUE
      INTEGER I

      BUFFER(I:I) = VALUE

      END


      SUBROUTINE MPITEST_INIT_EACH_INT_BUF_V(BUFFER, VALUES, I, J)
C     ***********************************************************************
C     Assigning an integer buffer
C     ***********************************************************************
C
      INTEGER BUFFER(*), VALUES(*), I, J

      BUFFER(J) = VALUES(I)

      END


#ifdef MPITEST_FINTEGER1_DEF
      SUBROUTINE MPITEST_INIT_EACH_INT1_BUF_V(BUFFER, VALUES, I, J)
C     ***********************************************************************
C     Assigning an integer*1 buffer
C     ***********************************************************************
C
      INTEGER*1 BUFFER(*), VALUES(*)
      INTEGER I, J

      BUFFER(J) = VALUES(I)

      END
#endif


#ifdef MPITEST_FINTEGER2_DEF
      SUBROUTINE MPITEST_INIT_EACH_INT2_BUF_V(BUFFER, VALUES, I, J)
C     ***********************************************************************
C     Assigning an integer*2 buffer
C     ***********************************************************************
C
      INTEGER*2 BUFFER(*), VALUES(*)
      INTEGER I, J

      BUFFER(J) = VALUES(I)

      END
#endif


#ifdef MPITEST_FINTEGER4_DEF
      SUBROUTINE MPITEST_INIT_EACH_INT4_BUF_V(BUFFER, VALUES, I, J)
C     ***********************************************************************
C     Assigning an integer*4 buffer
C     ***********************************************************************
C
      INTEGER*4 BUFFER(*), VALUES(*)
      INTEGER I, J

      BUFFER(J) = VALUES(I)

      END
#endif

      SUBROUTINE MPITEST_INIT_EACH_DOUBLE_BUF_V(BUFFER, VALUES, I, J)
C     ***********************************************************************
C     Assigning an double buffer
C     ***********************************************************************
C
      DOUBLE PRECISION BUFFER(*), VALUES(*)
      INTEGER I, J

      BUFFER(J) = VALUES(I)

      END


      SUBROUTINE MPITEST_INIT_EACH_REAL_BUF_V(BUFFER, VALUES, I, J)
C     ***********************************************************************
C     Assigning an real buffer
C     ***********************************************************************
C
      REAL BUFFER(*), VALUES(*)
      INTEGER I, J

      BUFFER(J) = VALUES(I)

      END


#ifdef MPITEST_FREAL2_DEF
      SUBROUTINE MPITEST_INIT_EACH_REAL2_BUF_V(BUFFER, VALUES, I, J)
C     ***********************************************************************
C     Assigning an real*2 buffer
C     ***********************************************************************
C
      REAL*2 BUFFER(*), VALUES(*)
      INTEGER I, J

      BUFFER(J) = VALUES(I)

      END
#endif


#ifdef MPITEST_FREAL4_DEF
      SUBROUTINE MPITEST_INIT_EACH_REAL4_BUF_V(BUFFER, VALUES, I, J)
C     ***********************************************************************
C     Assigning an real*4 buffer
C     ***********************************************************************
C
      REAL*4 BUFFER(*), VALUES(*)
      INTEGER I, J

      BUFFER(J) = VALUES(I)

      END
#endif


#ifdef MPITEST_FREAL8_DEF
      SUBROUTINE MPITEST_INIT_EACH_REAL8_BUF_V(BUFFER, VALUES, I, J)
C     ***********************************************************************
C     Assigning an real*8 buffer
C     ***********************************************************************
C
      REAL*8 BUFFER(*), VALUES(*)
      INTEGER I, J

      BUFFER(J) = VALUES(I)

      END
#endif


      SUBROUTINE MPITEST_INIT_EACH_COMPLEX_BUF_V(BUFFER, VALUES, I, J)
C     ***********************************************************************
C     Assigning a complex buffer
C     ***********************************************************************
C
      COMPLEX BUFFER(*), VALUES(*)
      INTEGER I, J

      BUFFER(J) = VALUES(I)

      END


#ifdef MPITEST_FDOUBLE_COMPLEX_DEF
      SUBROUTINE MPITEST_INIT_EACH_DC_BUF_V(BUFFER, VALUES,
     &                                                  I, J)
C     ***********************************************************************
C     Assigning a double complex buffer
C     ***********************************************************************
C
      DOUBLE COMPLEX BUFFER(*), VALUES(*)
      INTEGER I, J

      BUFFER(J) = VALUES(I)

      END
#endif


      SUBROUTINE MPITEST_INIT_EACH_LOGICAL_BUF_V(BUFFER, VALUES, I, J)
C     ***********************************************************************
C     Assigning an logical buffer
C     ***********************************************************************
C
      LOGICAL BUFFER(*), VALUES(*)
      INTEGER I, J

      BUFFER(J) = VALUES(I)

      END


      SUBROUTINE MPITEST_INIT_EACH_CHAR_BUF_V(BUFFER, VALUES, I, J)
C     ***********************************************************************
C     Assigning an character buffer
C     ***********************************************************************
C
      CHARACTER BUFFER *(*), VALUES(*)
      INTEGER I, J

      BUFFER(J:J) = VALUES(I)

      END



      SUBROUTINE MPITEST_INIT_EACH_INT_BUF_LOC(BUFFER, VALUE, I)
C     ***********************************************************************
C     Assigning an integer buffer
C     ***********************************************************************
C
      INTEGER BUFFER(*), VALUE(2), I

      BUFFER(2*I-1) = VALUE(1)
      BUFFER(2*I)   = VALUE(2)

      END


#ifdef MPITEST_FINTEGER1_DEF
      SUBROUTINE MPITEST_INIT_EACH_INT1_BUF_LOC(BUFFER, VALUE, I)
C     ***********************************************************************
C     Assigning an integer*1 buffer
C     ***********************************************************************
C
      INTEGER*1 BUFFER(*), VALUE(2)
      INTEGER I

      BUFFER(2*I-1) = VALUE(1)
      BUFFER(2*I)   = VALUE(2)

      END
#endif


#ifdef MPITEST_FINTEGER2_DEF
      SUBROUTINE MPITEST_INIT_EACH_INT2_BUF_LOC(BUFFER, VALUE, I)
C     ***********************************************************************
C     Assigning an integer*2 buffer
C     ***********************************************************************
C
      INTEGER*2 BUFFER(*), VALUE(2)
      INTEGER I

      BUFFER(2*I-1) = VALUE(1)
      BUFFER(2*I)   = VALUE(2)

      END
#endif


#ifdef MPITEST_FINTEGER4_DEF
      SUBROUTINE MPITEST_INIT_EACH_INT4_BUF_LOC(BUFFER, VALUE, I)
C     ***********************************************************************
C     Assigning an integer*4 buffer
C     ***********************************************************************
C
      INTEGER*4 BUFFER(*), VALUE(2)
      INTEGER I

      BUFFER(2*I-1) = VALUE(1)
      BUFFER(2*I)   = VALUE(2)

      END
#endif


      SUBROUTINE MPITEST_INIT_EACH_DBL_BUF_LOC(BUFFER, VALUE, I)
C     ***********************************************************************
C     Assigning an double buffer
C     ***********************************************************************
C
      DOUBLE PRECISION BUFFER(*), VALUE(2)
      INTEGER I

      BUFFER(2*I-1) = VALUE(1)
      BUFFER(2*I)   = VALUE(2)

      END


      SUBROUTINE MPITEST_INIT_EACH_REAL_BUF_LOC(BUFFER, VALUE, I)
C     ***********************************************************************
C     Assigning an real buffer
C     ***********************************************************************
C
      REAL BUFFER(*), VALUE(2)
      INTEGER I

      BUFFER(2*I-1) = VALUE(1)
      BUFFER(2*I)   = VALUE(2)

      END


#ifdef MPITEST_FREAL2_DEF
      SUBROUTINE MPITEST_INIT_EACH_REAL2_BUF_LOC(BUFFER, VALUE, I)
C     ***********************************************************************
C     Assigning an real*2 buffer
C     ***********************************************************************
C
      REAL*2 BUFFER(*), VALUE(2)
      INTEGER I

      BUFFER(2*I-1) = VALUE(1)
      BUFFER(2*I)   = VALUE(2)

      END
#endif


#ifdef MPITEST_FREAL4_DEF
      SUBROUTINE MPITEST_INIT_EACH_REAL4_BUF_LOC(BUFFER, VALUE, I)
C     ***********************************************************************
C     Assigning an real*4 buffer
C     ***********************************************************************
C
      REAL*4 BUFFER(*), VALUE(2)
      INTEGER I

      BUFFER(2*I-1) = VALUE(1)
      BUFFER(2*I)   = VALUE(2)

      END
#endif


#ifdef MPITEST_FREAL8_DEF
      SUBROUTINE MPITEST_INIT_EACH_REAL8_BUF_LOC(BUFFER, VALUE, I)
C     ***********************************************************************
C     Assigning an real*8 buffer
C     ***********************************************************************
C
      REAL*8 BUFFER(*), VALUE(2)
      INTEGER I

      BUFFER(2*I-1) = VALUE(1)
      BUFFER(2*I)   = VALUE(2)

      END
#endif


      SUBROUTINE MPITEST_INIT_EACH_CMPLX_BUF_LOC(BUFFER, VALUE, I)
C     ***********************************************************************
C     Assigning a complex buffer
C     ***********************************************************************
C
      COMPLEX BUFFER(*), VALUE(2)
      INTEGER I

      BUFFER(2*I-1) = VALUE(1)
      BUFFER(2*I)   = VALUE(2)

      END


#ifdef MPITEST_FDOUBLE_COMPLEX_DEF
      SUBROUTINE MPITEST_INIT_EACH_DC_BUF_LOC(BUFFER, VALUE, I)
C     ***********************************************************************
C     Assigning a double complex buffer
C     ***********************************************************************
C
      DOUBLE COMPLEX BUFFER(*), VALUE(2)
      INTEGER I

      BUFFER(2*I-1) = VALUE(1)
      BUFFER(2*I)   = VALUE(2)

      END
#endif


      SUBROUTINE MPITEST_INIT_EACH_LOGCL_BUF_LOC(BUFFER, VALUE, I)
C     ***********************************************************************
C     Assigning an logical buffer
C     ***********************************************************************
C
      LOGICAL BUFFER(*), VALUE(2)
      INTEGER I

      BUFFER(2*I-1) = VALUE(1)
      BUFFER(2*I)   = VALUE(2)

      END


      SUBROUTINE MPITEST_INIT_EACH_CHAR_BUF_LOC(BUFFER, VALUE, I)
C     ***********************************************************************
C     Assigning an character buffer
C     ***********************************************************************
C
      CHARACTER BUFFER *(*), VALUE(2)
      INTEGER I

      BUFFER(2*I-1:2*I-1) = VALUE(1)
      BUFFER(2*I:2*I)   = VALUE(2)

      END


      SUBROUTINE MPITEST_INIT_INT_BUF(BUFFER, VALUE)
C     ***********************************************************************
C     Assigning an integer buffer
C     ***********************************************************************
C
      INTEGER BUFFER, VALUE

      BUFFER = VALUE

      END


#ifdef MPITEST_FINTEGER1_DEF
      SUBROUTINE MPITEST_INIT_INT1_BUF(BUFFER, VALUE)
C     ***********************************************************************
C     Assigning an integer*1 buffer
C     ***********************************************************************
C
      INTEGER*1 BUFFER
      INTEGER VALUE

      BUFFER = VALUE

      END
#endif


#ifdef MPITEST_FINTEGER2_DEF
      SUBROUTINE MPITEST_INIT_INT2_BUF(BUFFER, VALUE)
C     ***********************************************************************
C     Assigning an integer*2 buffer
C     ***********************************************************************
C
      INTEGER*2 BUFFER
      INTEGER VALUE

      BUFFER = VALUE

      END
#endif


#ifdef MPITEST_FINTEGER4_DEF
      SUBROUTINE MPITEST_INIT_INT4_BUF(BUFFER, VALUE)
C     ***********************************************************************
C     Assigning an integer*4 buffer
C     ***********************************************************************
C
      INTEGER*4 BUFFER
      INTEGER VALUE

      BUFFER = VALUE

      END
#endif


      SUBROUTINE MPITEST_INIT_DOUBLE_BUF(BUFFER, VALUE)
C     ***********************************************************************
C     Assigning an double buffer
C     ***********************************************************************
C
      DOUBLE PRECISION BUFFER
      INTEGER VALUE

      BUFFER = DBLE(VALUE)

      END


      SUBROUTINE MPITEST_INIT_REAL_BUF(BUFFER, VALUE)
C     ***********************************************************************
C     Assigning an real buffer
C     ***********************************************************************
C
      REAL BUFFER
      INTEGER VALUE

      BUFFER = REAL(VALUE)

      END


#ifdef MPITEST_FREAL2_DEF
      SUBROUTINE MPITEST_INIT_REAL2_BUF(BUFFER, VALUE)
C     ***********************************************************************
C     Assigning an real*2 buffer
C     ***********************************************************************
C
      REAL*2 BUFFER
      INTEGER VALUE

      BUFFER = REAL(VALUE)

      END
#endif


#ifdef MPITEST_FREAL4_DEF
      SUBROUTINE MPITEST_INIT_REAL4_BUF(BUFFER, VALUE)
C     ***********************************************************************
C     Assigning an real*4 buffer
C     ***********************************************************************
C
      REAL*4 BUFFER
      INTEGER VALUE

      BUFFER = REAL(VALUE)

      END
#endif


#ifdef MPITEST_FREAL8_DEF
      SUBROUTINE MPITEST_INIT_REAL8_BUF(BUFFER, VALUE)
C     ***********************************************************************
C     Assigning an real*8 buffer
C     ***********************************************************************
C
      REAL*8 BUFFER
      INTEGER VALUE

      BUFFER = REAL(VALUE)

      END
#endif


      SUBROUTINE MPITEST_INIT_COMPLEX_BUF(BUFFER, VALUE)
C     ***********************************************************************
C     Assigning a complex buffer
C     ***********************************************************************
C
      COMPLEX BUFFER
      INTEGER VALUE

      BUFFER = CMPLX(VALUE)

      END


#ifdef MPITEST_FDOUBLE_COMPLEX_DEF
      SUBROUTINE MPITEST_INIT_DC_BUF(BUFFER, VALUE)
C     ***********************************************************************
C     Assigning a double complex buffer
C     ***********************************************************************
C
      DOUBLE COMPLEX BUFFER
      INTEGER VALUE

      BUFFER = CMPLX(VALUE)

      END
#endif


      SUBROUTINE MPITEST_INIT_LOGICAL_BUF(BUFFER, VALUE)
C     ***********************************************************************
C     Assigning an logical buffer
C     ***********************************************************************
C
      LOGICAL BUFFER
      INTEGER VALUE

      CALL MPITEST_INT_TO_LOGICAL(VALUE, BUFFER)

      END


      SUBROUTINE MPITEST_INIT_CHAR_BUF(BUFFER, VALUE)
C     ***********************************************************************
C     Assigning an character buffer
C     ***********************************************************************
C
      CHARACTER BUFFER
      INTEGER VALUE

      BUFFER = CHAR(VALUE)

      END


      SUBROUTINE MPITEST_INC_INIT_INT_BUF(BUFFER, VALUE, INC, I)
C     ***********************************************************************
C     Assigning an integer buffer
C     ***********************************************************************
C
      INTEGER BUFFER(*), VALUE, INC, I

      BUFFER(I) = VALUE + INC

      END


#ifdef MPITEST_FINTEGER1_DEF
      SUBROUTINE MPITEST_INC_INIT_INT1_BUF(BUFFER, VALUE, INC, I)
C     ***********************************************************************
C     Assigning an integer*1 buffer
C     ***********************************************************************
C
      INTEGER*1 BUFFER(*), VALUE
      INTEGER INC, I

      BUFFER(I) = VALUE + INC

      END
#endif


#ifdef MPITEST_FINTEGER2_DEF
      SUBROUTINE MPITEST_INC_INIT_INT2_BUF(BUFFER, VALUE, INC, I)
C     ***********************************************************************
C     Assigning an integer*2 buffer
C     ***********************************************************************
C
      INTEGER*2 BUFFER(*), VALUE
      INTEGER INC, I

      BUFFER(I) = VALUE + INC

      END
#endif


#ifdef MPITEST_FINTEGER4_DEF
      SUBROUTINE MPITEST_INC_INIT_INT4_BUF(BUFFER, VALUE, INC, I)
C     ***********************************************************************
C     Assigning an integer*4 buffer
C     ***********************************************************************
C
      INTEGER*4 BUFFER(*), VALUE
      INTEGER INC, I

      BUFFER(I) = VALUE + INC

      END
#endif


      SUBROUTINE MPITEST_INC_INIT_DOUBLE_BUF(BUFFER, VALUE, D, I)
C     ***********************************************************************
C     Assigning an integer buffer
C     ***********************************************************************
C
      DOUBLE PRECISION BUFFER(*), VALUE
      INTEGER I, D

      BUFFER(I) = VALUE + DBLE(D)

      END


      SUBROUTINE MPITEST_INC_INIT_REAL_BUF(BUFFER, VALUE, R, I)
C     ***********************************************************************
C     Assigning an real buffer
C     ***********************************************************************
C
      REAL BUFFER(*), VALUE
      INTEGER I, R

      BUFFER(I) = VALUE + REAL(R)

      END


#ifdef MPITEST_FREAL2_DEF
      SUBROUTINE MPITEST_INC_INIT_REAL2_BUF(BUFFER, VALUE, R, I)
C     ***********************************************************************
C     Assigning an real*2 buffer
C     ***********************************************************************
C
      REAL*2 BUFFER(*), VALUE
      INTEGER I, R

      BUFFER(I) = VALUE + REAL(R)

      END
#endif


#ifdef MPITEST_FREAL4_DEF
      SUBROUTINE MPITEST_INC_INIT_REAL4_BUF(BUFFER, VALUE, R, I)
C     ***********************************************************************
C     Assigning an real*4 buffer
C     ***********************************************************************
C
      REAL*4 BUFFER(*), VALUE
      INTEGER I, R

      BUFFER(I) = VALUE + REAL(R)

      END
#endif


#ifdef MPITEST_FREAL8_DEF
      SUBROUTINE MPITEST_INC_INIT_REAL8_BUF(BUFFER, VALUE, R, I)
C     ***********************************************************************
C     Assigning an real*8 buffer
C     ***********************************************************************
C
      REAL*8 BUFFER(*), VALUE
      INTEGER I, R

      BUFFER(I) = VALUE + REAL(R)

      END
#endif


      SUBROUTINE MPITEST_INC_INIT_COMPLEX_BUF(BUFFER, VALUE, C, I)
C     ***********************************************************************
C     Assigning a complex buffer
C     ***********************************************************************
C
      COMPLEX BUFFER(*), VALUE
      INTEGER I, C

      BUFFER(I) = VALUE + CMPLX(C)

      END


#ifdef MPITEST_FDOUBLE_COMPLEX_DEF
      SUBROUTINE MPITEST_INC_INIT_DC_BUF(BUFFER, VALUE,
     &                                               C, I)
C     ***********************************************************************
C     Assigning a double complex buffer
C     ***********************************************************************
C
      DOUBLE COMPLEX BUFFER(*), VALUE
      INTEGER I, C

      BUFFER(I) = VALUE + CMPLX(C)

      END
#endif


      SUBROUTINE MPITEST_INC_INIT_LOGICAL_BUF(BUFFER, VALUE, L, I)
C     ***********************************************************************
C     Assigning an logical buffer
C     ***********************************************************************
C
      LOGICAL BUFFER(*), VALUE
      INTEGER I, L

      
      BUFFER(I) = VALUE

       J = MOD(ABS(L), 2)

       IF (J .EQ. 0) THEN
          IF (VALUE .EQV. .TRUE.) THEN
            BUFFER(I) = .FALSE.
          ELSE
            BUFFER(I) = .TRUE.
           END IF
       ELSE
          BUFFER(I) = VALUE
       END IF

      END


      SUBROUTINE MPITEST_INC_INIT_CHAR_BUF(BUFFER, VALUE, C, I)
C     ***********************************************************************
C     Assigning an character buffer
C     ***********************************************************************
C
      CHARACTER BUFFER *(*), VALUE
      INTEGER I, C

      BUFFER(I:I) = CHAR(MOD(ICHAR(VALUE) + C, 128))

      END


      SUBROUTINE MPITEST_DATATEMPLATE_INIT(BUFFER, VALUE, TYPE)
C     ******************************************************************
C
C
C     ******************************************************************
C
      INCLUDE 'mpitestf.h'

      MPITEST_BUF_TYPE BUFFER
      INTEGER VALUE
      INTEGER TYPE

      CHARACTER*(IOSIZE) INFOBUF

100   FORMAT('MPITEST_dataTemplate_init(): Invalid input type: ',
     & INT_FMT)

      IF (TYPE .EQ. MPITEST_INTEGER) THEN
         CALL MPITEST_INIT_INT_BUF(BUFFER, VALUE)
      ELSE IF (TYPE .EQ. MPITEST_REAL) THEN
         CALL MPITEST_INIT_REAL_BUF(BUFFER, VALUE)
      ELSE IF (TYPE .EQ. MPITEST_DOUBLE_PRECISION) THEN
         CALL MPITEST_INIT_DOUBLE_BUF(BUFFER, VALUE)
      ELSE IF (TYPE .EQ. MPITEST_COMPLEX) THEN
         CALL MPITEST_INIT_COMPLEX_BUF(BUFFER, VALUE)
      ELSE IF (TYPE .EQ. MPITEST_LOGICAL) THEN
         CALL MPITEST_INIT_LOGICAL_BUF(BUFFER, VALUE)
      ELSE IF (TYPE .EQ. MPITEST_CHARACTER) THEN
         CALL MPITEST_INIT_CHAR_BUF(BUFFER, VALUE)

#ifdef MPITEST_FREAL2_DEF
      ELSE IF (TYPE .EQ. MPITEST_REAL2) THEN
         CALL MPITEST_INIT_REAL2_BUF(BUFFER, VALUE)
#endif

#ifdef MPITEST_FREAL4_DEF
      ELSE IF (TYPE .EQ. MPITEST_REAL4) THEN
         CALL MPITEST_INIT_REAL4_BUF(BUFFER, VALUE)
#endif

#ifdef MPITEST_FREAL8_DEF
      ELSE IF (TYPE .EQ. MPITEST_REAL8) THEN
         CALL MPITEST_INIT_REAL8_BUF(BUFFER, VALUE)
#endif

#ifdef MPITEST_FINTEGER1_DEF
      ELSE IF (TYPE .EQ. MPITEST_INTEGER1) THEN
         CALL MPITEST_INIT_INT1_BUF(BUFFER, VALUE)
#endif

#ifdef MPITEST_FINTEGER2_DEF
      ELSE IF (TYPE .EQ. MPITEST_INTEGER2) THEN
         CALL MPITEST_INIT_INT2_BUF(BUFFER, VALUE)
#endif

#ifdef MPITEST_FINTEGER4_DEF
      ELSE IF (TYPE .EQ. MPITEST_INTEGER4) THEN
         CALL MPITEST_INIT_INT4_BUF(BUFFER, VALUE)
#endif

#ifdef MPITEST_FDOUBLE_COMPLEX_DEF
      ELSE IF (TYPE .EQ. MPITEST_DOUBLE_COMPLEX) THEN
         CALL MPITEST_INIT_DC_BUF(BUFFER,
     &                                  VALUE)
#endif

      ELSE
         WRITE (INFOBUF, 100) TYPE
         CALL MPITEST_MESSAGE(MPITEST_FATAL, INFOBUF)
      END IF

      END


      SUBROUTINE INIT_INT_BUF_POS(BUFFER, VALUE, I)
C     ******************************************************************
C
C
C     ******************************************************************
C
      INCLUDE 'mpitestf.h'

      INTEGER BUFFER(*)
      INTEGER VALUE, I

      BUFFER(I) = VALUE

      END


#ifdef MPITEST_FINTEGER1_DEF
      SUBROUTINE INIT_INT1_BUF_POS(BUFFER, VALUE, I)
C     ******************************************************************
C
C
C     ******************************************************************
C
      INCLUDE 'mpitestf.h'

      INTEGER*1 BUFFER(*)
      INTEGER I, VALUE

      BUFFER(I) = VALUE

      END
#endif


#ifdef MPITEST_FINTEGER2_DEF
      SUBROUTINE INIT_INT2_BUF_POS(BUFFER, VALUE, I)
C     ******************************************************************
C
C
C     ******************************************************************
C
      INCLUDE 'mpitestf.h'

      INTEGER*2 BUFFER(*)
      INTEGER I, VALUE

      BUFFER(I) = VALUE

      END
#endif


#ifdef MPITEST_FINTEGER4_DEF
      SUBROUTINE INIT_INT4_BUF_POS(BUFFER, VALUE, I)
C     ******************************************************************
C
C
C     ******************************************************************
C
      INCLUDE 'mpitestf.h'

      INTEGER*4 BUFFER(*)
      INTEGER I, VALUE

      BUFFER(I) = VALUE

      END
#endif


      SUBROUTINE INIT_REAL_BUF_POS(BUFFER, VALUE, I)
C     ******************************************************************
C
C
C     ******************************************************************
C
      INCLUDE 'mpitestf.h'

      REAL BUFFER(*)

      INTEGER I, VALUE

      BUFFER(I) = REAL(VALUE)

      END


#ifdef MPITEST_FREAL2_DEF
      SUBROUTINE INIT_REAL2_BUF_POS(BUFFER, VALUE, I)
C     ******************************************************************
C
C
C     ******************************************************************
C
      INCLUDE 'mpitestf.h'

      REAL*2 BUFFER(*)

      INTEGER I, VALUE

      BUFFER(I) = REAL(VALUE)

      END
#endif


#ifdef MPITEST_FREAL4_DEF
      SUBROUTINE INIT_REAL4_BUF_POS(BUFFER, VALUE, I)
C     ******************************************************************
C
C
C     ******************************************************************
C
      INCLUDE 'mpitestf.h'

      REAL*4 BUFFER(*)

      INTEGER I, VALUE

      BUFFER(I) = REAL(VALUE)

      END
#endif


#ifdef MPITEST_FREAL8_DEF
      SUBROUTINE INIT_REAL8_BUF_POS(BUFFER, VALUE, I)
C     ******************************************************************
C
C
C     ******************************************************************
C
      INCLUDE 'mpitestf.h'

      REAL*8 BUFFER(*)

      INTEGER I, VALUE

      BUFFER(I) = REAL(VALUE)

      END
#endif


      SUBROUTINE INIT_COMPLEX_BUF_POS(BUFFER, VALUE, I)
C     ******************************************************************
C
C
C     ******************************************************************
C
      INCLUDE 'mpitestf.h'

      COMPLEX BUFFER(*)
      INTEGER I, VALUE

      BUFFER(I) = CMPLX(VALUE)

      END


#ifdef MPITEST_FDOUBLE_COMPLEX_DEF
      SUBROUTINE INIT_DCOMPLEX_BUF_POS(BUFFER, VALUE, I)
C     ******************************************************************
C
C
C     ******************************************************************
C
      INCLUDE 'mpitestf.h'

      DOUBLE COMPLEX BUFFER(*)
      INTEGER I, VALUE

      BUFFER(I) = CMPLX(VALUE)

      END
#endif


      SUBROUTINE INIT_LOGICAL_BUF_POS(BUFFER, VALUE, I)
C     ******************************************************************
C
C
C     ******************************************************************
C
      INCLUDE 'mpitestf.h'

      LOGICAL BUFFER(*)
      INTEGER I, VALUE

      CALL MPITEST_INT_TO_LOGICAL(VALUE, BUFFER(I))

C      J = MOD(ABS(VALUE), 2)
C
C      IF (J .EQ. 1) THEN
C         BUFFER(I) = .TRUE.
C      ELSE
C         BUFFER(I) = .FALSE.
C      END IF
C
      END

      SUBROUTINE INIT_CHARACTER_BUF_POS(BUFFER, VALUE, I)
C     ******************************************************************
C
C
C     ******************************************************************
C
      INCLUDE 'mpitestf.h'

      CHARACTER BUFFER *(*)
      INTEGER I, VALUE

      BUFFER(I:I) = CHAR(VALUE)

      END


      SUBROUTINE INIT_DOUBLE_BUF_POS(BUFFER, VALUE, I)
C     ******************************************************************
C
C
C     ******************************************************************
C
      INCLUDE 'mpitestf.h'

      DOUBLE PRECISION BUFFER(*)
      INTEGER I, VALUE

      BUFFER(I) = DBLE(VALUE)

      END


      SUBROUTINE MPITEST_DATATEMPLATE_INIT_POS(BUFFER, VALUE, I, TYPE)
C     ******************************************************************
C
C
C     ******************************************************************
C
      INCLUDE 'mpitestf.h'

      MPITEST_BUF_TYPE BUFFER(*)
      INTEGER TYPE, I, VALUE

      CHARACTER*(IOSIZE) INFOBUF

100   FORMAT('MPITEST_dataTemplate_init_pos(): Invalid input type: ',
     & INT_FMT)

      IF (TYPE .EQ. MPITEST_INTEGER) THEN
         CALL INIT_INT_BUF_POS(BUFFER, VALUE, I)
      ELSE IF (TYPE .EQ. MPITEST_REAL) THEN
         CALL INIT_REAL_BUF_POS(BUFFER, VALUE, I)
      ELSE IF (TYPE .EQ. MPITEST_DOUBLE_PRECISION) THEN
         CALL INIT_DOUBLE_BUF_POS(BUFFER, VALUE, I)
      ELSE IF (TYPE .EQ. MPITEST_COMPLEX) THEN
         CALL INIT_COMPLEX_BUF_POS(BUFFER, VALUE, I)
      ELSE IF (TYPE .EQ. MPITEST_LOGICAL) THEN
         CALL INIT_LOGICAL_BUF_POS(BUFFER, VALUE, I)
      ELSE IF (TYPE .EQ. MPITEST_CHARACTER) THEN
         CALL INIT_CHARACTER_BUF_POS(BUFFER, VALUE, I)

#ifdef MPITEST_FINTEGER1_DEF
      ELSE IF (TYPE .EQ. MPITEST_INTEGER1) THEN
         CALL INIT_INT1_BUF_POS(BUFFER, VALUE, I)
#endif

#ifdef MPITEST_FINTEGER2_DEF
      ELSE IF (TYPE .EQ. MPITEST_INTEGER2) THEN
         CALL INIT_INT2_BUF_POS(BUFFER, VALUE, I)
#endif

#ifdef MPITEST_FINTEGER4_DEF
      ELSE IF (TYPE .EQ. MPITEST_INTEGER4) THEN
         CALL INIT_INT4_BUF_POS(BUFFER, VALUE, I)
#endif

#ifdef MPITEST_FREAL2_DEF
      ELSE IF (TYPE .EQ. MPITEST_REAL2) THEN
         CALL INIT_REAL2_BUF_POS(BUFFER, VALUE, I)
#endif

#ifdef MPITEST_FREAL4_DEF
      ELSE IF (TYPE .EQ. MPITEST_REAL4) THEN
         CALL INIT_REAL4_BUF_POS(BUFFER, VALUE, I)
#endif

#ifdef MPITEST_FREAL8_DEF
      ELSE IF (TYPE .EQ. MPITEST_REAL8) THEN
         CALL INIT_REAL8_BUF_POS(BUFFER, VALUE, I)
#endif

#ifdef MPITEST_FDOUBLE_COMPLEX_DEF
      ELSE IF (TYPE .EQ. MPITEST_DOUBLE_COMPLEX) THEN
         CALL INIT_DCOMPLEX_BUF_POS(BUFFER, VALUE, I)
#endif

      ELSE
         WRITE (INFOBUF, 100) TYPE
         CALL MPITEST_MESSAGE(MPITEST_FATAL, INFOBUF)
      END IF

      END


      SUBROUTINE MPITEST_DATATEMPLATE_INC_INIT(BUFFER, VALUE, I, TYPE)
C     ******************************************************************
C
C
C     ******************************************************************
C
      INCLUDE 'mpitestf.h'

      MPITEST_BUF_TYPE BUFFER(*), VALUE
      INTEGER TYPE, I

      CHARACTER*(IOSIZE) INFOBUF

100   FORMAT('MPITEST_dataTemplate_inc_init(): Invalid input type: ',
     & INT_FMT)

      IF (TYPE .EQ. MPITEST_INTEGER) THEN
         CALL MPITEST_INC_INIT_INT_BUF(BUFFER, VALUE, I, I)
      ELSE IF (TYPE .EQ. MPITEST_REAL) THEN
         CALL MPITEST_INC_INIT_REAL_BUF(BUFFER, VALUE, I, I)
      ELSE IF (TYPE .EQ. MPITEST_DOUBLE_PRECISION) THEN
         CALL MPITEST_INC_INIT_DOUBLE_BUF(BUFFER, VALUE, I, I)
      ELSE IF (TYPE .EQ. MPITEST_COMPLEX) THEN
         CALL MPITEST_INC_INIT_COMPLEX_BUF(BUFFER, VALUE, I, I)
      ELSE IF (TYPE .EQ. MPITEST_LOGICAL) THEN
         CALL MPITEST_INC_INIT_LOGICAL_BUF(BUFFER, VALUE, I, I)
      ELSE IF (TYPE .EQ. MPITEST_CHARACTER) THEN
         CALL MPITEST_INC_INIT_CHAR_BUF(BUFFER, VALUE, I, I)

#ifdef MPITEST_FINTEGER1_DEF
      ELSE IF (TYPE .EQ. MPITEST_INTEGER1) THEN
         CALL MPITEST_INC_INIT_INT1_BUF(BUFFER, VALUE, I, I)
#endif

#ifdef MPITEST_FINTEGER2_DEF
      ELSE IF (TYPE .EQ. MPITEST_INTEGER2) THEN
         CALL MPITEST_INC_INIT_INT2_BUF(BUFFER, VALUE, I, I)
#endif

#ifdef MPITEST_FINTEGER4_DEF
      ELSE IF (TYPE .EQ. MPITEST_INTEGER4) THEN
         CALL MPITEST_INC_INIT_INT4_BUF(BUFFER, VALUE, I, I)
#endif

#ifdef MPITEST_FREAL2_DEF
      ELSE IF (TYPE .EQ. MPITEST_REAL2) THEN
         CALL MPITEST_INC_INIT_REAL2_BUF(BUFFER, VALUE, I, I)
#endif

#ifdef MPITEST_FREAL4_DEF
      ELSE IF (TYPE .EQ. MPITEST_REAL4) THEN
         CALL MPITEST_INC_INIT_REAL4_BUF(BUFFER, VALUE, I, I)
#endif

#ifdef MPITEST_FREAL8_DEF
      ELSE IF (TYPE .EQ. MPITEST_REAL8) THEN
         CALL MPITEST_INC_INIT_REAL8_BUF(BUFFER, VALUE, I, I)
#endif

#ifdef MPITEST_FDOUBLE_COMPLEX_DEF
      ELSE IF (TYPE .EQ. MPITEST_DOUBLE_COMPLEX) THEN
         CALL MPITEST_INC_INIT_DC_BUF(BUFFER, VALUE, I, I)
#endif

      ELSE
         WRITE (INFOBUF, 100) TYPE
         CALL MPITEST_MESSAGE(MPITEST_FATAL, INFOBUF)
      END IF

      END


      SUBROUTINE MPITEST_INIT_EACH_BUFFER(BUFFER, VALUE, TYPE, I)
C     ******************************************************************
C
C
C     ******************************************************************
      INCLUDE 'mpitestf.h'

      MPITEST_BUF_TYPE BUFFER(*), VALUE
      INTEGER TYPE, I

      CHARACTER*(IOSIZE) INFOBUF

100   FORMAT('MPITEST_INIT_EACH_BUFFER(): Invalid input type: ',
     & INT_FMT)

      IF (TYPE .EQ. MPITEST_INTEGER) THEN
         CALL MPITEST_INIT_EACH_INT_BUF(BUFFER, VALUE, I)
      ELSE IF (TYPE .EQ. MPITEST_REAL) THEN
         CALL MPITEST_INIT_EACH_REAL_BUF(BUFFER, VALUE, I)
      ELSE IF (TYPE .EQ. MPITEST_DOUBLE_PRECISION) THEN
         CALL MPITEST_INIT_EACH_DOUBLE_BUF(BUFFER, VALUE, I)
      ELSE IF (TYPE .EQ. MPITEST_COMPLEX) THEN
         CALL MPITEST_INIT_EACH_COMPLEX_BUF(BUFFER, VALUE, I)
      ELSE IF (TYPE .EQ. MPITEST_LOGICAL) THEN
         CALL MPITEST_INIT_EACH_LOGICAL_BUF(BUFFER, VALUE, I)
      ELSE IF (TYPE .EQ. MPITEST_CHARACTER) THEN
         CALL MPITEST_INIT_EACH_CHAR_BUF(BUFFER, VALUE, I)

#ifdef MPITEST_FINTEGER1_DEF
      ELSE IF (TYPE .EQ. MPITEST_INTEGER1) THEN
         CALL MPITEST_INIT_EACH_INT1_BUF(BUFFER, VALUE, I)
#endif

#ifdef MPITEST_FINTEGER2_DEF
      ELSE IF (TYPE .EQ. MPITEST_INTEGER2) THEN
         CALL MPITEST_INIT_EACH_INT2_BUF(BUFFER, VALUE, I)
#endif

#ifdef MPITEST_FINTEGER4_DEF
      ELSE IF (TYPE .EQ. MPITEST_INTEGER4) THEN
         CALL MPITEST_INIT_EACH_INT4_BUF(BUFFER, VALUE, I)
#endif

#ifdef MPITEST_FREAL2_DEF
      ELSE IF (TYPE .EQ. MPITEST_REAL2) THEN
         CALL MPITEST_INIT_EACH_REAL2_BUF(BUFFER, VALUE, I)
#endif

#ifdef MPITEST_FREAL4_DEF
      ELSE IF (TYPE .EQ. MPITEST_REAL4) THEN
         CALL MPITEST_INIT_EACH_REAL4_BUF(BUFFER, VALUE, I)
#endif

#ifdef MPITEST_FREAL8_DEF
      ELSE IF (TYPE .EQ. MPITEST_REAL8) THEN
         CALL MPITEST_INIT_EACH_REAL8_BUF(BUFFER, VALUE, I)
#endif

#ifdef MPITEST_FDOUBLE_COMPLEX_DEF
      ELSE IF (TYPE .EQ. MPITEST_DOUBLE_COMPLEX) THEN
         CALL MPITEST_INIT_EACH_DC_BUF(BUFFER, VALUE, I)
#endif

      ELSE
         WRITE (INFOBUF, 100) TYPE
         CALL MPITEST_MESSAGE(MPITEST_FATAL, INFOBUF)
      END IF

      END


      SUBROUTINE MPITEST_INIT_BUFFER(BUFFER_TYPE, LENGTH, VALUE, BUFFER)
C     ******************************************************************
C     Set the specified buffer of the specified type and the specified
C     length to the specified value.
C    
C     Arguments :
C     buffer_type         INPUT, integer specifying the type of the
C                          buffer.  Legal values are defined in
C                    
C     length              INPUT, integer length of the buffer.
C    
C     value               INPUT, value to be put into buffer.  This is
C                          declared to be of type dataTemplate.  The
C                          member of value corresponding to type buffer_type
C                          needs to have been assigned a meaningful value
C                          for this function to have meaningful results.
C     buffer              OUTPUT, the buffer to be set.  Passed in as a void
C                          pointer so that different types may all be
C                          encompassed in one call.
C    
C     This function uses a switch statement to differentiate between the
C     different allowed types.  The type casts are required so that the void
C     pointers may be dereferenced.
C     ******************************************************************
C
      INCLUDE 'mpitestf.h'
      INTEGER BUFFER_TYPE, LENGTH

      MPITEST_BUF_TYPE BUFFER(*), VALUE

      INTEGER I

      DO 100 I = 1, LENGTH, 1
         CALL MPITEST_INIT_EACH_BUFFER(BUFFER,
     &      VALUE, BUFFER_TYPE, I)
100   CONTINUE

      END

      SUBROUTINE MPITEST_INIT_EACH_BUFFER_LOC(BUFFER, VALUE, TYPE, I)
C     ******************************************************************
C
C
C     ******************************************************************
      INCLUDE 'mpitestf.h'

      MPITEST_BUF_TYPE BUFFER(*), VALUE(*)
      INTEGER TYPE, I

      CHARACTER*(IOSIZE) INFOBUF

100   FORMAT('MPITEST_INIT_EACH_BUFFER_LOC(): Invalid input type: ',
     & INT_FMT)

      IF (TYPE .EQ. MPITEST_INTEGER) THEN
         CALL MPITEST_INIT_EACH_INT_BUF_LOC(BUFFER, VALUE, I)
      ELSE IF (TYPE .EQ. MPITEST_REAL) THEN
         CALL MPITEST_INIT_EACH_REAL_BUF_LOC(BUFFER, VALUE, I)
      ELSE IF (TYPE .EQ. MPITEST_DOUBLE_PRECISION) THEN
         CALL MPITEST_INIT_EACH_DBL_BUF_LOC(BUFFER, VALUE, I)
      ELSE IF (TYPE .EQ. MPITEST_COMPLEX) THEN
         CALL MPITEST_INIT_EACH_CMPLX_BUF_LOC(BUFFER, VALUE, I)
      ELSE IF (TYPE .EQ. MPITEST_LOGICAL) THEN
         CALL MPITEST_INIT_EACH_LOGCL_BUF_LOC(BUFFER, VALUE, I)
      ELSE IF (TYPE .EQ. MPITEST_CHARACTER) THEN
         CALL MPITEST_INIT_EACH_CHAR_BUF_LOC(BUFFER, VALUE, I)

#ifdef MPITEST_FINTEGER1_DEF
      ELSE IF (TYPE .EQ. MPITEST_INTEGER1) THEN
         CALL MPITEST_INIT_EACH_INT1_BUF_LOC(BUFFER, VALUE, I)
#endif

#ifdef MPITEST_FINTEGER2_DEF
      ELSE IF (TYPE .EQ. MPITEST_INTEGER2) THEN
         CALL MPITEST_INIT_EACH_INT2_BUF_LOC(BUFFER, VALUE, I)
#endif

#ifdef MPITEST_FINTEGER4_DEF
      ELSE IF (TYPE .EQ. MPITEST_INTEGER4) THEN
         CALL MPITEST_INIT_EACH_INT4_BUF_LOC(BUFFER, VALUE, I)
#endif

#ifdef MPITEST_FREAL2_DEF
      ELSE IF (TYPE .EQ. MPITEST_REAL2) THEN
         CALL MPITEST_INIT_EACH_REAL2_BUF_LOC(BUFFER, VALUE, I)
#endif

#ifdef MPITEST_FREAL4_DEF
      ELSE IF (TYPE .EQ. MPITEST_REAL4) THEN
         CALL MPITEST_INIT_EACH_REAL4_BUF_LOC(BUFFER, VALUE, I)
#endif

#ifdef MPITEST_FREAL8_DEF
      ELSE IF (TYPE .EQ. MPITEST_REAL8) THEN
         CALL MPITEST_INIT_EACH_REAL8_BUF_LOC(BUFFER, VALUE, I)
#endif

#ifdef MPITEST_FDOUBLE_COMPLEX_DEF
      ELSE IF (TYPE .EQ. MPITEST_DOUBLE_COMPLEX) THEN
         CALL MPITEST_INIT_EACH_DC_BUF_LOC(BUFFER, VALUE, I)
#endif

      ELSE
         WRITE (INFOBUF, 100) TYPE
         CALL MPITEST_MESSAGE(MPITEST_FATAL, INFOBUF)
      END IF

      END


      SUBROUTINE MPITEST_INIT_BUFFER_LOC(BUFFER_TYPE, LENGTH,
     $                                   VALUE, BUFFER)
C     ******************************************************************
C     Set the specified buffer of the specified type and the specified
C     length to the specified value.
C    
C     Arguments :
C     buffer_type         INPUT, integer specifying the type of the
C                          buffer.  Legal values are defined in
C                    
C     length              INPUT, integer length of the buffer.
C    
C     value               INPUT, value to be put into buffer.  This is
C                          declared to be of type dataTemplate.  The
C                          member of value corresponding to type buffer_type
C                          needs to have been assigned a meaningful value
C                          for this function to have meaningful results.
C     buffer              OUTPUT, the buffer to be set.  Passed in as a void
C                          pointer so that different types may all be
C                          encompassed in one call.
C    
C     This function uses a switch statement to differentiate between the
C     different allowed types.  The type casts are required so that the void
C     pointers may be dereferenced.
C     ******************************************************************
C
      INCLUDE 'mpitestf.h'
      INTEGER BUFFER_TYPE, LENGTH

      MPITEST_BUF_TYPE BUFFER(*), VALUE(*)

      INTEGER I

      DO 100 I = 1, LENGTH, 1
         CALL MPITEST_INIT_EACH_BUFFER_LOC(BUFFER,
     &      VALUE, BUFFER_TYPE, I)
100   CONTINUE

      END


      SUBROUTINE MPITEST_INIT_BUFFER_INC(BUFFER_TYPE, LENGTH, VALUE,
     &                                   BUFFER)
C     ******************************************************************
C     Checks that the specified buffer of the specified type and the specified
C     length is set to the specified value.
C    
C     Arguments :
C     buffer_type         INPUT, integer specifying the type of the
C                          buffer.  Legal values are specified in the file
C                          include/mpitest_cfgf.h .
C    
C     length              INPUT, integer length of the buffer.
C    
C     value               INPUT, "correct" value that should be in buffer.
C                          This argument is declared as struct dataTemplate
C                          so that all types may be accomodated.
C    
C     buffer              OUTPUT, the buffer to be checked.  Passed in as a void
C                          pointer so that different types may all be
C                          encompassed in one call.
C    
C     This function uses a switch statement to differentiate between the
C     different allowed types.  The type casts are required so that the void
C     pointers may be dereferenced.
C   
C     ******************************************************************
      INCLUDE 'mpitestf.h'

      INTEGER BUFFER_TYPE, LENGTH

      MPITEST_BUF_TYPE BUFFER(*), VALUE

      INTEGER I

      DO 100 I = 1, LENGTH, 1
         CALL MPITEST_DATATEMPLATE_INC_INIT(BUFFER,
     &      VALUE, I, BUFFER_TYPE)
100   CONTINUE

      END


      SUBROUTINE MPITEST_INIT_EACH_BUFFER_V(I, J, BUFFER,
     &                                      VALUES, BUFFER_TYPE)
C     ******************************************************************
C
C
C     ******************************************************************
C
      INCLUDE 'mpitestf.h'

      MPITEST_BUF_TYPE BUFFER(*), VALUES(*)
      INTEGER I, J, BUFFER_TYPE

      CHARACTER*(IOSIZE) INFOBUF

100   FORMAT('MPITEST_INIT_EACH_BUFFER_V(): Invalid input type: ',
     & INT_FMT)

      IF (BUFFER_TYPE .EQ. MPITEST_INTEGER) THEN
         CALL MPITEST_INIT_EACH_INT_BUF_V(BUFFER, VALUES, I, J)
      ELSE IF (BUFFER_TYPE .EQ. MPITEST_REAL) THEN
         CALL MPITEST_INIT_EACH_REAL_BUF_V(BUFFER, VALUES, I, J)
      ELSE IF (BUFFER_TYPE .EQ. MPITEST_DOUBLE_PRECISION) THEN
         CALL MPITEST_INIT_EACH_DOUBLE_BUF_V(BUFFER, VALUES, I, J)
      ELSE IF (BUFFER_TYPE .EQ. MPITEST_COMPLEX) THEN
         CALL MPITEST_INIT_EACH_COMPLEX_BUF_V(BUFFER, VALUES, I, J)
      ELSE IF (BUFFER_TYPE .EQ. MPITEST_LOGICAL) THEN
         CALL MPITEST_INIT_EACH_LOGICAL_BUF_V(BUFFER, VALUES, I, J)
      ELSE IF (BUFFER_TYPE .EQ. MPITEST_CHARACTER) THEN
         CALL MPITEST_INIT_EACH_CHAR_BUF_V(BUFFER, VALUES, I, J)

#ifdef MPITEST_FINTEGER1_DEF
      ELSE IF (BUFFER_TYPE .EQ. MPITEST_INTEGER1) THEN
         CALL MPITEST_INIT_EACH_INT1_BUF_V(BUFFER, VALUES, I, J)
#endif

#ifdef MPITEST_FINTEGER2_DEF
      ELSE IF (BUFFER_TYPE .EQ. MPITEST_INTEGER2) THEN
         CALL MPITEST_INIT_EACH_INT2_BUF_V(BUFFER, VALUES, I, J)
#endif

#ifdef MPITEST_FINTEGER4_DEF
      ELSE IF (BUFFER_TYPE .EQ. MPITEST_INTEGER4) THEN
         CALL MPITEST_INIT_EACH_INT4_BUF_V(BUFFER, VALUES, I, J)
#endif

#ifdef MPITEST_FREAL2_DEF
      ELSE IF (BUFFER_TYPE .EQ. MPITEST_REAL2) THEN
         CALL MPITEST_INIT_EACH_REAL2_BUF_V(BUFFER, VALUES, I, J)
#endif

#ifdef MPITEST_FREAL4_DEF
      ELSE IF (BUFFER_TYPE .EQ. MPITEST_REAL4) THEN
         CALL MPITEST_INIT_EACH_REAL4_BUF_V(BUFFER, VALUES, I, J)
#endif

#ifdef MPITEST_FREAL8_DEF
      ELSE IF (BUFFER_TYPE .EQ. MPITEST_REAL8) THEN
         CALL MPITEST_INIT_EACH_REAL8_BUF_V(BUFFER, VALUES, I, J)
#endif

#ifdef MPITEST_FDOUBLE_COMPLEX_DEF
      ELSE IF (BUFFER_TYPE .EQ. MPITEST_DOUBLE_COMPLEX) THEN
         CALL MPITEST_INIT_EACH_DC_BUF_V(BUFFER, VALUES, I, J)
#endif

      ELSE
         WRITE (INFOBUF, 100) BUFFER_TYPE
         CALL MPITEST_MESSAGE(MPITEST_FATAL, INFOBUF)
      END IF

      END


      SUBROUTINE MPITEST_INIT_BUFFER_V(BUFFER_TYPE,
     &                                 NUMBLOCKS,
     &                                 COUNTS,
     &                                 DISPLS,
     &                                 VALUES,
     &                                 BUFFER)
C     ******************************************************************
C     Sets the value of the memory pointed to by buffer.  The type of the
C     data is specified by the buffer_type parameter.  There are numblocks
C     different chunks of data, the ith chunk has length counts[i], is located
C     at buffer+(displs[i]*extent(data type)), and gets set to values[i].
C    
C     Arguments :
C     buffer_type         INPUT, integer specifying the type of the
C                          buffer.  Legal values are defined in
C                          include/mpitest_cfgf.h .
C    
C     numblocks           INPUT, integer number of blocks of data to be set
C    
C     counts              INPUT, pointer to integer array containing the
C                          lengths of blocks. counts[i] is the length of the
C                          ith block
C    
C     displs              INPUT, pointer to integer array containing the
C                          displacements from buffer of the blocks.  displs[i]
C                          is the displacemnt of the ith block.
C    
C     values              INPUT, pointer to array of values to be put into
C                          buffer.  This array is of type dataTemplate and of
C                          length numblocks.  The member of values[i] corresponding
C                          to buffer_type must have been set to something meaningful
C                          for this routine to have meaningful results.
C    
C     buffer              OUTPUT, the buffer to be set.  Passed in as a void
C                          pointer so that different types may all be
C                          encompassed in one call.
C    
C     This function uses a switch statement to differentiate between the
C     different allowed types.  The type casts are required so that the void
C     pointers may be dereferenced.
C     ******************************************************************
C
      INCLUDE 'mpitestf.h'

      INTEGER BUFFER_TYPE, NUMBLOCKS, COUNTS(*), DISPLS(*)
      MPITEST_BUF_TYPE VALUES(*), BUFFER(*)

      INTEGER I, J

      DO 200 I = 1, NUMBLOCKS, 1
         DO 100 J = DISPLS(I) + 1, DISPLS(I) + COUNTS(I), 1
            CALL MPITEST_INIT_EACH_BUFFER_V(I, J, BUFFER,
     &      VALUES, BUFFER_TYPE)
100      CONTINUE
200   CONTINUE

      END


      SUBROUTINE MBE_HANDLE_INTEGER(I, BUFFER, EXPECTED_VALUE,
     &                              ERROR)
C     ******************************************************************
C
C     ******************************************************************
      INCLUDE 'mpitestf.h'

      INTEGER I, ERROR
      INTEGER BUFFER(*), EXPECTED_VALUE
      CHARACTER*(IOSIZE) ERR_STR

100   FORMAT ('MPITEST_BUFFER_ERRORS(): i=', INT_FMT,
     &        ', value=', INT_FMT, ', expected ', INT_FMT)

      IF (BUFFER(I) .NE. EXPECTED_VALUE) THEN
         ERR_STR=' '
         ERROR = ERROR + 1
         WRITE (ERR_STR, 100) I, BUFFER(I), EXPECTED_VALUE
         IF (ERROR .EQ. 1)
     &     CALL MPITEST_MESSAGE(MPITEST_NONFATAL, ERR_STR)
      END IF

      END


#ifdef MPITEST_FINTEGER1_DEF
      SUBROUTINE MBE_HANDLE_INTEGER1(I, BUFFER, EXPECTED_VALUE,
     &                               ERROR)
C     ******************************************************************
C
C     ******************************************************************
      INCLUDE 'mpitestf.h'

      INTEGER I, ERROR
      INTEGER*1 BUFFER(*), EXPECTED_VALUE
      CHARACTER*(IOSIZE) ERR_STR

100   FORMAT ('MPITEST_BUFFER_ERRORS(): i=', INT_FMT,
     &        ', value=', INT1_FMT, ', expected ', INT1_FMT)

      IF (BUFFER(I) .NE. EXPECTED_VALUE) THEN
         ERR_STR=' '
         ERROR = ERROR + 1
         WRITE (ERR_STR, 100) I, BUFFER(I), EXPECTED_VALUE
         IF (ERROR .EQ. 1)
     &     CALL MPITEST_MESSAGE(MPITEST_NONFATAL, ERR_STR)
      END IF

      END
#endif


#ifdef MPITEST_FINTEGER2_DEF
      SUBROUTINE MBE_HANDLE_INTEGER2(I, BUFFER, EXPECTED_VALUE,
     &                               ERROR)
C     ******************************************************************
C
C     ******************************************************************
      INCLUDE 'mpitestf.h'

      INTEGER I, ERROR
      INTEGER*2 BUFFER(*), EXPECTED_VALUE
      CHARACTER*(IOSIZE) ERR_STR

100   FORMAT ('MPITEST_BUFFER_ERRORS(): i=', INT_FMT,
     &        ', value=', INT2_FMT, ', expected ', INT2_FMT)

      IF (BUFFER(I) .NE. EXPECTED_VALUE) THEN
         ERR_STR=' '
         ERROR = ERROR + 1
         WRITE (ERR_STR, 100) I, BUFFER(I), EXPECTED_VALUE
         IF (ERROR .EQ. 1)
     &     CALL MPITEST_MESSAGE(MPITEST_NONFATAL, ERR_STR)
      END IF

      END
#endif


#ifdef MPITEST_FINTEGER4_DEF
      SUBROUTINE MBE_HANDLE_INTEGER4(I, BUFFER, EXPECTED_VALUE,
     &                               ERROR)
C     ******************************************************************
C
C     ******************************************************************
      INCLUDE 'mpitestf.h'

      INTEGER I, ERROR
      INTEGER*4 BUFFER(*), EXPECTED_VALUE
      CHARACTER*(IOSIZE) ERR_STR

100   FORMAT ('MPITEST_BUFFER_ERRORS(): i=', INT_FMT,
     &        ', value=', INT4_FMT, ', expected ', INT4_FMT)

      IF (BUFFER(I) .NE. EXPECTED_VALUE) THEN
         ERR_STR=' '
         ERROR = ERROR + 1
         WRITE (ERR_STR, 100) I, BUFFER(I), EXPECTED_VALUE
         IF (ERROR .EQ. 1)
     &     CALL MPITEST_MESSAGE(MPITEST_NONFATAL, ERR_STR)
      END IF

      END
#endif


      SUBROUTINE MBE_HANDLE_REAL(I, BUFFER, EXPECTED_VALUE,
     &                           ERROR)
C     ******************************************************************
C
C     ******************************************************************
      INCLUDE 'mpitestf.h'

      INTEGER I, ERROR
      REAL BUFFER(*), EXPECTED_VALUE
      CHARACTER*(IOSIZE) ERR_STR

100   FORMAT ('MPITEST_BUFFER_ERRORS(): i=', INT_FMT,
     &        ', value=', REAL_FMT, ', expected ', REAL_FMT)

      IF (BUFFER(I) .NE. EXPECTED_VALUE) THEN
         ERR_STR=' '
         ERROR = ERROR + 1
         WRITE (ERR_STR, 100) I, BUFFER(I), EXPECTED_VALUE
         IF (ERROR .EQ. 1)
     &     CALL MPITEST_MESSAGE(MPITEST_NONFATAL, ERR_STR)
      END IF

      END


#ifdef MPITEST_FREAL2_DEF
      SUBROUTINE MBE_HANDLE_REAL2(I, BUFFER, EXPECTED_VALUE,
     &                            ERROR)
C     ******************************************************************
C
C     ******************************************************************
      INCLUDE 'mpitestf.h'

      INTEGER I, ERROR
      REAL*2 BUFFER(*), EXPECTED_VALUE
      CHARACTER*(IOSIZE) ERR_STR

100   FORMAT ('MPITEST_BUFFER_ERRORS(): i=', INT_FMT,
     &        ', value=', REAL2_FMT, ', expected ', REAL2_FMT)

      IF (BUFFER(I) .NE. EXPECTED_VALUE) THEN
         ERR_STR=' '
         ERROR = ERROR + 1
         WRITE (ERR_STR, 100) I, BUFFER(I), EXPECTED_VALUE
         IF (ERROR .EQ. 1)
     &     CALL MPITEST_MESSAGE(MPITEST_NONFATAL, ERR_STR)
      END IF

      END
#endif


#ifdef MPITEST_FREAL4_DEF
      SUBROUTINE MBE_HANDLE_REAL4(I, BUFFER, EXPECTED_VALUE,
     &                            ERROR)
C     ******************************************************************
C
C     ******************************************************************
      INCLUDE 'mpitestf.h'

      INTEGER I, ERROR
      REAL*4 BUFFER(*), EXPECTED_VALUE
      CHARACTER*(IOSIZE) ERR_STR

100   FORMAT ('MPITEST_BUFFER_ERRORS(): i=', INT_FMT,
     &        ', value=', REAL4_FMT, ', expected ', REAL4_FMT)

      IF (BUFFER(I) .NE. EXPECTED_VALUE) THEN
         ERR_STR=' '
         ERROR = ERROR + 1
         WRITE (ERR_STR, 100) I, BUFFER(I), EXPECTED_VALUE
         IF (ERROR .EQ. 1)
     &     CALL MPITEST_MESSAGE(MPITEST_NONFATAL, ERR_STR)
      END IF

      END
#endif


#ifdef MPITEST_FREAL8_DEF
      SUBROUTINE MBE_HANDLE_REAL8(I, BUFFER, EXPECTED_VALUE,
     &                            ERROR)
C     ******************************************************************
C
C     ******************************************************************
      INCLUDE 'mpitestf.h'

      INTEGER I, ERROR
      REAL*8 BUFFER(*), EXPECTED_VALUE
      CHARACTER*(IOSIZE) ERR_STR

100   FORMAT ('MPITEST_BUFFER_ERRORS(): i=', INT_FMT,
     &        ', value=', REAL8_FMT, ', expected ', REAL8_FMT)

      IF (BUFFER(I) .NE. EXPECTED_VALUE) THEN
         ERR_STR=' '
         ERROR = ERROR + 1
         WRITE (ERR_STR, 100) I, BUFFER(I), EXPECTED_VALUE
         IF (ERROR .EQ. 1)
     &     CALL MPITEST_MESSAGE(MPITEST_NONFATAL, ERR_STR)
      END IF

      END
#endif


      SUBROUTINE MBE_HANDLE_DOUBLE(I, BUFFER, EXPECTED_VALUE,
     &                             ERROR)
C     ******************************************************************
C
C     ******************************************************************
      INCLUDE 'mpitestf.h'

      INTEGER I, ERROR
      DOUBLE PRECISION BUFFER(*), EXPECTED_VALUE
      CHARACTER*(IOSIZE) ERR_STR

100   FORMAT ('MPITEST_BUFFER_ERRORS(): i=', INT_FMT,
     &        ', value=', DOUBLE_FMT, ', expected ', DOUBLE_FMT)

      IF (BUFFER(I) .NE. EXPECTED_VALUE) THEN
         ERR_STR=' '
         ERROR = ERROR + 1
         WRITE (ERR_STR, 100) I, BUFFER(I), EXPECTED_VALUE
         IF (ERROR .EQ. 1)
     &     CALL MPITEST_MESSAGE(MPITEST_NONFATAL, ERR_STR)
      END IF

      END


      SUBROUTINE MBE_HANDLE_COMPLEX(I, BUFFER, EXPECTED_VALUE,
     &                              ERROR)
C     ******************************************************************
C
C     ******************************************************************
      INCLUDE 'mpitestf.h'

      INTEGER I, ERROR
      COMPLEX BUFFER(*), EXPECTED_VALUE
      CHARACTER*(IOSIZE) ERR_STR

100   FORMAT ('MPITEST_BUFFER_ERRORS(): i=', INT_FMT,
     &        ', value=', COMPLEX_FMT,
     &        ', expected ', COMPLEX_FMT)

      IF (BUFFER(I) .NE. EXPECTED_VALUE) THEN
         ERR_STR=' '
         ERROR = ERROR + 1
         WRITE (ERR_STR, 100) I, BUFFER(I), EXPECTED_VALUE
         IF (ERROR .EQ. 1)
     &     CALL MPITEST_MESSAGE(MPITEST_NONFATAL, ERR_STR)
      END IF

      END


#ifdef MPITEST_FDOUBLE_COMPLEX_DEF
      SUBROUTINE MBE_HANDLE_DCOMPLEX(I, BUFFER, EXPECTED_VALUE,
     &                               ERROR)
C     ******************************************************************
C
C     ******************************************************************
      INCLUDE 'mpitestf.h'

      INTEGER I, ERROR
      DOUBLE COMPLEX BUFFER(*), EXPECTED_VALUE
      CHARACTER*(IOSIZE) ERR_STR

100   FORMAT ('MPITEST_BUFFER_ERRORS(): i=', INT_FMT,
     &        ', value=', DCOMPLEX_FMT,
     &        ', expected ', DCOMPLEX_FMT)

      IF (BUFFER(I) .NE. EXPECTED_VALUE) THEN
         ERR_STR=' '
         ERROR = ERROR + 1
         WRITE (ERR_STR, 100) I, BUFFER(I), EXPECTED_VALUE
         IF (ERROR .EQ. 1)
     &     CALL MPITEST_MESSAGE(MPITEST_NONFATAL, ERR_STR)
      END IF

      END
#endif


      SUBROUTINE MBE_HANDLE_LOGICAL(I, BUFFER, EXPECTED_VALUE,
     &                              ERROR)
C     ******************************************************************
C
C     ******************************************************************
      INCLUDE 'mpitestf.h'

      INTEGER I, ERROR
      LOGICAL BUFFER(*), EXPECTED_VALUE
      CHARACTER*(IOSIZE) ERR_STR

100   FORMAT ('MPITEST_BUFFER_ERRORS(): i=', INT_FMT,
     &        ', value=', LOG_FMT, ', expected ', LOG_FMT)

      IF (BUFFER(I) .NEQV. EXPECTED_VALUE) THEN
         ERR_STR=' '
         ERROR = ERROR + 1
         WRITE (ERR_STR, 100) I,BUFFER(I), EXPECTED_VALUE
         IF (ERROR .EQ. 1)
     &     CALL MPITEST_MESSAGE(MPITEST_NONFATAL, ERR_STR)
      END IF

      END


      SUBROUTINE MBE_HANDLE_CHARACTER(I, BUFFER, EXPECTED_VALUE,
     &                                ERROR)
C     ******************************************************************
C
C     ******************************************************************
      INCLUDE 'mpitestf.h'

      INTEGER I, ERROR
      CHARACTER BUFFER *(*), EXPECTED_VALUE
      CHARACTER*(IOSIZE) ERR_STR

100   FORMAT ('MPITEST_BUFFER_ERRORS(): i=', INT_FMT,
     &        ', value=', CHAR_FMT, ', expected ', CHAR_FMT)

      IF (BUFFER(I:I) .NE. EXPECTED_VALUE) THEN
         ERR_STR=' '
         ERROR = ERROR + 1
         WRITE (ERR_STR, 100) I, BUFFER(I:I), EXPECTED_VALUE
         IF (ERROR .EQ. 1)
     &     CALL MPITEST_MESSAGE(MPITEST_NONFATAL, ERR_STR)
      END IF

      END


      SUBROUTINE MPITEST_BUFFER_ERRORS(BUFFER_TYPE,
     &                                 LENGTH,
     &                                 VALUE,
     &                                 BUFFER,
     &                                 ERROR)
C     ******************************************************************
C     Checks that the specified buffer of the specified type and the specified
C     length is set to the specified value.
C    
C     Arguments :
C     buffer_type         INPUT, integer specifying the type of the
C                          buffer.  Legal values are specified in the file
C                          include/mpitest_cfgf.h .
C
C     length              INPUT, integer length of the buffer.
C
C     value               INPUT, "correct" value that should be in buffer.
C                          This argument is declared as struct dataTemplate
C                          so that all types may be accomodated.
C    
C     buffer              OUTPUT, the buffer to be checked.  Passed in as a void
C                          pointer so that different types may all be
C                          encompassed in one call.
C    
C     This function uses a switch statement to differentiate between the
C     different allowed types.  The type casts are required so that the void
C     pointers may be dereferenced.
C     ******************************************************************
C
      INCLUDE 'mpitestf.h'

      INTEGER BUFFER_TYPE, LENGTH, ERROR

      MPITEST_BUF_TYPE VALUE, BUFFER(*)

      CHARACTER*(IOSIZE) INFOBUF
      INTEGER I

      ERROR = 0

      DO 200 I = 1, LENGTH, 1
         IF (BUFFER_TYPE .EQ. MPITEST_INTEGER) THEN
            CALL MBE_HANDLE_INTEGER(I, BUFFER,
     &                              VALUE, ERROR)
         ELSE IF (BUFFER_TYPE .EQ. MPITEST_REAL) THEN
            CALL MBE_HANDLE_REAL(I, BUFFER,
     &                           VALUE, ERROR)
         ELSE IF (BUFFER_TYPE .EQ. MPITEST_DOUBLE_PRECISION) THEN
            CALL MBE_HANDLE_DOUBLE(I, BUFFER,
     &                             VALUE, ERROR)
         ELSE IF (BUFFER_TYPE .EQ. MPITEST_COMPLEX) THEN
            CALL MBE_HANDLE_COMPLEX(I, BUFFER,
     &                              VALUE, ERROR)
         ELSE IF (BUFFER_TYPE .EQ. MPITEST_LOGICAL) THEN
            CALL MBE_HANDLE_LOGICAL(I, BUFFER,
     &                              VALUE, ERROR)
         ELSE IF (BUFFER_TYPE .EQ. MPITEST_CHARACTER) THEN
            CALL MBE_HANDLE_CHARACTER(I, BUFFER,
     &                                VALUE, ERROR)

#ifdef MPITEST_FINTEGER1_DEF
         ELSE IF (BUFFER_TYPE .EQ. MPITEST_INTEGER1) THEN
            CALL MBE_HANDLE_INTEGER1(I, BUFFER,
     &                               VALUE, ERROR)
#endif

#ifdef MPITEST_FINTEGER2_DEF
         ELSE IF (BUFFER_TYPE .EQ. MPITEST_INTEGER2) THEN
            CALL MBE_HANDLE_INTEGER2(I, BUFFER,
     &                               VALUE, ERROR)
#endif

#ifdef MPITEST_FINTEGER4_DEF
         ELSE IF (BUFFER_TYPE .EQ. MPITEST_INTEGER4) THEN
            CALL MBE_HANDLE_INTEGER4(I, BUFFER,
     &                               VALUE, ERROR)
#endif

#ifdef MPITEST_FREAL2_DEF
         ELSE IF (BUFFER_TYPE .EQ. MPITEST_REAL2) THEN
            CALL MBE_HANDLE_REAL2(I, BUFFER,
     &                            VALUE, ERROR)
#endif

#ifdef MPITEST_FREAL4_DEF
         ELSE IF (BUFFER_TYPE .EQ. MPITEST_REAL4) THEN
            CALL MBE_HANDLE_REAL4(I, BUFFER,
     &                            VALUE, ERROR)
#endif

#ifdef MPITEST_FREAL8_DEF
         ELSE IF (BUFFER_TYPE .EQ. MPITEST_REAL8) THEN
            CALL MBE_HANDLE_REAL8(I, BUFFER,
     &                            VALUE, ERROR)
#endif

#ifdef MPITEST_FDOUBLE_COMPLEX_DEF
         ELSE IF (BUFFER_TYPE .EQ. MPITEST_DOUBLE_COMPLEX) THEN
            CALL MBE_HANDLE_DCOMPLEX(I, BUFFER,
     &                               VALUE, ERROR)
#endif

         END IF

200   CONTINUE

      END


      SUBROUTINE MBEL_HANDLE_INTEGER(I, BUFFER, EXPECTED_VALUE,
     &                               ERROR)
C     ******************************************************************
C
C     ******************************************************************
      INCLUDE 'mpitestf.h'

      INTEGER I, ERROR
      INTEGER BUFFER(*), EXPECTED_VALUE(2)
      CHARACTER*(IOSIZE) ERR_STR

100   FORMAT ('MPITEST_BUFFER_ERRORS_LOC(): i=', INT_FMT,
     &        ', value=', INT_FMT, ', expected(1) ', INT_FMT)

200   FORMAT ('MPITEST_BUFFER_ERRORS_LOC(): i=', INT_FMT,
     &        ', value=', INT_FMT, ', expected(2) ', INT_FMT)

      IF (BUFFER(2*I - 1) .NE. EXPECTED_VALUE(1)) THEN
         ERR_STR=' '
         ERROR = ERROR + 1
         WRITE (ERR_STR, 100) I, BUFFER(I), EXPECTED_VALUE(1)
         IF (ERROR .EQ. 1)
     &     CALL MPITEST_MESSAGE(MPITEST_NONFATAL, ERR_STR)
      END IF

      IF (BUFFER(2*I) .NE. EXPECTED_VALUE(2)) THEN
         ERR_STR=' '
         ERROR = ERROR + 1
         WRITE (ERR_STR, 200) I, BUFFER(I), EXPECTED_VALUE(2)
         IF (ERROR .EQ. 1)
     &     CALL MPITEST_MESSAGE(MPITEST_NONFATAL, ERR_STR)
      END IF

      END


      SUBROUTINE MBEL_HANDLE_REAL(I, BUFFER, EXPECTED_VALUE,
     &                            ERROR)
C     ******************************************************************
C
C     ******************************************************************
      INCLUDE 'mpitestf.h'

      INTEGER I, ERROR
      REAL BUFFER(*), EXPECTED_VALUE(2)
      CHARACTER*(IOSIZE) ERR_STR

100   FORMAT ('MPITEST_BUFFER_ERRORS_LOC(): i=', INT_FMT,
     &        ', value=', REAL_FMT, ', expected(1) ', REAL_FMT)

200   FORMAT ('MPITEST_BUFFER_ERRORS_LOC(): i=', INT_FMT,
     &        ', value=', REAL_FMT, ', expected(2) ', REAL_FMT)

      IF (BUFFER(2*I - 1) .NE. EXPECTED_VALUE(1)) THEN
         ERR_STR=' '
         ERROR = ERROR + 1
         WRITE (ERR_STR, 100) I, BUFFER(I), EXPECTED_VALUE(1)
         IF (ERROR .EQ. 1)
     &     CALL MPITEST_MESSAGE(MPITEST_NONFATAL, ERR_STR)
      END IF

      IF (BUFFER(2*I) .NE. EXPECTED_VALUE(2)) THEN
         ERR_STR=' '
         ERROR = ERROR + 1
         WRITE (ERR_STR, 200) I, BUFFER(I), EXPECTED_VALUE(2)
         IF (ERROR .EQ. 1)
     &     CALL MPITEST_MESSAGE(MPITEST_NONFATAL, ERR_STR)
      END IF

      END


      SUBROUTINE MBEL_HANDLE_DOUBLE(I, BUFFER, EXPECTED_VALUE,
     &                              ERROR)
C     ******************************************************************
C
C     ******************************************************************
      INCLUDE 'mpitestf.h'

      INTEGER I, ERROR
      DOUBLE PRECISION BUFFER(*), EXPECTED_VALUE(2)
      CHARACTER*(IOSIZE) ERR_STR

100   FORMAT ('MPITEST_BUFFER_ERRORS_LOC(): i=', INT_FMT,
     &        ', value=', DOUBLE_FMT, ', expected(1) ', DOUBLE_FMT)

200   FORMAT ('MPITEST_BUFFER_ERRORS_LOC(): i=', INT_FMT,
     &        ', value=', DOUBLE_FMT, ', expected(2) ', DOUBLE_FMT)

      IF (BUFFER(2*I - 1) .NE. EXPECTED_VALUE(1)) THEN
         ERR_STR=' '
         ERROR = ERROR + 1
         WRITE (ERR_STR, 100) I, BUFFER(I), EXPECTED_VALUE(1)
         IF (ERROR .EQ. 1)
     &     CALL MPITEST_MESSAGE(MPITEST_NONFATAL, ERR_STR)
      END IF

      IF (BUFFER(2*I) .NE. EXPECTED_VALUE(2)) THEN
         ERR_STR=' '
         ERROR = ERROR + 1
         WRITE (ERR_STR, 200) I, BUFFER(I), EXPECTED_VALUE(2)
         IF (ERROR .EQ. 1)
     &     CALL MPITEST_MESSAGE(MPITEST_NONFATAL, ERR_STR)
      END IF

      END


      SUBROUTINE MPITEST_BUFFER_ERRORS_LOC(BUFFER_TYPE,
     &                                     LENGTH,
     &                                     VALUE,
     &                                     BUFFER,
     &                                     ERROR)
C     ******************************************************************
C     Checks that the specified buffer of the specified type and the specified
C     length is set to the specified value.
C    
C     Arguments :
C     buffer_type         INPUT, integer specifying the type of the
C                          buffer.  Legal values are specified in the file
C                          include/mpitest_cfgf.h .
C
C     length              INPUT, integer length of the buffer.
C
C     value               INPUT, "correct" value that should be in buffer.
C                          This argument is declared as struct dataTemplate
C                          so that all types may be accomodated.
C    
C     buffer              OUTPUT, the buffer to be checked.  Passed in as a void
C                          pointer so that different types may all be
C                          encompassed in one call.
C    
C     This function uses a switch statement to differentiate between the
C     different allowed types.  The type casts are required so that the void
C     pointers may be dereferenced.
C     ******************************************************************
C
      INCLUDE 'mpitestf.h'

      INTEGER BUFFER_TYPE, LENGTH, ERROR

      MPITEST_BUF_TYPE VALUE(*), BUFFER(*)

      CHARACTER*(IOSIZE) INFOBUF
      INTEGER I

      ERROR = 0

      DO 200 I = 1, LENGTH, 1
         IF (BUFFER_TYPE .EQ. MPITEST_INTEGER) THEN
            CALL MBEL_HANDLE_INTEGER(I, BUFFER,
     &                              VALUE, ERROR)
         ELSE IF (BUFFER_TYPE .EQ. MPITEST_REAL) THEN
            CALL MBEL_HANDLE_REAL(I, BUFFER,
     &                           VALUE, ERROR)
         ELSE IF (BUFFER_TYPE .EQ. MPITEST_DOUBLE_PRECISION) THEN
            CALL MBEL_HANDLE_DOUBLE(I, BUFFER,
     &                             VALUE, ERROR)
         END IF

200   CONTINUE

      END


      SUBROUTINE MBEV_HANDLE_INTEGER(I, J, BUFFER, EXPECTED_VALUE,
     &                               ERROR)
C     ******************************************************************
C
C     ******************************************************************
      INCLUDE 'mpitestf.h'

      INTEGER I, J, ERROR
      INTEGER BUFFER(*), EXPECTED_VALUE(*)
      CHARACTER*(IOSIZE) ERR_STR

100   FORMAT ('MPITEST_BUFFER_ERRORS_V(): i,j=', INT_FMT,
     &        ', ', INT_FMT,  ', value=', INT_FMT,
     &        ', expected ', INT_FMT)

      IF (BUFFER(J) .NE. EXPECTED_VALUE(I)) THEN
         ERR_STR=' '
         ERROR = ERROR + 1
         WRITE (ERR_STR, 100) I, J, BUFFER(J), EXPECTED_VALUE(I)
         IF (ERROR .EQ. 1)
     &     CALL MPITEST_MESSAGE(MPITEST_NONFATAL, ERR_STR)
      END IF

      END


#ifdef MPITEST_FINTEGER1_DEF
      SUBROUTINE MBEV_HANDLE_INTEGER1(I, J, BUFFER, EXPECTED_VALUE,
     &                                ERROR)
C     ******************************************************************
C
C     ******************************************************************
      INCLUDE 'mpitestf.h'

      INTEGER I, J, ERROR
      INTEGER*1 BUFFER(*), EXPECTED_VALUE(*)
      CHARACTER*(IOSIZE) ERR_STR

100   FORMAT ('MPITEST_BUFFER_ERRORS_V(): i,j=', INT_FMT,
     &        ', ', INT_FMT,  ', value=', INT1_FMT,
     &        ', expected ', INT1_FMT)

      IF (BUFFER(J) .NE. EXPECTED_VALUE(I)) THEN
         ERR_STR=' '
         ERROR = ERROR + 1
         WRITE (ERR_STR, 100) I, J, BUFFER(J), EXPECTED_VALUE(I)
         IF (ERROR .EQ. 1)
     &     CALL MPITEST_MESSAGE(MPITEST_NONFATAL, ERR_STR)
      END IF

      END
#endif


#ifdef MPITEST_FINTEGER2_DEF
      SUBROUTINE MBEV_HANDLE_INTEGER2(I, J, BUFFER, EXPECTED_VALUE,
     &                                ERROR)
C     ******************************************************************
C
C     ******************************************************************
      INCLUDE 'mpitestf.h'

      INTEGER I, J, ERROR
      INTEGER*2 BUFFER(*), EXPECTED_VALUE(*)
      CHARACTER*(IOSIZE) ERR_STR

100   FORMAT ('MPITEST_BUFFER_ERRORS_V(): i,j=', INT_FMT,
     &        ', ', INT_FMT,  ', value=', INT2_FMT,
     &        ', expected ', INT2_FMT)

      IF (BUFFER(J) .NE. EXPECTED_VALUE(I)) THEN
         ERR_STR=' '
         ERROR = ERROR + 1
         WRITE (ERR_STR, 100) I, J, BUFFER(J), EXPECTED_VALUE(I)
         IF (ERROR .EQ. 1)
     &     CALL MPITEST_MESSAGE(MPITEST_NONFATAL, ERR_STR)
      END IF

      END
#endif


#ifdef MPITEST_FINTEGER4_DEF
      SUBROUTINE MBEV_HANDLE_INTEGER4(I, J, BUFFER, EXPECTED_VALUE,
     &                                ERROR)
C     ******************************************************************
C
C     ******************************************************************
      INCLUDE 'mpitestf.h'

      INTEGER I, J, ERROR
      INTEGER*4 BUFFER(*), EXPECTED_VALUE(*)
      CHARACTER*(IOSIZE) ERR_STR

100   FORMAT ('MPITEST_BUFFER_ERRORS_V(): i,j=', INT_FMT,
     &        ', ', INT_FMT,  ', value=', INT4_FMT,
     &        ', expected ', INT4_FMT)

      IF (BUFFER(J) .NE. EXPECTED_VALUE(I)) THEN
         ERR_STR=' '
         ERROR = ERROR + 1
         WRITE (ERR_STR, 100) I, J, BUFFER(J), EXPECTED_VALUE(I)
         IF (ERROR .EQ. 1)
     &     CALL MPITEST_MESSAGE(MPITEST_NONFATAL, ERR_STR)
      END IF

      END
#endif


      SUBROUTINE MBEV_HANDLE_REAL(I, J, BUFFER, EXPECTED_VALUE,
     &                            ERROR)
C     ******************************************************************
C
C     ******************************************************************
      INCLUDE 'mpitestf.h'

      INTEGER I, J, ERROR
      REAL BUFFER(*), EXPECTED_VALUE(*)
      CHARACTER*(IOSIZE) ERR_STR

100   FORMAT ('MPITEST_BUFFER_ERRORS_V(): i,j=', INT_FMT, ', ',
     &        INT_FMT, ', value=', REAL_FMT, ', expected ',
     &        REAL_FMT)

      IF (BUFFER(J) .NE. EXPECTED_VALUE(I)) THEN
         ERR_STR=' '
         ERROR = ERROR + 1
         WRITE (ERR_STR, 100) I, J, BUFFER(J), EXPECTED_VALUE(I)
         IF (ERROR .EQ. 1)
     &     CALL MPITEST_MESSAGE(MPITEST_NONFATAL, ERR_STR)
      END IF

      END


#ifdef MPITEST_FREAL2_DEF
      SUBROUTINE MBEV_HANDLE_REAL2(I, J, BUFFER, EXPECTED_VALUE,
     &                             ERROR)
C     ******************************************************************
C
C     ******************************************************************
      INCLUDE 'mpitestf.h'

      INTEGER I, J, ERROR
      REAL*2 BUFFER(*), EXPECTED_VALUE(*)
      CHARACTER*(IOSIZE) ERR_STR

100   FORMAT ('MPITEST_BUFFER_ERRORS_V(): i,j=', INT_FMT, ', ',
     &        INT_FMT, ', value=', REAL2_FMT, ', expected ',
     &        REAL2_FMT)

      IF (BUFFER(J) .NE. EXPECTED_VALUE(I)) THEN
         ERR_STR=' '
         ERROR = ERROR + 1
         WRITE (ERR_STR, 100) I, J, BUFFER(J), EXPECTED_VALUE(I)
         IF (ERROR .EQ. 1)
     &     CALL MPITEST_MESSAGE(MPITEST_NONFATAL, ERR_STR)
      END IF

      END
#endif


#ifdef MPITEST_FREAL4_DEF
      SUBROUTINE MBEV_HANDLE_REAL4(I, J, BUFFER, EXPECTED_VALUE,
     &                             ERROR)
C     ******************************************************************
C
C     ******************************************************************
      INCLUDE 'mpitestf.h'

      INTEGER I, J, ERROR
      REAL*4 BUFFER(*), EXPECTED_VALUE(*)
      CHARACTER*(IOSIZE) ERR_STR

100   FORMAT ('MPITEST_BUFFER_ERRORS_V(): i,j=', INT_FMT, ', ',
     &        INT_FMT, ', value=', REAL4_FMT, ', expected ',
     &        REAL4_FMT)

      IF (BUFFER(J) .NE. EXPECTED_VALUE(I)) THEN
         ERR_STR=' '
         ERROR = ERROR + 1
         WRITE (ERR_STR, 100) I, J, BUFFER(J), EXPECTED_VALUE(I)
         IF (ERROR .EQ. 1)
     &     CALL MPITEST_MESSAGE(MPITEST_NONFATAL, ERR_STR)
      END IF

      END
#endif


#ifdef MPITEST_FREAL8_DEF
      SUBROUTINE MBEV_HANDLE_REAL8(I, J, BUFFER, EXPECTED_VALUE,
     &                             ERROR)
C     ******************************************************************
C
C     ******************************************************************
      INCLUDE 'mpitestf.h'

      INTEGER I, J, ERROR
      REAL*8 BUFFER(*), EXPECTED_VALUE(*)
      CHARACTER*(IOSIZE) ERR_STR

100   FORMAT ('MPITEST_BUFFER_ERRORS_V(): i,j=', INT_FMT, ', ',
     &        INT_FMT, ', value=', REAL8_FMT, ', expected ',
     &        REAL8_FMT)

      IF (BUFFER(J) .NE. EXPECTED_VALUE(I)) THEN
         ERR_STR=' '
         ERROR = ERROR + 1
         WRITE (ERR_STR, 100) I, J, BUFFER(J), EXPECTED_VALUE(I)
         IF (ERROR .EQ. 1)
     &     CALL MPITEST_MESSAGE(MPITEST_NONFATAL, ERR_STR)
      END IF

      END
#endif


      SUBROUTINE MBEV_HANDLE_DOUBLE(I, J, BUFFER, EXPECTED_VALUE,
     &                              ERROR)
C     ******************************************************************
C
C     ******************************************************************
      INCLUDE 'mpitestf.h'

      INTEGER I, J, ERROR
      DOUBLE PRECISION BUFFER(*), EXPECTED_VALUE(*)
      CHARACTER*(IOSIZE) ERR_STR

100   FORMAT ('MPITEST_BUFFER_ERRORS_V(): i,j=', INT_FMT, ', ',
     &        INT_FMT, ', value=', DOUBLE_FMT, ', expected ',
     &        DOUBLE_FMT)

      IF (BUFFER(J) .NE. EXPECTED_VALUE(I)) THEN
         ERR_STR=' '
         ERROR = ERROR + 1
         WRITE (ERR_STR, 100) I, J, BUFFER(J), EXPECTED_VALUE(I)
         IF (ERROR .EQ. 1)
     &     CALL MPITEST_MESSAGE(MPITEST_NONFATAL, ERR_STR)
      END IF

      END


      SUBROUTINE MBEV_HANDLE_COMPLEX(I, J, BUFFER, EXPECTED_VALUE,
     &                               ERROR)
C     ******************************************************************
C
C     ******************************************************************
      INCLUDE 'mpitestf.h'

      INTEGER I, J, ERROR
      COMPLEX BUFFER(*), EXPECTED_VALUE(*)
      CHARACTER*(IOSIZE) ERR_STR

100   FORMAT ('MPITEST_BUFFER_ERRORS_V(): i,j=', INT_FMT,
     &        ', ', INT_FMT, ', value=', COMPLEX_FMT,
     &        ', expected ', COMPLEX_FMT)

      IF (BUFFER(J) .NE. EXPECTED_VALUE(I)) THEN
         ERR_STR=' '
         ERROR = ERROR + 1
         WRITE (ERR_STR, 100) I, J, BUFFER(J), EXPECTED_VALUE(I)
         IF (ERROR .EQ. 1)
     &     CALL MPITEST_MESSAGE(MPITEST_NONFATAL, ERR_STR)
      END IF

      END


#ifdef MPITEST_FDOUBLE_COMPLEX_DEF
      SUBROUTINE MBEV_HANDLE_DCOMPLEX(I, J, BUFFER, EXPECTED_VALUE,
     &                                ERROR)
C     ******************************************************************
C
C     ******************************************************************
      INCLUDE 'mpitestf.h'

      INTEGER I, J, ERROR
      DOUBLE COMPLEX BUFFER(*), EXPECTED_VALUE(*)
      CHARACTER*(IOSIZE) ERR_STR

100   FORMAT ('MPITEST_BUFFER_ERRORS_V(): i,j=', INT_FMT,
     &        ', ', INT_FMT, ', value=', DCOMPLEX_FMT,
     &        ', expected ', DCOMPLEX_FMT)

      IF (BUFFER(J) .NE. EXPECTED_VALUE(I)) THEN
         ERR_STR=' '
         ERROR = ERROR + 1
         WRITE (ERR_STR, 100) I, J, BUFFER(J), EXPECTED_VALUE(I)
         IF (ERROR .EQ. 1)
     &     CALL MPITEST_MESSAGE(MPITEST_NONFATAL, ERR_STR)
      END IF

      END
#endif


      SUBROUTINE MBEV_HANDLE_LOGICAL(I, J, BUFFER, EXPECTED_VALUE,
     &                               ERROR)
C     ******************************************************************
C
C     ******************************************************************
      INCLUDE 'mpitestf.h'


      INTEGER I, J, ERROR
      LOGICAL BUFFER(*), EXPECTED_VALUE(*)
      CHARACTER*(IOSIZE) ERR_STR

100   FORMAT ('MPITEST_BUFFER_ERRORS_V(): i,j=', INT_FMT,
     &        ', ', INT_FMT,  ', value=', LOG_FMT,
     &        ', expected ', LOG_FMT)

      IF (BUFFER(J) .NEQV. EXPECTED_VALUE(I)) THEN
         ERR_STR=' '
         ERROR = ERROR + 1
         WRITE (ERR_STR, 100) I, J, BUFFER(J), EXPECTED_VALUE(I)
         IF (ERROR .EQ. 1)
     &     CALL MPITEST_MESSAGE(MPITEST_NONFATAL, ERR_STR)
      END IF

      END


      SUBROUTINE MBEV_HANDLE_CHARACTER(I, J, BUFFER, EXPECTED_VALUE,
     &                                 ERROR)
C     ******************************************************************
C
C     ******************************************************************
      INCLUDE 'mpitestf.h'

      INTEGER I, J, ERROR
      CHARACTER BUFFER *(*), EXPECTED_VALUE *(*)
      CHARACTER*(IOSIZE) ERR_STR

100   FORMAT ('MPITEST_BUFFER_ERRORS_V(): i,j=', INT_FMT,
     &        ', ', INT_FMT,  ', value=', CHAR_FMT,
     &        ', expected ', CHAR_FMT)

      IF (BUFFER(J:J) .NE. EXPECTED_VALUE(I:I)) THEN
         ERR_STR=' '
         ERROR = ERROR + 1
         WRITE (ERR_STR, 100) I, J, BUFFER(J:J), EXPECTED_VALUE(I:I)
         IF (ERROR .EQ. 1)
     &     CALL MPITEST_MESSAGE(MPITEST_NONFATAL, ERR_STR)
      END IF

      END


      SUBROUTINE MPITEST_BUFFER_ERRORS_V(BUFFER_TYPE,
     &                                   NUMBLOCKS, 
     &                                   COUNTS,
     &                                   DISPLS,
     &                                   VALUES,
     &                                   BUFFER,
     &                                   ERROR)
C     ******************************************************************
C     Checks the values of the memory pointed to by buffer.  The type of the
C     data is specified by the buffer_type parameter.  There are numblocks
C     different chunks of data, the ith chunk has length counts[i], is located
C     at buffer+(displs[i]*extent(data type)), and should be equal to values[i].
C
C
C     Arguments :
C     buffer_type         INPUT, integer specifying the type of the
C                          buffer.  Legal values are specified in the file
C                          include/mpitest_cfgf.h .
C    
C     numblocks           INPUT, integer number of blocks of data to be set
C    
C     counts              INPUT, pointer to integer array containing the
C                          lengths of blocks. counts[i] is the length of the
C                          ith block
C    
C     displs              INPUT, pointer to integer array containing the
C                          displacements from buffer of the blocks.  displs[i]
C                          is the displacemnt of the ith block.
C    
C     values              INPUT, pointer to array of values that should be in
C                          buffer.  This parameter is declared as an array
C                          of struct dataTemplate's so that all possible
C                          buffer types may be accomodated.
C    
C     buffer              OUTPUT, the buffer to be set.  Passed in as a void
C                          pointer so that different types may all be
C                          encompassed in one call.
C    
C     This function uses a switch statement to differentiate between the
C     different allowed types.  The type casts are required so that the void
C     pointers may be dereferenced.
C     ******************************************************************
      INCLUDE 'mpitestf.h'

      INTEGER BUFFER_TYPE, NUMBLOCKS, COUNTS(*), DISPLS(*)
      INTEGER ERROR
      MPITEST_BUF_TYPE VALUES(*), BUFFER(*)

      INTEGER I, J

      ERROR = 0

      DO 200 I = 1, NUMBLOCKS, 1
         DO 100 J = DISPLS(I) + 1, DISPLS(I) + COUNTS(I), 1

            IF (BUFFER_TYPE .EQ. MPITEST_INTEGER) THEN
               CALL MBEV_HANDLE_INTEGER(I, J, BUFFER,
     &                                   VALUES, ERROR)
            ELSE IF (BUFFER_TYPE .EQ. MPITEST_REAL) THEN
               CALL MBEV_HANDLE_REAL(I, J, BUFFER,
     &                                VALUES, ERROR)
            ELSE IF (BUFFER_TYPE .EQ. MPITEST_DOUBLE_PRECISION) THEN
               CALL MBEV_HANDLE_DOUBLE(I, J, BUFFER,
     &                                  VALUES, ERROR)
            ELSE IF (BUFFER_TYPE .EQ. MPITEST_COMPLEX) THEN
               CALL MBEV_HANDLE_COMPLEX(I, J, BUFFER,
     &                                   VALUES, ERROR)
            ELSE IF (BUFFER_TYPE .EQ. MPITEST_LOGICAL) THEN
               CALL MBEV_HANDLE_LOGICAL(I, J, BUFFER,
     &                                   VALUES, ERROR)
            ELSE IF (BUFFER_TYPE .EQ. MPITEST_CHARACTER) THEN
               CALL MBEV_HANDLE_CHARACTER(I, J, BUFFER,
     &                                     VALUES, ERROR)
#ifdef MPITEST_FINTEGER1_DEF
            ELSE IF (BUFFER_TYPE .EQ. MPITEST_INTEGER1) THEN
               CALL MBEV_HANDLE_INTEGER1(I, J, BUFFER,
     &                               VALUES, ERROR)
#endif

#ifdef MPITEST_FINTEGER2_DEF
            ELSE IF (BUFFER_TYPE .EQ. MPITEST_INTEGER2) THEN
               CALL MBEV_HANDLE_INTEGER2(I, J, BUFFER,
     &                               VALUES, ERROR)
#endif

#ifdef MPITEST_FINTEGER4_DEF
            ELSE IF (BUFFER_TYPE .EQ. MPITEST_INTEGER4) THEN
               CALL MBEV_HANDLE_INTEGER4(I, J, BUFFER,
     &                               VALUES, ERROR)
#endif

#ifdef MPITEST_FREAL2_DEF
            ELSE IF (BUFFER_TYPE .EQ. MPITEST_REAL2) THEN
               CALL MBEV_HANDLE_REAL2(I, J, BUFFER,
     &                            VALUES, ERROR)
#endif

#ifdef MPITEST_FREAL4_DEF
            ELSE IF (BUFFER_TYPE .EQ. MPITEST_REAL4) THEN
               CALL MBEV_HANDLE_REAL4(I, J, BUFFER,
     &                            VALUES, ERROR)
#endif

#ifdef MPITEST_FREAL8_DEF
            ELSE IF (BUFFER_TYPE .EQ. MPITEST_REAL8) THEN
               CALL MBEV_HANDLE_REAL8(I, J, BUFFER,
     &                            VALUES, ERROR)
#endif

#ifdef MPITEST_FDOUBLE_COMPLEX_DEF
            ELSE IF (BUFFER_TYPE .EQ. MPITEST_DOUBLE_COMPLEX) THEN
               CALL MBEV_HANDLE_DCOMPLEX(I, J, BUFFER,
     &                               VALUES, ERROR)
#endif
            END IF

100      CONTINUE
200   CONTINUE

      END


      SUBROUTINE MBEOV_HANDLE_INTEGER(I, BUFFER, EXPECTED_VALUE,
     &                               ERROR)
C     ******************************************************************
C
C     ******************************************************************
      INCLUDE 'mpitestf.h'

      INTEGER I, ERROR
      INTEGER BUFFER(*), EXPECTED_VALUE
      CHARACTER*(IOSIZE) ERR_STR

100   FORMAT ('MPITEST_BUFFER_ERRORS_OV(): i=', INT_FMT,
     &        ', value=', INT_FMT, ', expected ',
     &        INT_FMT)

      IF (BUFFER(I) .NE. EXPECTED_VALUE) THEN
         ERR_STR=' '
         ERROR = ERROR + 1
         WRITE (ERR_STR, 100) I, BUFFER(I), EXPECTED_VALUE
         IF (ERROR .EQ. 1)
     &     CALL MPITEST_MESSAGE(MPITEST_NONFATAL, ERR_STR)
      END IF

      END


#ifdef MPITEST_FINTEGER1_DEF
      SUBROUTINE MBEOV_HANDLE_INTEGER1(I, BUFFER, EXPECTED_VALUE,
     &                                ERROR)
C     ******************************************************************
C
C     ******************************************************************
      INCLUDE 'mpitestf.h'

      INTEGER I, ERROR
      INTEGER*1 BUFFER(*), EXPECTED_VALUE
      CHARACTER*(IOSIZE) ERR_STR

100   FORMAT ('MPITEST_BUFFER_ERRORS_OV(): i=', INT_FMT,
     &        ', value=', INT1_FMT, ', expected ',
     &        INT1_FMT)

      IF (BUFFER(I) .NE. EXPECTED_VALUE) THEN
         ERR_STR=' '
         ERROR = ERROR + 1
         WRITE (ERR_STR, 100) I, BUFFER(I), EXPECTED_VALUE
         IF (ERROR .EQ. 1)
     &     CALL MPITEST_MESSAGE(MPITEST_NONFATAL, ERR_STR)
      END IF

      END
#endif


#ifdef MPITEST_FINTEGER2_DEF
      SUBROUTINE MBEOV_HANDLE_INTEGER2(I, BUFFER, EXPECTED_VALUE,
     &                                ERROR)
C     ******************************************************************
C
C     ******************************************************************
      INCLUDE 'mpitestf.h'

      INTEGER I, ERROR
      INTEGER*2 BUFFER(*), EXPECTED_VALUE
      CHARACTER*(IOSIZE) ERR_STR

100   FORMAT ('MPITEST_BUFFER_ERRORS_OV(): i=', INT_FMT,
     &        ', value=', INT2_FMT, ', expected ',
     &        INT2_FMT)

      IF (BUFFER(I) .NE. EXPECTED_VALUE) THEN
         ERR_STR=' '
         ERROR = ERROR + 1
         WRITE (ERR_STR, 100) I, BUFFER(I), EXPECTED_VALUE
         IF (ERROR .EQ. 1)
     &     CALL MPITEST_MESSAGE(MPITEST_NONFATAL, ERR_STR)
      END IF

      END
#endif


#ifdef MPITEST_FINTEGER4_DEF
      SUBROUTINE MBEOV_HANDLE_INTEGER4(I, BUFFER, EXPECTED_VALUE,
     &                                ERROR)
C     ******************************************************************
C
C     ******************************************************************
      INCLUDE 'mpitestf.h'

      INTEGER I, ERROR
      INTEGER*4 BUFFER(*), EXPECTED_VALUE
      CHARACTER*(IOSIZE) ERR_STR

100   FORMAT ('MPITEST_BUFFER_ERRORS_OV(): i=', INT_FMT,
     &        ', value=', INT4_FMT, ', expected ',
     &        INT4_FMT)

      IF (BUFFER(I) .NE. EXPECTED_VALUE) THEN
         ERR_STR=' '
         ERROR = ERROR + 1
         WRITE (ERR_STR, 100) I, BUFFER(I), EXPECTED_VALUE
         IF (ERROR .EQ. 1)
     &     CALL MPITEST_MESSAGE(MPITEST_NONFATAL, ERR_STR)
      END IF

      END
#endif


      SUBROUTINE MBEOV_HANDLE_REAL(I, BUFFER, EXPECTED_VALUE,
     &                            ERROR)
C     ******************************************************************
C
C     ******************************************************************
      INCLUDE 'mpitestf.h'

      INTEGER I, ERROR
      REAL BUFFER(*), EXPECTED_VALUE
      CHARACTER*(IOSIZE) ERR_STR

100   FORMAT ('MPITEST_BUFFER_ERRORS_OV(): i=', INT_FMT,
     &        ', value=', REAL_FMT, ', expected ',
     &        REAL_FMT)

      IF (BUFFER(I) .NE. EXPECTED_VALUE) THEN
         ERR_STR=' '
         ERROR = ERROR + 1
         WRITE (ERR_STR, 100) I, BUFFER(I), EXPECTED_VALUE
         IF (ERROR .EQ. 1)
     &     CALL MPITEST_MESSAGE(MPITEST_NONFATAL, ERR_STR)
      END IF

      END


#ifdef MPITEST_FREAL2_DEF
      SUBROUTINE MBEOV_HANDLE_REAL2(I, BUFFER, EXPECTED_VALUE,
     &                             ERROR)
C     ******************************************************************
C
C     ******************************************************************
      INCLUDE 'mpitestf.h'

      INTEGER I, ERROR
      REAL*2 BUFFER(*), EXPECTED_VALUE
      CHARACTER*(IOSIZE) ERR_STR

100   FORMAT ('MPITEST_BUFFER_ERRORS_OV(): i=', INT_FMT,
     &        ', value=', REAL2_FMT, ', expected ',
     &        REAL2_FMT)

      IF (BUFFER(I) .NE. EXPECTED_VALUE) THEN
         ERR_STR=' '
         ERROR = ERROR + 1
         WRITE (ERR_STR, 100) I, BUFFER(I), EXPECTED_VALUE
         IF (ERROR .EQ. 1)
     &     CALL MPITEST_MESSAGE(MPITEST_NONFATAL, ERR_STR)
      END IF

      END
#endif


#ifdef MPITEST_FREAL4_DEF
      SUBROUTINE MBEOV_HANDLE_REAL4(I, BUFFER, EXPECTED_VALUE,
     &                             ERROR)
C     ******************************************************************
C
C     ******************************************************************
      INCLUDE 'mpitestf.h'

      INTEGER I, ERROR
      REAL*4 BUFFER(*), EXPECTED_VALUE
      CHARACTER*(IOSIZE) ERR_STR

100   FORMAT ('MPITEST_BUFFER_ERRORS_OV(): i=', INT_FMT,
     &        ', value=', REAL4_FMT, ', expected ',
     &        REAL4_FMT)

      IF (BUFFER(I) .NE. EXPECTED_VALUE) THEN
         ERR_STR=' '
         ERROR = ERROR + 1
         WRITE (ERR_STR, 100) I, BUFFER(I), EXPECTED_VALUE
         IF (ERROR .EQ. 1)
     &     CALL MPITEST_MESSAGE(MPITEST_NONFATAL, ERR_STR)
      END IF

      END
#endif


#ifdef MPITEST_FREAL8_DEF
      SUBROUTINE MBEOV_HANDLE_REAL8(I, BUFFER, EXPECTED_VALUE,
     &                             ERROR)
C     ******************************************************************
C
C     ******************************************************************
      INCLUDE 'mpitestf.h'

      INTEGER I, ERROR
      REAL*8 BUFFER(*), EXPECTED_VALUE
      CHARACTER*(IOSIZE) ERR_STR

100   FORMAT ('MPITEST_BUFFER_ERRORS_OV(): i=', INT_FMT,
     &        ', value=', REAL8_FMT, ', expected ',
     &        REAL8_FMT)

      IF (BUFFER(I) .NE. EXPECTED_VALUE) THEN
         ERR_STR=' '
         ERROR = ERROR + 1
         WRITE (ERR_STR, 100) I, BUFFER(I), EXPECTED_VALUE
         IF (ERROR .EQ. 1)
     &     CALL MPITEST_MESSAGE(MPITEST_NONFATAL, ERR_STR)
      END IF

      END
#endif


      SUBROUTINE MBEOV_HANDLE_DOUBLE(I, BUFFER, EXPECTED_VALUE,
     &                              ERROR)
C     ******************************************************************
C
C     ******************************************************************
      INCLUDE 'mpitestf.h'

      INTEGER I, ERROR
      DOUBLE PRECISION BUFFER(*), EXPECTED_VALUE
      CHARACTER*(IOSIZE) ERR_STR

100   FORMAT ('MPITEST_BUFFER_ERRORS_OV(): i=', INT_FMT,
     &        ', value=', DOUBLE_FMT, ', expected ',
     &        DOUBLE_FMT)

      IF (BUFFER(I) .NE. EXPECTED_VALUE) THEN
         ERR_STR=' '
         ERROR = ERROR + 1
         WRITE (ERR_STR, 100) I, BUFFER(I), EXPECTED_VALUE
         IF (ERROR .EQ. 1)
     &     CALL MPITEST_MESSAGE(MPITEST_NONFATAL, ERR_STR)
      END IF

      END


      SUBROUTINE MBEOV_HANDLE_COMPLEX(I, BUFFER, EXPECTED_VALUE,
     &                               ERROR)
C     ******************************************************************
C
C     ******************************************************************
      INCLUDE 'mpitestf.h'

      INTEGER I, ERROR
      COMPLEX BUFFER(*), EXPECTED_VALUE
      CHARACTER*(IOSIZE) ERR_STR

100   FORMAT ('MPITEST_BUFFER_ERRORS_OV(): i=', INT_FMT,
     &        ', value=', COMPLEX_FMT,
     &        ', expected ', COMPLEX_FMT)

      IF (BUFFER(I) .NE. EXPECTED_VALUE) THEN
         ERR_STR=' '
         ERROR = ERROR + 1
         WRITE (ERR_STR, 100) I, BUFFER(I), EXPECTED_VALUE
         IF (ERROR .EQ. 1)
     &     CALL MPITEST_MESSAGE(MPITEST_NONFATAL, ERR_STR)
      END IF

      END


#ifdef MPITEST_FDOUBLE_COMPLEX_DEF
      SUBROUTINE MBEOV_HANDLE_DCOMPLEX(I, BUFFER, EXPECTED_VALUE,
     &                                ERROR)
C     ******************************************************************
C
C     ******************************************************************
      INCLUDE 'mpitestf.h'

      INTEGER I, ERROR
      DOUBLE COMPLEX BUFFER(*), EXPECTED_VALUE
      CHARACTER*(IOSIZE) ERR_STR

100   FORMAT ('MPITEST_BUFFER_ERRORS_OV(): i=', INT_FMT,
     &        ', value=', DCOMPLEX_FMT,
     &        ', expected ', DCOMPLEX_FMT)

      IF (BUFFER(I) .NE. EXPECTED_VALUE) THEN
         ERR_STR=' '
         ERROR = ERROR + 1
         WRITE (ERR_STR, 100) I, BUFFER(I), EXPECTED_VALUE
         IF (ERROR .EQ. 1)
     &     CALL MPITEST_MESSAGE(MPITEST_NONFATAL, ERR_STR)
      END IF

      END
#endif


      SUBROUTINE MBEOV_HANDLE_LOGICAL(I, BUFFER, EXPECTED_VALUE,
     &                               ERROR)
C     ******************************************************************
C
C     ******************************************************************
      INCLUDE 'mpitestf.h'


      INTEGER I, ERROR
      LOGICAL BUFFER(*), EXPECTED_VALUE
      CHARACTER*(IOSIZE) ERR_STR

100   FORMAT ('MPITEST_BUFFER_ERRORS_OV(): i=', INT_FMT,
     &        ', value=', LOG_FMT, ', expected ',
     &        LOG_FMT)

      IF (BUFFER(I) .NEQV. EXPECTED_VALUE) THEN
         ERR_STR=' '
         ERROR = ERROR + 1
         WRITE (ERR_STR, 100) I, BUFFER(I), EXPECTED_VALUE
         IF (ERROR .EQ. 1)
     &     CALL MPITEST_MESSAGE(MPITEST_NONFATAL, ERR_STR)
      END IF

      END


      SUBROUTINE MBEOV_HANDLE_CHARACTER(I, BUFFER, EXPECTED_VALUE,
     &                                  ERROR)
C     ******************************************************************
C
C     ******************************************************************
      INCLUDE 'mpitestf.h'

      INTEGER I, ERROR
      CHARACTER BUFFER *(*), EXPECTED_VALUE
      CHARACTER*(IOSIZE) ERR_STR

100   FORMAT ('MPITEST_BUFFER_ERRORS_OV(): i=', INT_FMT,
     &        ', value=', CHAR_FMT, ', expected ',
     &        CHAR_FMT)

      IF (BUFFER(I:I) .NE. EXPECTED_VALUE) THEN
         ERR_STR=' '
         ERROR = ERROR + 1
         WRITE (ERR_STR, 100) I, BUFFER(I:I), EXPECTED_VALUE
         IF (ERROR .EQ. 1)
     &     CALL MPITEST_MESSAGE(MPITEST_NONFATAL, ERR_STR)
      END IF

      END


      SUBROUTINE MPITEST_BUFFER_ERRORS_OV(BUFFER_TYPE,
     &                                    LENGTH,
     &                                    VALUE,
     &                                    BUFFER,
     &                                    ERROR)
C     ******************************************************************
C     Checks that the specified buffer of the specified type at the specified
C     length is set to the specified value.  Typically called to check one
C     past the message length to ensure there was no overflow.
C    
C     Arguments :
C     buffer_type         INPUT, integer specifying the type of the
C                          buffer.  Legal values are specified in the file
C                          include/mpitest_cfgf.h .
C    
C     length              INPUT, integer length of the buffer.
C    
C     value               INPUT, "correct" value that should be in buffer.
C                          This argument is declared as struct dataTemplate
C                          so that all types may be accomodated.
C    
C     buffer              OUTPUT, the buffer to be checked.  Passed in as a void
C                          pointer so that different types may all be
C                          encompassed in one call.
C    
C     This function uses a switch statement to differentiate between the
C     different allowed types.  The type casts are required so that the void
C     pointers may be dereferenced.
C     ******************************************************************
C
      INCLUDE 'mpitestf.h'

      INTEGER BUFFER_TYPE, LENGTH, ERROR
      MPITEST_BUF_TYPE VALUE, BUFFER(*)

      INTEGER I

      ERROR = 0

      I = LENGTH + 1

      IF (BUFFER_TYPE .EQ. MPITEST_INTEGER) THEN
         CALL MBEOV_HANDLE_INTEGER(I, BUFFER,
     &                           VALUE, ERROR)
      ELSE IF (BUFFER_TYPE .EQ. MPITEST_REAL) THEN
         CALL MBEOV_HANDLE_REAL(I, BUFFER,
     &                        VALUE, ERROR)
      ELSE IF (BUFFER_TYPE .EQ. MPITEST_DOUBLE_PRECISION) THEN
         CALL MBEOV_HANDLE_DOUBLE(I, BUFFER,
     &                          VALUE, ERROR)
      ELSE IF (BUFFER_TYPE .EQ. MPITEST_COMPLEX) THEN
         CALL MBEOV_HANDLE_COMPLEX(I, BUFFER,
     &                           VALUE, ERROR)
      ELSE IF (BUFFER_TYPE .EQ. MPITEST_LOGICAL) THEN
         CALL MBEOV_HANDLE_LOGICAL(I, BUFFER,
     &                           VALUE, ERROR)
      ELSE IF (BUFFER_TYPE .EQ. MPITEST_CHARACTER) THEN
         CALL MBEOV_HANDLE_CHARACTER(I, BUFFER,
     &                             VALUE, ERROR)

#ifdef MPITEST_FINTEGER1_DEF
      ELSE IF (BUFFER_TYPE .EQ. MPITEST_INTEGER1) THEN
         CALL MBEOV_HANDLE_INTEGER1(I, BUFFER,
     &                              VALUE, ERROR)
#endif

#ifdef MPITEST_FINTEGER2_DEF
      ELSE IF (BUFFER_TYPE .EQ. MPITEST_INTEGER2) THEN
         CALL MBEOV_HANDLE_INTEGER2(I, BUFFER,
     &                              VALUE, ERROR)
#endif

#ifdef MPITEST_FINTEGER4_DEF
      ELSE IF (BUFFER_TYPE .EQ. MPITEST_INTEGER4) THEN
         CALL MBEOV_HANDLE_INTEGER4(I, BUFFER,
     &                              VALUE, ERROR)
#endif

#ifdef MPITEST_FREAL2_DEF
      ELSE IF (BUFFER_TYPE .EQ. MPITEST_REAL2) THEN
         CALL MBEOV_HANDLE_REAL2(I, BUFFER,
     &                           VALUE, ERROR)
#endif

#ifdef MPITEST_FREAL4_DEF
      ELSE IF (BUFFER_TYPE .EQ. MPITEST_REAL4) THEN
         CALL MBEOV_HANDLE_REAL4(I, BUFFER,
     &                           VALUE, ERROR)
#endif

#ifdef MPITEST_FREAL8_DEF
      ELSE IF (BUFFER_TYPE .EQ. MPITEST_REAL8) THEN
         CALL MBEOV_HANDLE_REAL8(I, BUFFER,
     &                           VALUE, ERROR)
#endif

#ifdef MPITEST_FDOUBLE_COMPLEX_DEF
      ELSE IF (BUFFER_TYPE .EQ. MPITEST_DOUBLE_COMPLEX) THEN
         CALL MBEOV_HANDLE_DCOMPLEX(I, BUFFER,
     &                              VALUE, ERROR)
#endif

      END IF

      END


      SUBROUTINE MBEI_HANDLE_INTEGER(I, BUFFER, EXPECTED_VALUE,
     &                               ERROR)
C     ******************************************************************
C
C     ******************************************************************
      INCLUDE 'mpitestf.h'

      INTEGER I, ERROR
      INTEGER BUFFER(*), EXPECTED_VALUE
      CHARACTER*(IOSIZE) ERR_STR

100   FORMAT ('MPITEST_BUFFER_ERRORS_INC(): i=',
     &        INT_FMT, ', value=', INT_FMT,
     &        ', expected ', INT_FMT)

      IF (BUFFER(I) .NE. EXPECTED_VALUE + I) THEN
         ERR_STR=' '
         ERROR = ERROR + 1
         WRITE (ERR_STR, 100) I, BUFFER(I), EXPECTED_VALUE + I
         IF (ERROR .EQ. 1)
     &     CALL MPITEST_MESSAGE(MPITEST_NONFATAL, ERR_STR)
      END IF

      END


#ifdef MPITEST_FINTEGER1_DEF
      SUBROUTINE MBEI_HANDLE_INTEGER1(I, BUFFER, EXPECTED_VALUE,
     &                                ERROR)
C     ******************************************************************
C
C     ******************************************************************
      INCLUDE 'mpitestf.h'

      INTEGER I, ERROR
      INTEGER*1 BUFFER(*), EXPECTED_VALUE, TEST_VAL
      CHARACTER*(IOSIZE) ERR_STR

100   FORMAT ('MPITEST_BUFFER_ERRORS_INC(): i=',
     &        INT_FMT, ', value=', INT1_FMT,
     &        ', expected ', INT1_FMT)

      TEST_VAL = EXPECTED_VALUE + I
      IF (BUFFER(I) .NE. TEST_VAL) THEN
         ERR_STR=' '
         ERROR = ERROR + 1
         WRITE (ERR_STR, 100) I, BUFFER(I), TEST_VAL
         IF (ERROR .EQ. 1)
     &     CALL MPITEST_MESSAGE(MPITEST_NONFATAL, ERR_STR)
      END IF

      END
#endif


#ifdef MPITEST_FINTEGER2_DEF
      SUBROUTINE MBEI_HANDLE_INTEGER2(I, BUFFER, EXPECTED_VALUE,
     &                                ERROR)
C     ******************************************************************
C
C     ******************************************************************
      INCLUDE 'mpitestf.h'

      INTEGER I, ERROR
      INTEGER*2 BUFFER(*), EXPECTED_VALUE, TEST_VAL
      CHARACTER*(IOSIZE) ERR_STR

100   FORMAT ('MPITEST_BUFFER_ERRORS_INC(): i=',
     &        INT_FMT, ', value=', INT2_FMT,
     &        ', expected ', INT2_FMT)

      TEST_VAL = EXPECTED_VALUE + I
      IF (BUFFER(I) .NE. TEST_VAL) THEN
         ERR_STR=' '
         ERROR = ERROR + 1
         WRITE (ERR_STR, 100) I, BUFFER(I), TEST_VAL
         IF (ERROR .EQ. 1)
     &     CALL MPITEST_MESSAGE(MPITEST_NONFATAL, ERR_STR)
      END IF

      END
#endif


#ifdef MPITEST_FINTEGER4_DEF
      SUBROUTINE MBEI_HANDLE_INTEGER4(I, BUFFER, EXPECTED_VALUE,
     &                                ERROR)
C     ******************************************************************
C
C     ******************************************************************
      INCLUDE 'mpitestf.h'

      INTEGER I, ERROR
      INTEGER*4 BUFFER(*), EXPECTED_VALUE, TEST_VAL
      CHARACTER*(IOSIZE) ERR_STR

100   FORMAT ('MPITEST_BUFFER_ERRORS_INC(): i=',
     &        INT_FMT, ', value=', INT4_FMT,
     &        ', expected ', INT4_FMT)

      TEST_VAL = EXPECTED_VALUE + I
      IF (BUFFER(I) .NE. TEST_VAL) THEN
         ERR_STR=' '
         ERROR = ERROR + 1
         WRITE (ERR_STR, 100) I, BUFFER(I), TEST_VAL
         IF (ERROR .EQ. 1)
     &     CALL MPITEST_MESSAGE(MPITEST_NONFATAL, ERR_STR)
      END IF

      END
#endif


      SUBROUTINE MBEI_HANDLE_REAL(I, BUFFER, EXPECTED_VALUE,
     &                            ERROR)
C     ******************************************************************
C
C     ******************************************************************
      INCLUDE 'mpitestf.h'

      INTEGER I, ERROR
      REAL BUFFER(*), EXPECTED_VALUE, TEST_VAL
      CHARACTER*(IOSIZE) ERR_STR

100   FORMAT ('MPITEST_BUFFER_ERRORS_INC(): i=',
     &        INT_FMT, ', value=', REAL_FMT,
     &        ', expected ', REAL_FMT)

      TEST_VAL = EXPECTED_VALUE + REAL(I)
      IF (BUFFER(I) .NE. TEST_VAL) THEN
         ERR_STR=' '
         ERROR = ERROR + 1
         WRITE (ERR_STR, 100) I, BUFFER(I), TEST_VAL
         IF (ERROR .EQ. 1)
     &     CALL MPITEST_MESSAGE(MPITEST_NONFATAL, ERR_STR)
      END IF

      END


#ifdef MPITEST_FREAL2_DEF
      SUBROUTINE MBEI_HANDLE_REAL2(I, BUFFER, EXPECTED_VALUE,
     &                             ERROR)
C     ******************************************************************
C
C     ******************************************************************
      INCLUDE 'mpitestf.h'

      INTEGER I, ERROR
      REAL*2 BUFFER(*), EXPECTED_VALUE, TEST_VAL
      CHARACTER*(IOSIZE) ERR_STR

100   FORMAT ('MPITEST_BUFFER_ERRORS_INC(): i=',
     &        INT_FMT, ', value=', REAL2_FMT,
     &        ', expected ', REAL2_FMT)

      TEST_VAL = EXPECTED_VALUE + REAL(I)
      IF (BUFFER(I) .NE. TEST_VAL) THEN
         ERR_STR=' '
         ERROR = ERROR + 1
         WRITE (ERR_STR, 100) I, BUFFER(I), TEST_VAL
         IF (ERROR .EQ. 1)
     &     CALL MPITEST_MESSAGE(MPITEST_NONFATAL, ERR_STR)
      END IF

      END
#endif


#ifdef MPITEST_FREAL4_DEF
      SUBROUTINE MBEI_HANDLE_REAL4(I, BUFFER, EXPECTED_VALUE,
     &                             ERROR)
C     ******************************************************************
C
C     ******************************************************************
      INCLUDE 'mpitestf.h'

      INTEGER I, ERROR
      REAL*4 BUFFER(*), EXPECTED_VALUE
      CHARACTER*(IOSIZE) ERR_STR

100   FORMAT ('MPITEST_BUFFER_ERRORS_INC(): i=',
     &        INT_FMT, ', value=', REAL4_FMT,
     &        ', expected ', REAL4_FMT)

      IF (BUFFER(I) .NE. EXPECTED_VALUE + REAL(I)) THEN
         ERR_STR=' '
         ERROR = ERROR + 1
         WRITE (ERR_STR, 100) I, BUFFER(I), EXPECTED_VALUE + REAL(I)
         IF (ERROR .EQ. 1)
     &     CALL MPITEST_MESSAGE(MPITEST_NONFATAL, ERR_STR)
      END IF

      END
#endif


#ifdef MPITEST_FREAL8_DEF
      SUBROUTINE MBEI_HANDLE_REAL8(I, BUFFER, EXPECTED_VALUE,
     &                             ERROR)
C     ******************************************************************
C
C     ******************************************************************
      INCLUDE 'mpitestf.h'

      INTEGER I, ERROR
      REAL*8 BUFFER(*), EXPECTED_VALUE
      CHARACTER*(IOSIZE) ERR_STR

100   FORMAT ('MPITEST_BUFFER_ERRORS_INC(): i=',
     &        INT_FMT, ', value=', REAL8_FMT,
     &        ', expected ', REAL8_FMT)

      IF (BUFFER(I) .NE. EXPECTED_VALUE + REAL(I)) THEN
         ERR_STR=' '
         ERROR = ERROR + 1
         WRITE (ERR_STR, 100) I, BUFFER(I), EXPECTED_VALUE + REAL(I)
         IF (ERROR .EQ. 1)
     &     CALL MPITEST_MESSAGE(MPITEST_NONFATAL, ERR_STR)
      END IF

      END
#endif


      SUBROUTINE MBEI_HANDLE_DOUBLE(I, BUFFER, EXPECTED_VALUE,
     &                              ERROR)
C     ******************************************************************
C
C     ******************************************************************
      INCLUDE 'mpitestf.h'

      INTEGER I, ERROR
      DOUBLE PRECISION BUFFER(*), EXPECTED_VALUE
      CHARACTER*(IOSIZE) ERR_STR

100   FORMAT ('MPITEST_BUFFER_ERRORS_INC(): i=',
     &        INT_FMT, ', value=', DOUBLE_FMT,
     &        ', expected ', DOUBLE_FMT)

      IF (BUFFER(I) .NE. EXPECTED_VALUE + DBLE(I)) THEN
         ERR_STR=' '
         ERROR = ERROR + 1
         WRITE (ERR_STR, 100) I, BUFFER(I), EXPECTED_VALUE + DBLE(I)
         IF (ERROR .EQ. 1)
     &     CALL MPITEST_MESSAGE(MPITEST_NONFATAL, ERR_STR)
      END IF

      END


      SUBROUTINE MBEI_HANDLE_COMPLEX(I, BUFFER, EXPECTED_VALUE,
     &                               ERROR)
C     ******************************************************************
C
C     ******************************************************************
      INCLUDE 'mpitestf.h'

      INTEGER I, ERROR
      COMPLEX BUFFER(*), EXPECTED_VALUE
      CHARACTER*(IOSIZE) ERR_STR

100   FORMAT ('MPITEST_BUFFER_ERRORS_INC(): i=', INT_FMT,
     &        ', value=', COMPLEX_FMT,
     &        ', expected ', COMPLEX_FMT)

      IF (BUFFER(I) .NE. EXPECTED_VALUE + CMPLX(I)) THEN
         ERR_STR=' '
         ERROR = ERROR + 1
         WRITE (ERR_STR, 100) I, BUFFER(I), EXPECTED_VALUE + CMPLX(I)
         IF (ERROR .EQ. 1)
     &     CALL MPITEST_MESSAGE(MPITEST_NONFATAL, ERR_STR)
      END IF

      END


#ifdef MPITEST_FDOUBLE_COMPLEX_DEF
      SUBROUTINE MBEI_HANDLE_DCOMPLEX(I, BUFFER, EXPECTED_VALUE,
     &                                ERROR)
C     ******************************************************************
C
C     ******************************************************************
      INCLUDE 'mpitestf.h'

      INTEGER I, ERROR
      DOUBLE COMPLEX BUFFER(*), EXPECTED_VALUE
      CHARACTER*(IOSIZE) ERR_STR

100   FORMAT ('MPITEST_BUFFER_ERRORS_INC(): i=', INT_FMT,
     &        ', value=', DCOMPLEX_FMT,
     &        ', expected ', DCOMPLEX_FMT)

      IF (BUFFER(I) .NE. EXPECTED_VALUE + CMPLX(I)) THEN
         ERR_STR=' '
         ERROR = ERROR + 1
         WRITE (ERR_STR, 100) I, BUFFER(I), EXPECTED_VALUE + CMPLX(I)
         IF (ERROR .EQ. 1)
     &     CALL MPITEST_MESSAGE(MPITEST_NONFATAL, ERR_STR)
      END IF

      END
#endif


      SUBROUTINE MBEI_HANDLE_LOGICAL(I, BUFFER, EXPECTED_VALUE,
     &                               ERROR)
C     ******************************************************************
C
C     ******************************************************************
      INCLUDE 'mpitestf.h'


      INTEGER I, ERROR, J
      LOGICAL BUFFER(*), EXPECTED_VALUE, TEST_VAL
      CHARACTER*(IOSIZE) ERR_STR

100   FORMAT ('MPITEST_BUFFER_ERRORS_INC(): i=',
     &        INT_FMT, ', value=', LOG_FMT,
     &        ', expected ', LOG_FMT)

       J = MOD(ABS(I), 2)

       IF (J .EQ. 0) THEN
          IF (EXPECTED_VALUE .EQV. .TRUE.) THEN
            TEST_VAL = .FALSE.
          ELSE
            TEST_VAL = .TRUE.
           END IF
       ELSE
          TEST_VAL = EXPECTED_VALUE
       END IF

      IF (BUFFER(I) .NEQV. TEST_VAL) THEN
         ERR_STR=' '
         ERROR = ERROR + 1
         WRITE (ERR_STR, 100) I, BUFFER(I), TEST_VAL
         IF (ERROR .EQ. 1)
     &     CALL MPITEST_MESSAGE(MPITEST_NONFATAL, ERR_STR)
      END IF

      END


      SUBROUTINE MBEI_HANDLE_CHARACTER(I, BUFFER, EXPECTED_VALUE,
     &                                 ERROR)
C     ******************************************************************
C
C     ******************************************************************
      INCLUDE 'mpitestf.h'

      INTEGER I, ERROR
      CHARACTER BUFFER *(*), EXPECTED_VALUE
      CHARACTER*(IOSIZE) ERR_STR

100   FORMAT ('MPITEST_BUFFER_ERRORS_INC(): i=',
     &        INT_FMT, ', value=', CHAR_FMT,
     &        ', expected ', CHAR_FMT)

      IF (BUFFER(I:I) .NE. CHAR(MOD(ICHAR(EXPECTED_VALUE)+I, 128))) THEN
         ERR_STR=' '
         ERROR = ERROR + 1
         WRITE (ERR_STR, 100) I, BUFFER(I:I),
     &      CHAR(MOD(ICHAR(EXPECTED_VALUE) + I,128))
         IF (ERROR .EQ. 1)
     &     CALL MPITEST_MESSAGE(MPITEST_NONFATAL, ERR_STR)
      END IF

      END


      SUBROUTINE MPITEST_BUFFER_ERRORS_INC(BUFFER_TYPE,
     &                                    LENGTH,
     &                                    VALUE,
     &                                    BUFFER,
     &                                    ERROR)
C  ******************************************************************
C  Checks that the specified buffer of the specified type and the specified
C  length is set to the specified value.
C
C  Arguments :
C  buffer_type         INPUT, integer specifying the type of the
C                       buffer.  Legal values are specified in  the file
C                       include/mpitest_cfgf.h .
C 
C  length              INPUT, integer length of the buffer.
C 
C  value               INPUT, "correct" value that should be in buffer.
C                       This argument is declared as struct dataTemplate
C                       so that all types may be accomodated.
C 
C  buffer              OUTPUT, the buffer to be checked.  Passed in as a void
C                       pointer so that different types may all be
C                       encompassed in one call.
C 
C  This function uses a switch statement to differentiate between the
C  different allowed types.  The type casts are required so that the void
C  pointers may be dereferenced.
C  ******************************************************************
      INCLUDE 'mpitestf.h'

      INTEGER BUFFER_TYPE, LENGTH, ERROR
      MPITEST_BUF_TYPE VALUE, BUFFER(*)

      INTEGER I

      ERROR = 0

      DO 100 I = 1, LENGTH, 1
         IF (BUFFER_TYPE .EQ. MPITEST_INTEGER) THEN
            CALL MBEI_HANDLE_INTEGER(I, BUFFER, VALUE, ERROR)
         ELSE IF (BUFFER_TYPE .EQ. MPITEST_REAL) THEN
            CALL MBEI_HANDLE_REAL(I, BUFFER, VALUE, ERROR)
         ELSE IF (BUFFER_TYPE .EQ. MPITEST_COMPLEX) THEN
            CALL MBEI_HANDLE_COMPLEX(I, BUFFER, VALUE, ERROR)
         ELSE IF (BUFFER_TYPE .EQ. MPITEST_DOUBLE_PRECISION) THEN
            CALL MBEI_HANDLE_DOUBLE(I, BUFFER, VALUE, ERROR)
         ELSE IF (BUFFER_TYPE .EQ. MPITEST_LOGICAL) THEN
            CALL MBEI_HANDLE_LOGICAL(I, BUFFER, VALUE, ERROR)
         ELSE IF (BUFFER_TYPE .EQ. MPITEST_CHARACTER) THEN
            CALL MBEI_HANDLE_CHARACTER(I, BUFFER, VALUE, ERROR)

#ifdef MPITEST_FINTEGER1_DEF
         ELSE IF (BUFFER_TYPE .EQ. MPITEST_INTEGER1) THEN
            CALL MBEI_HANDLE_INTEGER1(I, BUFFER, VALUE, ERROR)
#endif

#ifdef MPITEST_FINTEGER2_DEF
         ELSE IF (BUFFER_TYPE .EQ. MPITEST_INTEGER2) THEN
            CALL MBEI_HANDLE_INTEGER2(I, BUFFER, VALUE, ERROR)
#endif

#ifdef MPITEST_FINTEGER4_DEF
         ELSE IF (BUFFER_TYPE .EQ. MPITEST_INTEGER4) THEN
            CALL MBEI_HANDLE_INTEGER4(I, BUFFER, VALUE, ERROR)
#endif

#ifdef MPITEST_FREAL2_DEF
         ELSE IF (BUFFER_TYPE .EQ. MPITEST_REAL2) THEN
            CALL MBEI_HANDLE_REAL2(I, BUFFER, VALUE, ERROR)
#endif

#ifdef MPITEST_FREAL4_DEF
         ELSE IF (BUFFER_TYPE .EQ. MPITEST_REAL4) THEN
            CALL MBEI_HANDLE_REAL4(I, BUFFER, VALUE, ERROR)
#endif

#ifdef MPITEST_FREAL8_DEF
         ELSE IF (BUFFER_TYPE .EQ. MPITEST_REAL8) THEN
            CALL MBEI_HANDLE_REAL8(I, BUFFER, VALUE, ERROR)
#endif

#ifdef MPITEST_DOUBLE_COMPLEX_DEF
         ELSE IF (BUFFER_TYPE .EQ. MPITEST_DOUBLE_COMPLEX) THEN
            CALL MBEI_HANDLE_DCOMPLEX(I, BUFFER, VALUE, ERROR)
#endif

         END IF

100   CONTINUE

      END

C
C     Comment out the MACRO definition of MPITEST_FCMDLINE
C     if iargc() or getarg() is not supported.
C

#ifdef MPITEST_FCMDLINE
      SUBROUTINE MPITEST_HELP_MESSAGE()
C     ******************************************************************
C     Customize as necessary.
C     ******************************************************************
      INCLUDE 'mpitestf.h'
      INCLUDE 'mpif.h'

      INTEGER IERR

      IF (MPITEST_ME .EQ. 0) THEN
         IF (MPITEST_STDOUTFILE .EQ. 1) THEN
            WRITE (MPITEST_STDOUT, *)
     &         'Usage: testname [-help] [-verbose n]',
     &         ' [-stdoutdir directory]'
            WRITE (MPITEST_STDOUT, *)
     &         '-help       Generate this message'
            WRITE (MPITEST_STDOUT, *)
     &         '-stdoutdir  Send message output to file'
            WRITE (MPITEST_STDOUT, *)
     &         '-verbose    Set verbose flag to 0, 1, or 2'
         ELSE
            WRITE (*, *)
     &         'Usage: testname [-help] [-verbose n]',
     &         '[-stdoutdir directory]'
            WRITE (*, *)
     &         '-help       Generate this message'
            WRITE (*, *)
     &         '-stdoutdir  Send message output to file'
            WRITE (*, *)
     &         '-verbose    Set verbose flag to 0, 1, or 2'
         END IF
      END IF

      CALL MPI_FINALIZE(IERR)

      STOP

      END
#endif

      SUBROUTINE MPITEST_INT_TO_STR(I, STR, LEN)
C     ******************************************************************
C     Convert an integer to a string
C
C     The algoithm used may not be the most efficient but should be
C     portable.  Standard Fortran 77 does not support recursive
C     subroutine.
C
C     INPUT:  Input integer: I
C     OUTPUT: Output String: STR
C             Output string length: LEN
C     ******************************************************************
C
      INCLUDE 'mpitestf.h'

      INTEGER I, LEN
      CHARACTER*(*) STR

      INTEGER J
      CHARACTER*(IOSIZE) TEMP_STR

      INTEGER DIGIT, REMAIN

      J = 1

      STR = ''

      REMAIN = I

100   DIGIT = MOD(REMAIN, 10)
      REMAIN = INT(REMAIN / 10)

      IF (DIGIT .EQ. 0) THEN
         TEMP_STR(J:J) = '0'
      ELSE IF (DIGIT .EQ. 1) THEN
         TEMP_STR(J:J) = '1'
      ELSE IF (DIGIT .EQ. 2) THEN
         TEMP_STR(J:J) = '2'
      ELSE IF (DIGIT .EQ. 3) THEN
         TEMP_STR(J:J) = '3'
      ELSE IF (DIGIT .EQ. 4) THEN
         TEMP_STR(J:J) = '4'
      ELSE IF (DIGIT .EQ. 5) THEN
         TEMP_STR(J:J) = '5'
      ELSE IF (DIGIT .EQ. 6) THEN
         TEMP_STR(J:J) = '6'
      ELSE IF (DIGIT .EQ. 7) THEN
         TEMP_STR(J:J) = '7'
      ELSE IF (DIGIT .EQ. 8) THEN
         TEMP_STR(J:J) = '8'
      ELSE IF (DIGIT .EQ. 9) THEN
         TEMP_STR(J:J) = '9'
      END IF

      IF (REMAIN .NE. 0) THEN
         J = J + 1
         GO TO 100
      END IF

C     Reversing the string

      LEN = 1

200   STR(LEN:LEN) = TEMP_STR(J:J)

      J = J - 1
      IF (J .GE. 1) THEN
         LEN = LEN + 1
         GO TO 200
      END IF

      END


      SUBROUTINE MPITEST_GET_STR_START_POS(ARG, POS)
      INCLUDE 'mpitestf.h'

      CHARACTER*(IOSIZE) ARG
      INTEGER            POS, I

      DO 100 I = 1, IOSIZE, 1
         IF (((ICHAR(ARG(I:I)) .GE. ICHAR('A')) .AND.
     $        (ICHAR(ARG(I:I)) .LE. ICHAR('Z'))) .OR.
     $       ((ICHAR(ARG(I:I)) .GE. ICHAR('a')) .AND.
     $        (ICHAR(ARG(I:I)) .LE. ICHAR('z'))) .OR.
     $       ((ICHAR(ARG(I:I)) .GE. ICHAR('0')) .AND.
     $        (ICHAR(ARG(I:I)) .LE. ICHAR('9'))) .OR.
     $       (ARG(I:I) .EQ. '-') .OR.
     $       (ARG(I:I) .EQ. '_')) THEN
            GO TO 101
         END IF
100   CONTINUE

101   POS = I
      
      END


#ifdef MPITEST_FCMDLINE
      SUBROUTINE MPITEST_GET_PARAMETER()
C     ******************************************************************
C     Read the command line parameters.  WARNING: ARGC() and IARGC()
C     must be supported, and the parameters must be available on all
C     ranks, or this won't work.  Controlled by ifdef in foptions.h.
C
C     History :
C     03/07/96        Created       ST
C     ******************************************************************
      INCLUDE 'mpitestf.h'

      INTEGER  IARGC
      EXTERNAL IARGC

      INTEGER I, J, K, LEN, STEP, ARGC
      CHARACTER*(IOSIZE) INFOBUF
      CHARACTER*(IOSIZE) ARG
      CHARACTER*(MPITEST_FILENAME_MAX) FILENAME, RANK_STR

50    FORMAT ('Cannot open output file: ', CHAR_FMT)

      I = 1
      STEP = 0

      ARGC = IARGC()

C     IF (MPITEST_ME .EQ. 0) THEN
C        PRINT *, MPITEST_ME,' DEBUG: ',I,'IARGC() returns ', ARGC
C     END IF

100   IF (I .GT. ARGC) THEN
         GO TO 200
      END IF

      CALL GETARG(I, ARG)

      CALL MPITEST_GET_STR_START_POS(ARG, K)

C     IF (MPITEST_ME .EQ. 0) THEN
C        PRINT *, MPITEST_ME,' DEBUG:',I,' GETARG() returns ', '"', ARG,
C    $        '"'
C     END IF

      IF (ARG(K:K+5) .EQ. '-help') THEN
         CALL MPITEST_HELP_MESSAGE()
         STEP = 1
      ELSE IF (ARG(K:K+8) .EQ. '-verbose') THEN
         IF (I + 1 .GT. ARGC) THEN
            INFOBUF='argument required for -verbose'
            CALL MPITEST_MESSAGE(MPITEST_FATAL, INFOBUF)
         END IF

         CALL GETARG(I+1, ARG)

         CALL MPITEST_GET_STR_START_POS(ARG, K)

C        IF (MPITEST_ME .EQ. 0) THEN
C        PRINT *, MPITEST_ME,' DEBUG:',I,' GETARG() returns ', '"', ARG,
C    $        '"'
C        END IF

         IF (ARG(K:K) .EQ. '0') THEN
            MPITEST_VERBOSE = 0
         ELSE IF (ARG(K:K) .EQ. '1') THEN
            MPITEST_VERBOSE = 1
         ELSE IF (ARG(K:K) .EQ. '2') THEN
            MPITEST_VERBOSE = 2
         ELSE
            INFOBUF='Illegal verbosity argument on command line'
            CALL MPITEST_MESSAGE(MPITEST_FATAL, INFOBUF)
         END IF

         STEP = 2

      ELSE IF (ARG(K:K+10) .EQ. '-stdoutdir') THEN
         IF (I + 1 .GT. ARGC) THEN
            INFOBUF='directory argument required for -verbose'
            CALL MPITEST_MESSAGE(MPITEST_FATAL, INFOBUF)
         END IF

         CALL GETARG(I+1, ARG)

C        IF (MPITEST_ME .EQ. 0) THEN
C        PRINT *, MPITEST_ME,' DEBUG:',I,' GETARG() returns ', '"', ARG,
C    $        '"'
C        END IF

         CALL MPITEST_GET_STR_START_POS(ARG, K)
C
C        Locate last non-space character
C
         J = IOSIZE
101      IF (ARG(J:J) .NE. ' ') THEN
            GO TO 150
         ELSE
            J = J - 1
            IF (J .LT. 1) THEN
               J = 1
               GO TO 150
            END IF
         END IF

         GO TO 101

C
C        Contructs filename to be used for output
C
150      FILENAME = ARG(1:J)

C
C        Convert integer rank to string representation and
C        use it as part of the filename
C
         CALL MPITEST_INT_TO_STR(MPITEST_ME, RANK_STR, LEN)

         FILENAME(J+1:J+8+LEN) = '/stdout.' // RANK_STR

C
C        Open file
C
         OPEN(MPITEST_STDOUT, ERR=999, FILE=FILENAME, STATUS='NEW')

         MPITEST_STDOUTFILE = 1
         STEP = 2
      ELSE
C
C     ignore unknown option
C
         print *, 'Unknown option ', ARG
         STEP = 1
      END IF

      I = I + STEP
      GO TO 100

200   RETURN

999   INFOBUF='Unable to open file'
      CALL MPITEST_MESSAGE(MPITEST_NONFATAL, INFOBUF)
      CALL MPITEST_MESSAGE(MPITEST_FATAL, FILENAME)

      END

#endif


      SUBROUTINE MPITEST_INIT(IERR)
C     ******************************************************************
C     Initialize the validation suite environment.  Set the global
C     rank and the number of processors in the application.  Set the
C     current rank to the global rank.
C
C     History :
C     12/07/95        Created       Greg Morrow
C     ******************************************************************
      INCLUDE 'mpitestf.h'
      INCLUDE 'mpif.h'

      INTEGER IERR

      INTEGER ERR, ERRSIZE
      CHARACTER*(IOSIZE) INFOBUF
      CHARACTER*(60) MYVERS

 99   FORMAT(CHAR_FMT, INT_FMT, CHAR_FMT)

      INFOBUF='@(#)MPI V1.1 Validation Suite V1.0-01' 
      INFOBUF = ' '

      CALL MPI_COMM_SIZE(MPI_COMM_WORLD, MPITEST_NUMP, IERR)
      IF (IERR .NE. MPI_SUCCESS) THEN 
        WRITE(INFOBUF,99) 'MPI_COMM_SIZE() returned',
     $     IERR, ' in MPITEST_INIT()'
        CALL MPITEST_MESSAGE(MPITEST_NONFATAL, INFOBUF) 
        INFOBUF=' ' 
        CALL MPI_ERROR_STRING(IERR, INFOBUF, ERRSIZE, ERR)
        CALL MPITEST_MESSAGE(MPITEST_FATAL, INFOBUF) 
      END IF

      CALL MPI_COMM_RANK(MPI_COMM_WORLD, MPITEST_ME, IERR)
      IF (IERR .NE. MPI_SUCCESS) THEN 
        WRITE(INFOBUF,99) 'MPI_COMM_RANK() returned',
     $     IERR, ' in MPITEST_INIT()'
        CALL MPITEST_MESSAGE(MPITEST_NONFATAL, INFOBUF) 
        INFOBUF=' ' 
        CALL MPI_ERROR_STRING(IERR, INFOBUF, ERRSIZE, ERR)
        CALL MPITEST_MESSAGE(MPITEST_FATAL, INFOBUF) 
      END IF

      CALL MPITEST_INIT_DATATYPES()

      MPITEST_CURRENT_RANK = MPITEST_ME

      CALL MPI_ERRHANDLER_SET(MPI_COMM_WORLD,
     $                        MPI_ERRORS_RETURN, IERR)
      IF (IERR .NE. MPI_SUCCESS) THEN 
        WRITE(INFOBUF,99) 'MPI_ERRHANDLER_SET() returned',
     $     IERR, ' in MPITEST_INIT()'
        CALL MPITEST_MESSAGE(MPITEST_NONFATAL, INFOBUF) 
        INFOBUF=' ' 
        CALL MPI_ERROR_STRING(IERR, INFOBUF, ERRSIZE, ERR)
        CALL MPITEST_MESSAGE(MPITEST_FATAL, INFOBUF) 
      END IF

#ifdef MPITEST_FCMDLINE
      MPITEST_VERBOSE = 0
      MPITEST_STDOUTFILE = 0
      CALL MPITEST_GET_PARAMETER()
#else
      MPITEST_VERBOSE = 0
C     If iargc() and getarg() is not provided.  There is no easy
C     and portable way to pass an absolute file names of files to
C     which each node can direct it's output to...
      MPITEST_STDOUTFILE = 0
#endif
C      MPITEST_VERBOSE=1

      END
C     end of 'subroutine MPITEST_INIT()'



      SUBROUTINE MPITEST_REPORT(PASS, FAIL, VERIFY, TESTNAME)
C     ******************************************************************
C     Report the final status of a test.
C
C     Close the output files, if required.
C
C     Arguments :
C     fail, pass, verify  Number of each reported by the calling process.
C     testname            Name of the test.               
C
C     History :
C      2/07/96    GT    Created
C     ******************************************************************
      INCLUDE 'mpitestf.h'
      INCLUDE 'mpif.h'

      INTEGER PASS, FAIL, VERIFY
      CHARACTER*32 TESTNAME

      INTEGER PASSTOTAL, FAILTOTAL, VERIFYTOTAL
      INTEGER TOTAL
      INTEGER IERR, ERR, ERRSIZE
      CHARACTER*(IOSIZE) INFOBUF
      INTEGER I

 50   FORMAT(INT_FMT, ': cannot close file')
 99   FORMAT(CHAR_FMT, INT_FMT, CHAR_FMT)
 199  FORMAT('Node results: pass=', INT_FMT,', fail=',
     $       INT_FMT,', verify=', INT_FMT)
 200  FORMAT('MPITEST_results: ', CHAR_FMT ,'all tests PASSED (',
     $       INT_FMT, ')')
 201  FORMAT('MPITEST_results: ', CHAR_FMT, ' ', INT_FMT,
     $       ' tests PASSED, must manually verify ',
     $       INT_FMT, ' (of ', INT_FMT, ')')
 202  FORMAT('MPITEST_results: ', CHAR_FMT, ' ', INT_FMT,
     $  ' tests FAILED  (of ', INT_FMT, ')')
 203  FORMAT('MPITEST_results: ', CHAR_FMT, ' ', INT_FMT,
     $       ' tests FAILED, must manually verify ', INT_FMT,
     $       ' (of ', INT_FMT, ')')
 204  FORMAT(A)

      WRITE(INFOBUF, 199) PASS, FAIL, VERIFY
      CALL MPITEST_MESSAGE(MPITEST_INFO1, INFOBUF)

      CALL MPI_ALLREDUCE(PASS, PASSTOTAL, 1, MPI_INTEGER, MPI_SUM,
     $  MPI_COMM_WORLD, IERR)
      IF (IERR .NE. MPI_SUCCESS) THEN
        WRITE(INFOBUF,99) 'MPI_ALLREDUCE() returned',
     $     IERR, ' in MPITEST_RESULTS()'
        CALL MPITEST_MESSAGE(MPITEST_NONFATAL, INFOBUF)
        INFOBUF=' '
        CALL MPI_ERROR_STRING(IERR, INFOBUF, ERRSIZE, ERR)
        CALL MPITEST_MESSAGE(MPITEST_FATAL, INFOBUF)
      END IF

      CALL MPI_ALLREDUCE(FAIL, FAILTOTAL, 1, MPI_INTEGER, MPI_SUM,
     $  MPI_COMM_WORLD, IERR)
      IF (IERR .NE. MPI_SUCCESS) THEN
        WRITE(INFOBUF,99) 'MPI_ALLREDUCE() returned',
     $     IERR, ' in MPITEST_RESULTS()'
        CALL MPITEST_MESSAGE(MPITEST_NONFATAL, INFOBUF)
        INFOBUF=' '
        CALL MPI_ERROR_STRING(IERR, INFOBUF, ERRSIZE, ERR)
        CALL MPITEST_MESSAGE(MPITEST_FATAL, INFOBUF)
      END IF

      CALL MPI_ALLREDUCE(VERIFY, VERIFYTOTAL, 1, MPI_INTEGER, MPI_SUM,
     $  MPI_COMM_WORLD, IERR)
      IF (IERR .NE. MPI_SUCCESS) THEN
        WRITE(INFOBUF,99) 'MPI_ALLREDUCE() returned',
     $     IERR, ' in MPITEST_RESULTS()'
        CALL MPITEST_MESSAGE(MPITEST_NONFATAL, INFOBUF)
        INFOBUF=' '
        CALL MPI_ERROR_STRING(IERR, INFOBUF, ERRSIZE, ERR)
        CALL MPITEST_MESSAGE(MPITEST_FATAL, INFOBUF)
      END IF

      TOTAL=PASSTOTAL+FAILTOTAL+VERIFYTOTAL

C     Remove trailing blanks, if any, from test name to help clean-up output.
      DO 300 I=32, 7, -1
        IF (TESTNAME(I:I) .NE. ' ') GO TO 301
 300  CONTINUE
 301  CONTINUE

      IF (MPITEST_ME .EQ. 0) THEN
        IF (FAILTOTAL .EQ. 0) THEN
          IF (VERIFYTOTAL .EQ. 0) THEN
            WRITE(INFOBUF, 200) TESTNAME(1:I), TOTAL
          ELSE
            WRITE(INFOBUF, 201) TESTNAME(1:I), PASSTOTAL, VERIFYTOTAL,
     $         TOTAL
          END IF
        ELSE
          IF (VERIFYTOTAL .EQ. 0) THEN
            WRITE(INFOBUF, 202) TESTNAME(1:I), FAILTOTAL, TOTAL
          ELSE
            WRITE(INFOBUF, 203) TESTNAME(1:I), FAILTOTAL, VERIFYTOTAL,
     $         TOTAL
          END IF
        END IF
      
C     Remove trailing blanks, if any, from results to help clean-up output.
      DO 400 I=IOSIZE, 40, -1
        IF (INFOBUF(I:I) .NE. ' ') GO TO 401
 400  CONTINUE
 401  CONTINUE

      IF (MPITEST_STDOUTFILE .EQ. 1) THEN
         WRITE (MPITEST_STDOUT, *) INFOBUF(1:I)
         CLOSE(MPITEST_STDOUT, ERR=999)
      ELSE
         WRITE (*,204)INFOBUF(1:I)
      END IF

      END IF

      RETURN

999   WRITE (INFOBUF, 50) MPITEST_ME
      CALL MPITEST_MESSAGE(MPITEST_FATAL, INFOBUF)

      END
C     end of 'subroutine MPITEST_report()'


      SUBROUTINE MPITEST_MESSAGE(LEVEL, TEXT)
C     ******************************************************************
C     Print a message.  The message is passed as a fixed length
C     character array, whose length (IOSIZE) is set by the C pre-
C     processor.  
C
C     Arguments :
C     int level.....Specifies the circumstance under
C                   which the message should be reported.
C		  Choices :
C		    MPITEST_FATAL: always report, abort app.
C		    MPITEST_NONFATAL :always report, don't abort
C		    MPITEST_INFO0 : always report 
C		    MPITEST_INFO1 : report if MPITEST_verbose != 0
C		    MPITEST_INFO2 : report if MPITEST_verbose = 2
C		   The above macros are defined in mpitestf.h
C
C     character*(IOSIZE) text....The message to be printed. 
C
C     History :
C     12/07/95        Created       Greg Morrow
C     1/16/07         Make use of LEN to allow other lengths of buffers to
C                     be passed (such as an ERROR_STRING from MPI_ERROR_STRING)
C                     William Gropp
C     ******************************************************************
      INCLUDE 'mpitestf.h'
      INCLUDE 'mpif.h'

      INTEGER LEVEL
      CHARACTER*(*) TEXT
      INTEGER I, IERR
      INTEGER TEXTLEN

      TEXTLEN = LEN(TEXT)

 101  FORMAT(' MPITEST_FATAL(', INT_FMT,'): ', CHAR_FMT)
 102  FORMAT(' MPITEST_ERROR(', INT_FMT,'): ', CHAR_FMT)
 103  FORMAT(' MPITEST_INFO (', INT_FMT,'): ', CHAR_FMT)
 104  FORMAT(' MPITEST_INFO1(', INT_FMT,'): ', CHAR_FMT)
 105  FORMAT(' MPITEST_INFO2(', INT_FMT,'): ', CHAR_FMT)
 106  FORMAT(' MPITEST_VERFY(', INT_FMT,'): ', CHAR_FMT)

C     Remove trailing blanks, if any, from message to help clean-up output.
      DO 200 I=TEXTLEN, 40, -1
        IF (TEXT(I:I) .NE. ' ') GO TO 201
 200  CONTINUE
 201  CONTINUE

      IF (MPITEST_STDOUTFILE .EQ. 1) THEN
         IF (LEVEL .EQ. MPITEST_FATAL) THEN
            WRITE (MPITEST_stdout, 101) MPITEST_ME,TEXT(1:I)
            CALL MPI_ABORT(MPI_COMM_WORLD, -1, IERR)
            IF (IERR .NE. MPI_SUCCESS) THEN
C              Can't abort, bailing out!
C
               STOP
            END IF
         ELSE IF (LEVEL .EQ. MPITEST_NONFATAL) THEN
            WRITE (MPITEST_stdout, 102) MPITEST_ME,TEXT(1:I)
         ELSE IF (LEVEL .EQ. MPITEST_INFO0) THEN
            WRITE (MPITEST_stdout, 103) MPITEST_ME,TEXT(1:I)
         ELSE IF ((LEVEL .EQ. MPITEST_INFO1) .AND.
     $           (MPITEST_VERBOSE .NE. 0)) THEN
            WRITE (MPITEST_stdout, 104) MPITEST_ME,TEXT(1:I)
         ELSE IF (LEVEL .EQ. MPITEST_INFO2 .AND.
     $            MPITEST_VERBOSE .EQ. 2) THEN
            WRITE (MPITEST_stdout, 105) MPITEST_ME,TEXT(1:I)
         ELSE IF (LEVEL .EQ. MPITEST_VERIFY) THEN
            WRITE (MPITEST_stdout, 106) MPITEST_ME,TEXT(1:I)
         END IF
      ELSE
         IF (LEVEL .EQ. MPITEST_FATAL) THEN
            WRITE (*, 101) MPITEST_ME,TEXT(1:I)
            CALL MPI_ABORT(MPI_COMM_WORLD, -1, IERR)
            IF (IERR .NE. MPI_SUCCESS) THEN
C              Can't abort, bailing out!
C
               STOP
            END IF
         ELSE IF (LEVEL .EQ. MPITEST_NONFATAL) THEN
            WRITE (*, 102) MPITEST_ME,TEXT(1:I)
         ELSE IF (LEVEL .EQ. MPITEST_INFO0) THEN
            WRITE (*, 103) MPITEST_ME,TEXT(1:I)
         ELSE IF ((LEVEL .EQ. MPITEST_INFO1) .AND.
     $           (MPITEST_VERBOSE .NE. 0)) THEN
            WRITE (*, 104) MPITEST_ME,TEXT(1:I)
         ELSE IF (LEVEL .EQ. MPITEST_INFO2 .AND.
     $            MPITEST_VERBOSE .EQ. 2) THEN
            WRITE (*, 105) MPITEST_ME,TEXT(1:I)
         ELSE IF (LEVEL .EQ. MPITEST_VERIFY) THEN
            WRITE (*, 106) MPITEST_ME,TEXT(1:I)
         END IF
      END IF

      END
C     end of 'subroutine MPITEST_message()'

      SUBROUTINE MPITEST_GET_COMMUNICATOR(CONTEXT, INDEX, COMM, SIZE)
C     ******************************************************************
C     Get a new communicator, comm, of type context, whose size token 
C     begins at MPITEST_comm_sizes(index).
C
C     Arguments
C     context    INPUT, integer specifying communicator type
C                 MPITEST_comm_world : MPI_COMM_WORLD
C                 MPITEST_comm_self  : MPI_COMM_SELF
C                 MPITEST_comm_dup   : duped comm
C                 MPITEST_comm_create: new comm
C                 MPITEST_comm_split : split comm
C                 MPITEST_comm_inter : intercommunicator
C                 MPITEST_comm_merge : merged intercommunicator
C     
C     index      INPUT, integer giving index in MPITEST_comm_sizes()
C                 of the beginning of the size token of this comm
C     
C     comm       OUTPUT, handle to new MPI_Comm.  If the calling rank
C                 is not a member of the created comm, then the return
C		  value is comm = MPI_COMM_NULL.
C
C     size      OUTPUT, number of ranks in the new communicator
C
C
C     History :
C     12/07/95        Created       Greg Morrow
C     ******************************************************************
      INCLUDE 'mpitestf.h'
      INCLUDE 'mpif.h'

      INTEGER CONTEXT, INDEX, COMM, SIZE

      INTEGER COMM1, IERR, ERR, ERRSIZE
      CHARACTER*(IOSIZE) INFOBUF
 99   FORMAT(CHAR_FMT, INT_FMT, CHAR_FMT)

      IF (CONTEXT .EQ. MPITEST_COMM_WORLD) THEN
        COMM = MPI_COMM_WORLD
        SIZE = MPITEST_nump
        MPITEST_CURRENT_RANK = MPITEST_ME
        MPITEST_INTER = MPITEST_NOT_INTER

      ELSE IF (CONTEXT .EQ. MPITEST_COMM_SELF) THEN
        IF (MPITEST_ME .EQ. 0) THEN
          COMM = MPI_COMM_SELF
          MPITEST_CURRENT_RANK = 0
        ELSE
          COMM = MPI_COMM_NULL
          MPITEST_CURRENT_RANK = MPI_UNDEFINED
        END IF
        SIZE = 1
        MPITEST_INTER = MPITEST_NOT_INTER

      ELSE IF (CONTEXT .EQ. MPITEST_COMM_DUP) THEN
        CALL MPITEST_GET_DUPED_COMMUNICATOR(INDEX, COMM, SIZE)
        MPITEST_INTER = MPITEST_NOT_INTER

      ELSE IF (CONTEXT .EQ. MPITEST_COMM_CREATE) THEN
        CALL MPITEST_CREATE_COMMUNICATOR(INDEX, COMM, SIZE)
        MPITEST_INTER = MPITEST_NOT_INTER

      ELSE IF (CONTEXT .EQ. MPITEST_COMM_SPLIT) THEN
        CALL MPITEST_SPLIT_COMMUNICATOR(INDEX, COMM, SIZE)
        MPITEST_INTER = MPITEST_NOT_INTER

      ELSE IF (CONTEXT .EQ. MPITEST_COMM_INTER) THEN
        CALL MPITEST_GET_INTERCOMM(INDEX, COMM, SIZE)

      ELSE IF (CONTEXT .EQ. MPITEST_COMM_MERGE) THEN
C         print *, 'index before intercomm = ', index
        CALL MPITEST_GET_INTERCOMM(INDEX, COMM1, SIZE)

        IF (COMM1 .NE.	MPI_COMM_NULL) THEN
          CALL MPI_INTERCOMM_MERGE(COMM1, .FALSE., COMM, IERR)
          IF (IERR .NE. MPI_SUCCESS) THEN
            WRITE(INFOBUF,99) 'MPI_INTERCOMM_MERGE() returned',
     $         IERR, ' in MPITEST_GET_COMMUNICATOR()'
            CALL MPITEST_MESSAGE(MPITEST_NONFATAL, INFOBUF)
            INFOBUF=' '
            CALL MPI_ERROR_STRING(IERR, INFOBUF, ERRSIZE, ERR)
            CALL MPITEST_MESSAGE(MPITEST_FATAL, INFOBUF)
          END IF

          CALL MPI_COMM_FREE(COMM1, IERR)
          IF (IERR .NE. MPI_SUCCESS) THEN
            WRITE(INFOBUF,99) 'MPI_COMM_FREE() returned',
     $         IERR, ' in MPITEST_GET_COMMUNICATOR()'
            CALL MPITEST_MESSAGE(MPITEST_NONFATAL, INFOBUF)
            INFOBUF=' '
            CALL MPI_ERROR_STRING(IERR, INFOBUF, ERRSIZE, ERR)
            CALL MPITEST_MESSAGE(MPITEST_FATAL, INFOBUF)
          END IF

          CALL MPI_COMM_RANK(COMM, MPITEST_CURRENT_RANK, IERR)
          IF (IERR .NE. MPI_SUCCESS) THEN 
            WRITE(INFOBUF,99) 'MPI_COMM_RANK() returned',
     $         IERR, ' in MPITEST_GET_COMMUNICATOR()'
            CALL MPITEST_MESSAGE(MPITEST_NONFATAL, INFOBUF) 
            INFOBUF=' ' 
            CALL MPI_ERROR_STRING(IERR, INFOBUF, ERRSIZE, ERR)
            CALL MPITEST_MESSAGE(MPITEST_FATAL, INFOBUF) 
          END IF

          CALL MPI_COMM_SIZE(COMM, SIZE, IERR)
          IF (IERR .NE. MPI_SUCCESS) THEN 
            WRITE(INFOBUF,99) 'MPI_COMM_SIZE() returned',
     $         IERR, ' in MPITEST_GET_COMMUNICATOR()'
            CALL MPITEST_MESSAGE(MPITEST_NONFATAL, INFOBUF) 
            INFOBUF=' ' 
            CALL MPI_ERROR_STRING(IERR, INFOBUF, ERRSIZE, ERR)
            CALL MPITEST_MESSAGE(MPITEST_FATAL, INFOBUF) 
          END IF
        ELSE
          COMM = MPI_COMM_NULL
        END IF
        MPITEST_INTER = MPITEST_NOT_INTER

      END IF

      END
C     end of 'subroutine MPITEST_get_communicator()'


      SUBROUTINE MPITEST_SPLIT_COMMUNICATOR(INDEX, COMM, SIZE)
C     ******************************************************************
C     Get a new split communicator.
C
C     Arguments
C     index  INPUT, integer index of the size token of the
C             desired communicator in the MPITEST_comm_sizes() array.
C     
C     comm   OUTPUT, integer handle for the new communicator,
C             or MPI_COMM_NULL if the calling rank is not a member 
C             of the group of the new communicator.
C
C     size   OUTPUT, integer number of ranks in the new communicator
C
C     Algorithm
C     When using MPI_COMM_SPLIT(), the integer key contains information
C     about the rank in the new communicator, and the integer color
C     provides information about membership in the new communicator.
C     In this routine, key starts out 0, and color starts out MPI_UNDEFINED.
C     If color is not changed before the call to MPI_COMM_SPLIT(), 
C     then the calling rank will not be included in the new comm.
C     
C     First get the size of the communicator with a library call.
C     Next step through the elements of MPITEST_comm_sizes(), starting
C     at 'index', and for each element, determine if that element
C     implies the calling rank is in the new comm.  If so, reset
C     color and key.
C
C     Then just call MPI_COMM_SPLIT() and return size.
C     
C     History :
C     12/07/95        Created       Greg Morrow
C     ******************************************************************
      INCLUDE 'mpitestf.h'
      INCLUDE 'mpif.h'

      INTEGER INDEX, COMM, SIZE

      INTEGER IERR, ERR, ERRSIZE, I
      INTEGER SIZTOK, COLOR, KEY, START, INC
      CHARACTER*(IOSIZE) INFOBUF

 99   FORMAT(CHAR_FMT, INT_FMT, CHAR_FMT)

      KEY = 0
      COLOR = MPI_UNDEFINED
      MPITEST_CURRENT_RANK = MPI_UNDEFINED

      CALL MPITEST_GET_COMM_SIZE(MPITEST_COMM_SPLIT, INDEX, SIZE)
      
      DO 100 i = 1, SIZE
        SIZTOK = MPITEST_COMMS(INDEX)

        IF (SIZTOK .EQ. MPITEST_COMM_COMINC) THEN
          START = MPITEST_COMMS(INDEX + 1)
          INC = MPITEST_COMMS(INDEX + 3)
          IF (MPITEST_ME .EQ. (START+(I-1)*INC)) THEN
            COLOR = 1
            KEY = I
            MPITEST_CURRENT_RANK = I-1
          END IF

        ELSE IF (SIZTOK .EQ. MPITEST_COMM_RNKLST) THEN
          IF (MPITEST_ME .EQ. MPITEST_COMMS(INDEX+1+I)) THEN
            COLOR = 1
            KEY = I
            MPITEST_CURRENT_RANK = I-1
          END IF
        ELSE IF (MPITEST_ME .EQ. (I-1)) THEN
          COLOR = 1
          KEY = I
          MPITEST_CURRENT_RANK = I-1
        END IF
 100  CONTINUE

      IF (MPITEST_CURRENT_RANK .NE. MPI_UNDEFINED) THEN
        COLOR = 0
      ELSE
        COLOR = 1
        SIZE = MPITEST_NUMP - SIZE
      END IF

      CALL MPI_COMM_SPLIT(MPI_COMM_WORLD, COLOR, KEY, COMM, IERR)
      IF (IERR .NE. MPI_SUCCESS) THEN 
        WRITE(INFOBUF,99) 'MPI_COMM_SPLIT() returned',
     $     IERR, ' in MPITEST_SPLIT_COMMUNICATOR()'
        CALL MPITEST_MESSAGE(MPITEST_NONFATAL, INFOBUF) 
        INFOBUF=' ' 
        CALL MPI_ERROR_STRING(IERR, INFOBUF, ERRSIZE, ERR)
        CALL MPITEST_MESSAGE(MPITEST_FATAL, INFOBUF) 
      END IF

      CALL MPI_COMM_RANK(COMM, MPITEST_CURRENT_RANK, IERR)
      IF (IERR .NE. MPI_SUCCESS) THEN 
        WRITE(INFOBUF,99) 'MPI_COMM_RANK() returned',
     $     IERR, ' in MPITEST_SPLIT_COMMUNICATOR()'
        CALL MPITEST_MESSAGE(MPITEST_NONFATAL, INFOBUF) 
        INFOBUF=' ' 
        CALL MPI_ERROR_STRING(IERR, INFOBUF, ERRSIZE, ERR)
        CALL MPITEST_MESSAGE(MPITEST_FATAL, INFOBUF) 
      END IF

C      print*,'Split() exit : ', MPITEST_me

      END
C     end of 'subroutine MPITEST_split_communicator()'


      SUBROUTINE MPITEST_GET_DUPED_COMMUNICATOR(INDEX, COMM, SIZE)
C     ******************************************************************
C     Create a duplicated communicator.
C
C     Arguments :
C     index  INPUT integer index of the size token of the
C             desired communicator in the MPITEST_comm_sizes() array.
C     
C     comm   OUTPUT integer handle for the new communicator,
C             or MPI_COMM_NULL if the calling rank is not a member 
C             of the group of the new communicator.
C
C     size   OUTPUT, integer number of ranks in the new communicator
C
C     Algorithm : Get a new comm with a call to create_communicator(),
C     then call MPI_COMM_DUP() to duplicate it.
C     
C
C     History :
C     12/07/95        Created       Greg Morrow
C     ******************************************************************
      INCLUDE 'mpitestf.h'
      INCLUDE 'mpif.h'

      INTEGER INDEX, COMM, SIZE

      INTEGER TEMPCOMM, IERR, ERR, ERRSIZE
      CHARACTER*(IOSIZE) INFOBUF

 99   FORMAT(CHAR_FMT, INT_FMT, CHAR_FMT)

C     Create a communicator, setting MPITEST_current_rank in the process.
      CALL MPITEST_CREATE_COMMUNICATOR(INDEX, TEMPCOMM, SIZE)

      IF (MPITEST_CURRENT_RANK .NE. MPI_UNDEFINED) THEN
        CALL MPI_COMM_DUP(TEMPCOMM, COMM, IERR)
        IF (IERR .NE. MPI_SUCCESS) THEN 
          WRITE(INFOBUF,99) 'MPI_COMM_DUP() returned',
     $       IERR, ' in MPITEST_GET_DUPED_COMMUNICATOR()'
          CALL MPITEST_MESSAGE(MPITEST_NONFATAL, INFOBUF) 
          INFOBUF=' ' 
          CALL MPI_ERROR_STRING(IERR, INFOBUF, ERRSIZE, ERR)
          CALL MPITEST_MESSAGE(MPITEST_FATAL, INFOBUF) 
        END IF
        CALL MPI_COMM_FREE(TEMPCOMM, IERR)
        IF (IERR .NE. MPI_SUCCESS) THEN 
          WRITE(INFOBUF,99) 'MPI_COMM_FREE() returned',
     $       IERR, ' in MPITEST_GET_DUPED_COMMUNICATOR()'
          CALL MPITEST_MESSAGE(MPITEST_NONFATAL, INFOBUF) 
          INFOBUF=' ' 
          CALL MPI_ERROR_STRING(IERR, INFOBUF, ERRSIZE, ERR)
          CALL MPITEST_MESSAGE(MPITEST_FATAL, INFOBUF) 
        END IF
      ELSE
        COMM = MPI_COMM_NULL
      END IF

      END
C     end of 'subroutine MPITEST_get_duped_communicator()'



      SUBROUTINE MPITEST_GET_INTERCOMM(INDEX, COMM, SIZE)
C     ******************************************************************
C     Get an intercommunicator.
C
C     This routine creates an intercommunicator as specified in the
C     configuration array, MPITEST_comms().  If this routine is called,
C     that means that MPITEST_comms(index) = MPITEST_COMM_INTER. 
C     The configuration array syntax is such that following the
C     MPITEST_COMM_INTER token will be tokens
C     corresponding to two intracommunicators.  These two intracomm's
C     are then the basis for building the intercommunicator.
C
C     This routine takes the following steps :
C     1. Get a duplicate of MPI_COMM_WORLD to use as the peer comm.
C     2. Determine the type and index of the two intracommunicators
C        to be used in creating the intercommunicator.
C     3. Get the two intracommunicators.
C     4. Obtain the remote leader information.  Each of the local
C        leaders (in this routine rank 0 always acts as local leader) 
C        must know the rank in peercomm of the other local leader.
C        This information is collected to all ranks via a call
C        to MPI_Allgather().
C     5. Create the intercommunicator.
C     6. Free the intracomms and the peer comm.
C
C     History :
C     12/07/95        Created       Greg Morrow
C     ******************************************************************
      INCLUDE 'mpitestf.h'
      INCLUDE 'mpif.h'

      INTEGER INDEX, COMM, SIZE

      INTEGER COMM1, COMM2, PEERCOMM
      INTEGER GROUP1, GROUP2, GROUPWORLD
      INTEGER INDEX1, INDEX2, RANK1, RANK2
      INTEGER REMOTE1, REMOTE2
      INTEGER ICOMTAG, SIZE2, IERR
      INTEGER TYPE1, TYPE2, TEMP, I
      INTEGER ERR, ERRSIZE
      INTEGER RANKS(MAX_RANKS)

      CHARACTER*(IOSIZE) INFOBUF

 99   FORMAT(CHAR_FMT, INT_FMT, CHAR_FMT)
      
      RANK1 = -1
      RANK2 = -1
      REMOTE1 = -1
      REMOTE2 = -1
      COMM1 = MPI_COMM_NULL
      COMM2 = MPI_COMM_NULL
      ICOMTAG = 12365


C     Get a copy of MPI_COMM_WORLD to use as peer comm 
      CALL MPI_COMM_DUP(MPI_COMM_WORLD, PEERCOMM, IERR)
      IF (IERR .NE. MPI_SUCCESS) THEN 
        WRITE(INFOBUF,99) 'MPI_COMM_DUP() returned',
     $     IERR, ' in MPITEST_GET_INTERCOMM()'
        CALL MPITEST_MESSAGE(MPITEST_NONFATAL, INFOBUF) 
        INFOBUF=' ' 
        CALL MPI_ERROR_STRING(IERR, INFOBUF, ERRSIZE, ERR)
        CALL MPITEST_MESSAGE(MPITEST_FATAL, INFOBUF) 
      END IF

C     Find out what kind of intra-comms we're using
      INDEX1 = INDEX + 1
      INDEX2 = INDEX + 1
      
C     The first type 
      TYPE1 = MPITEST_COMMS(INDEX1)

C     How many comms of this type are there? (either 1 or 2)
C     This also sets index2 to the next comm type token in
C     the MPITEST_comms() array.
      CALL COUNT_ELEMENTS_INCR_INDEX(INDEX2, TEMP)

C     The first comm's index in MPITEST_comms()
      CALL FIND_COMM_INDEX(INDEX1, 1, INDEX1)

C     If temp = 1, then MPITEST_comms(index2) is the second type.
C     If temp = 2, then both comms are of type1
      IF (TEMP .EQ. 1) THEN
         TYPE2 = MPITEST_COMMS(INDEX2)
         CALL FIND_COMM_INDEX(INDEX2, 1, INDEX2)
C- check this well!
      ELSE
         TYPE2 = TYPE1
         CALL FIND_COMM_INDEX(INDEX+1, 2, INDEX2)
      END IF

C     Get comm1 and {rank in comm1}=rank1


C     Note that FORTRAN does not handle recursion well,
C     so we rewrite subroutine communicator() here.  In
C     other words, the following conditional is equivalent 
C     to a call to communicator(), but that would be
C     a recursive call (since the current function was called
C     from communicator()), so we just reimplement
C     what communicator() does here.

      IF (TYPE1 .EQ. MPITEST_COMM_DUP) THEN
C         print *, 'duped 1'
       CALL MPITEST_GET_DUPED_COMMUNICATOR(INDEX1, COMM1, SIZE)
C         print *, 'size = ', size

      ELSE IF (TYPE1 .EQ. MPITEST_COMM_CREATE) THEN
C         print *, 'create 1'
       CALL MPITEST_CREATE_COMMUNICATOR(INDEX1, COMM1, SIZE)

      ELSE IF (TYPE1 .EQ. MPITEST_COMM_SPLIT) THEN
C         print *, 'split 1'
        CALL MPITEST_SPLIT_COMMUNICATOR(INDEX1, COMM1, SIZE)
      END IF

      IF (COMM1 .NE. MPI_COMM_NULL) then
         CALL MPI_COMM_RANK(COMM1, RANK1, IERR)
      ELSE
         RANK1 = -1
      END IF


C***  Get comm2 and {rank in comm2} = rank2

C***  Note that FORTRAN does not handle recursion well,
C***  so we rewrite subroutine communicator() here.  In
C***  other words, the following conditional is equivalent 
C***  to a call to communicator(), but that would be
C***  a recursive call (since the current function was called
C***  from communicator()), so we just reimplement
C***  what communicator() does here.

      IF (TYPE2 .EQ. MPITEST_COMM_DUP) THEN
C         print *, 'duped'
       CALL MPITEST_GET_DUPED_COMMUNICATOR(INDEX2, COMM2, SIZE)
C         print *, 'size = ', size

      ELSE IF (TYPE2 .EQ. MPITEST_COMM_CREATE) THEN
C         print *, 'commcreate'
       CALL MPITEST_CREATE_COMMUNICATOR(INDEX2, COMM2, SIZE)

      ELSE IF (TYPE2 .EQ. MPITEST_COMM_SPLIT) THEN
C         print *, 'split'
        CALL MPITEST_SPLIT_COMMUNICATOR(INDEX2, COMM2, SIZE)
      END IF

      IF (COMM2 .NE. MPI_COMM_NULL) then
         CALL MPI_COMM_RANK(COMM2, RANK2, IERR)
      ELSE
         RANK2 = -1
      END IF

      IF (RANK1 .EQ. 0) THEN
         REMOTE1 = MPITEST_ME
      ELSE
         REMOTE1 = -1
      END IF

      DO 10 I=1, MPITEST_NUMP
        RANKS(I) = -1
 10   CONTINUE


      CALL MPI_ALLGATHER(REMOTE1, 1, MPI_INTEGER, RANKS, 1, 
     &  MPI_INTEGER, MPI_COMM_WORLD, IERR)
      IF (IERR .NE. MPI_SUCCESS) THEN 
        WRITE(INFOBUF,99) 'MPI_COMM_DUP() returned',
     $     IERR, ' in MPITEST_GET_INTERCOMM()'
        CALL MPITEST_MESSAGE(MPITEST_NONFATAL, INFOBUF) 
        INFOBUF=' ' 
        CALL MPI_ERROR_STRING(IERR, INFOBUF, ERRSIZE, ERR)
        CALL MPITEST_MESSAGE(MPITEST_FATAL, INFOBUF) 
      END IF

      DO 50 I=1, MPITEST_NUMP
        IF (RANKS(I) .NE. -1) THEN
          REMOTE1 = RANKS(I)
          GO TO 60
        END IF
 50   CONTINUE

 60   CONTINUE

      IF (RANK2 .EQ. 0) THEN
         REMOTE2 = MPITEST_ME
      ELSE
         REMOTE2 = -1
      END IF

      DO 100 I=1, MPITEST_NUMP
        RANKS(I) = -1
 100  CONTINUE

      CALL MPI_ALLGATHER(REMOTE2, 1, MPI_INTEGER, RANKS, 1, 
     &  MPI_INTEGER, MPI_COMM_WORLD, ierr)
      IF (IERR .NE. MPI_SUCCESS) THEN 
        WRITE(INFOBUF,99) 'MPI_COMM_DUP() returned',
     $     IERR, ' in MPITEST_GET_INTERCOMM()'
        CALL MPITEST_MESSAGE(MPITEST_NONFATAL, INFOBUF) 
        INFOBUF=' ' 
        CALL MPI_ERROR_STRING(IERR, INFOBUF, ERRSIZE, ERR)
        CALL MPITEST_MESSAGE(MPITEST_FATAL, INFOBUF) 
      END IF


      DO 150 I=1, MPITEST_NUMP
        IF (RANKS(I) .NE. -1) THEN
          REMOTE2 = RANKS(I)
          GO TO 160
        END IF
 150  CONTINUE

 160  CONTINUE


      IF (COMM1 .NE. MPI_COMM_NULL) THEN
        CALL MPI_INTERCOMM_CREATE(COMM1, 0, PEERCOMM,
     &        REMOTE2, ICOMTAG, COMM, IERR)
        IF (IERR .NE. MPI_SUCCESS) THEN 
          WRITE(INFOBUF,99) 'MPI_INTERCOMM_CREATE() returned',
     $       IERR, ' in MPITEST_GET_INTERCOMM()'
          CALL MPITEST_MESSAGE(MPITEST_NONFATAL, INFOBUF) 
          INFOBUF=' ' 
          CALL MPI_ERROR_STRING(IERR, INFOBUF, ERRSIZE, ERR)
          CALL MPITEST_MESSAGE(MPITEST_FATAL, INFOBUF) 
        END IF

        CALL MPI_COMM_REMOTE_SIZE(COMM, SIZE, IERR)
        IF (IERR .NE. MPI_SUCCESS) THEN 
          WRITE(INFOBUF,99) 'MPI_COMM_REMOTE_SIZE() returned',
     $       IERR, ' in MPITEST_GET_INTERCOMM()'
          CALL MPITEST_MESSAGE(MPITEST_NONFATAL, INFOBUF) 
          INFOBUF=' ' 
          CALL MPI_ERROR_STRING(IERR, INFOBUF, ERRSIZE, ERR)
          CALL MPITEST_MESSAGE(MPITEST_FATAL, INFOBUF) 
        END IF

        CALL MPI_COMM_SIZE(COMM, SIZE2, IERR)
        IF (IERR .NE. MPI_SUCCESS) THEN 
          WRITE(INFOBUF,99) 'MPI_COMM_SIZE() returned',
     $       IERR, ' in MPITEST_GET_INTERCOMM()'
          CALL MPITEST_MESSAGE(MPITEST_NONFATAL, INFOBUF) 
          INFOBUF=' ' 
          CALL MPI_ERROR_STRING(IERR, INFOBUF, ERRSIZE, ERR)
          CALL MPITEST_MESSAGE(MPITEST_FATAL, INFOBUF) 
        END IF

        CALL MPI_COMM_RANK(COMM, MPITEST_CURRENT_RANK, IERR)
        IF (IERR .NE. MPI_SUCCESS) THEN 
          WRITE(INFOBUF,99) 'MPI_COMM_RANK() returned',
     $       IERR, ' in MPITEST_GET_INTERCOMM()'
          CALL MPITEST_MESSAGE(MPITEST_NONFATAL, INFOBUF) 
          INFOBUF=' ' 
          CALL MPI_ERROR_STRING(IERR, INFOBUF, ERRSIZE, ERR)
          CALL MPITEST_MESSAGE(MPITEST_FATAL, INFOBUF) 
        END IF

        CALL MPI_COMM_FREE(COMM1, IERR)
        IF (IERR .NE. MPI_SUCCESS) THEN 
          WRITE(INFOBUF,99) 'MPI_COMM_FREE() returned',
     $       IERR, ' in MPITEST_GET_INTERCOMM()'
          CALL MPITEST_MESSAGE(MPITEST_NONFATAL, INFOBUF) 
          INFOBUF=' ' 
          CALL MPI_ERROR_STRING(IERR, INFOBUF, ERRSIZE, ERR)
          CALL MPITEST_MESSAGE(MPITEST_FATAL, INFOBUF) 
        END IF

        MPITEST_INTER = 0

      ELSE IF (COMM2 .NE. MPI_COMM_NULL) THEN
        CALL MPI_INTERCOMM_CREATE(COMM2, 0, PEERCOMM,
     &        REMOTE1, ICOMTAG, COMM, IERR)
        IF (IERR .NE. MPI_SUCCESS) THEN 
          WRITE(INFOBUF,99) 'MPI_INTERCOMM_CREATE() returned',
     $       IERR, ' in MPITEST_GET_INTERCOMM()'
          CALL MPITEST_MESSAGE(MPITEST_NONFATAL, INFOBUF) 
          INFOBUF=' ' 
          CALL MPI_ERROR_STRING(IERR, INFOBUF, ERRSIZE, ERR)
          CALL MPITEST_MESSAGE(MPITEST_FATAL, INFOBUF) 
        END IF

        CALL MPI_COMM_REMOTE_SIZE(COMM, SIZE, IERR)
        IF (IERR .NE. MPI_SUCCESS) THEN 
          WRITE(INFOBUF,99) 'MPI_COMM_REMOTE_SIZE() returned',
     $       IERR, ' in MPITEST_GET_INTERCOMM()'
          CALL MPITEST_MESSAGE(MPITEST_NONFATAL, INFOBUF) 
          INFOBUF=' ' 
          CALL MPI_ERROR_STRING(IERR, INFOBUF, ERRSIZE, ERR)
          CALL MPITEST_MESSAGE(MPITEST_FATAL, INFOBUF) 
        END IF

        CALL MPI_COMM_SIZE(COMM, SIZE2, IERR)
        IF (IERR .NE. MPI_SUCCESS) THEN 
          WRITE(INFOBUF,99) 'MPI_COMM_SIZE() returned',
     $       IERR, ' in MPITEST_GET_INTERCOMM()'
          CALL MPITEST_MESSAGE(MPITEST_NONFATAL, INFOBUF) 
          INFOBUF=' ' 
          CALL MPI_ERROR_STRING(IERR, INFOBUF, ERRSIZE, ERR)
          CALL MPITEST_MESSAGE(MPITEST_FATAL, INFOBUF) 
        END IF

        CALL MPI_COMM_RANK(COMM, MPITEST_CURRENT_RANK, IERR)
        IF (IERR .NE. MPI_SUCCESS) THEN 
          WRITE(INFOBUF,99) 'MPI_COMM_RANK() returned',
     $       IERR, ' in MPITEST_GET_INTERCOMM()'
          CALL MPITEST_MESSAGE(MPITEST_NONFATAL, INFOBUF) 
          INFOBUF=' ' 
          CALL MPI_ERROR_STRING(IERR, INFOBUF, ERRSIZE, ERR)
          CALL MPITEST_MESSAGE(MPITEST_FATAL, INFOBUF) 
        END IF

        CALL MPI_COMM_FREE(COMM2, IERR)
        IF (IERR .NE. MPI_SUCCESS) THEN 
          WRITE(INFOBUF,99) 'MPI_COMM_FREE() returned',
     $       IERR, ' in MPITEST_GET_INTERCOMM()'
          CALL MPITEST_MESSAGE(MPITEST_NONFATAL, INFOBUF) 
          INFOBUF=' ' 
          CALL MPI_ERROR_STRING(IERR, INFOBUF, ERRSIZE, ERR)
          CALL MPITEST_MESSAGE(MPITEST_FATAL, INFOBUF) 
        END IF

        MPITEST_INTER = 1

      ELSE
        SIZE = 0
        SIZE2 = 0
        COMM = MPI_COMM_NULL
        MPITEST_CURRENT_RANK = MPI_UNDEFINED
        MPITEST_INTER = MPI_UNDEFINED
      ENDIF

      SIZE = SIZE + SIZE2
      CALL MPI_ALLGATHER(SIZE, 1, MPI_INTEGER, RANKS, 1, MPI_INTEGER,
     &       MPI_COMM_WORLD, IERR)
      IF (IERR .NE. MPI_SUCCESS) THEN 
        WRITE(INFOBUF,99) 'MPI_ALLGATHER() returned',
     $     IERR, ' in MPITEST_GET_INTERCOMM()'
        CALL MPITEST_MESSAGE(MPITEST_NONFATAL, INFOBUF) 
        INFOBUF=' ' 
        CALL MPI_ERROR_STRING(IERR, INFOBUF, ERRSIZE, ERR)
        CALL MPITEST_MESSAGE(MPITEST_FATAL, INFOBUF) 
      END IF
      DO 200 I=1, MPITEST_NUMP
        IF (RANKS(I) .NE. 0) THEN
          SIZE=RANKS(I)
          GO TO 201
        END IF
 200  CONTINUE

 201  CONTINUE


      CALL MPI_COMM_FREE(PEERCOMM, IERR)
      IF (IERR .NE. MPI_SUCCESS) THEN 
        WRITE(INFOBUF,99) 'MPI_COMM_FREE() returned',
     $     IERR, ' in MPITEST_GET_INTERCOMM()'
        CALL MPITEST_MESSAGE(MPITEST_NONFATAL, INFOBUF) 
        INFOBUF=' ' 
        CALL MPI_ERROR_STRING(IERR, INFOBUF, ERRSIZE, ERR)
        CALL MPITEST_MESSAGE(MPITEST_FATAL, INFOBUF) 
      END IF

      END
C     end of 'subroutine MPITEST_get_intercomm()'


      SUBROUTINE MPITEST_CREATE_COMMUNICATOR(INDEX, COMM, SIZE)
C     ******************************************************************
C     Create a new communicator.
C
C     Arguments :
C       index                INPUT integer index into MPITEST_comms()
C                             of beginning of desired comm's size token
C
C       comm                 OUTPUT integer new communicator
C
C       size                 OUTPUT integer size of the new communicator.
C
C     History :
C     12/08/95        Created       Greg Morrow
C     ******************************************************************
      INCLUDE 'mpitestf.h'
      INCLUDE 'mpif.h'

      INTEGER INDEX, COMM, SIZE

      INTEGER IERR, COMM_SIZE, I
      INTEGER OLD_GROUP, NEW_GROUP
      INTEGER ERR, ERRSIZE
      INTEGER RANKS(MAX_RANKS)

      CHARACTER*(IOSIZE) INFOBUF

 99   FORMAT(CHAR_FMT, INT_FMT, CHAR_FMT)

      CALL MPITEST_GET_COMM_SIZE(MPITEST_COMM_CREATE, INDEX, COMM_SIZE)

      CALL MPI_COMM_GROUP(MPI_COMM_WORLD, OLD_GROUP, IERR)
      IF (IERR .NE. MPI_SUCCESS) THEN 
        WRITE(INFOBUF,99) 'MPI_COMM_GROUP() returned',
     $     IERR, ' in MPITEST_CREATE_COMMUNICATOR()'
        CALL MPITEST_MESSAGE(MPITEST_NONFATAL, INFOBUF) 
        INFOBUF=' ' 
        CALL MPI_ERROR_STRING(IERR, INFOBUF, ERRSIZE, ERR)
        CALL MPITEST_MESSAGE(MPITEST_FATAL, INFOBUF) 
      END IF

      DO 100 I=1, COMM_SIZE
        IF (MPITEST_COMMS(INDEX) .EQ. MPITEST_COMM_COMINC) THEN
          RANKS(I) = MPITEST_COMMS(INDEX+1)+(I-1)*MPITEST_COMMS(INDEX+3)
        ELSE IF (MPITEST_COMMS(INDEX) .EQ. MPITEST_COMM_RNKLST)THEN
          RANKS(I) = MPITEST_COMMS(INDEX+1+I)
        ELSE
          RANKS(I) = I-1
        END IF
 100  CONTINUE

      CALL MPI_GROUP_INCL(OLD_GROUP, COMM_SIZE, RANKS,
     $       NEW_GROUP, IERR)
      IF (IERR .NE. MPI_SUCCESS) THEN 
        WRITE(INFOBUF,99) 'MPI_GROUP_INCL() returned',
     $     IERR, ' in MPITEST_CREATE_COMMUNICATOR()'
        CALL MPITEST_MESSAGE(MPITEST_NONFATAL, INFOBUF) 
        INFOBUF=' ' 
        CALL MPI_ERROR_STRING(IERR, INFOBUF, ERRSIZE, ERR)
        CALL MPITEST_MESSAGE(MPITEST_FATAL, INFOBUF) 
      END IF

      CALL MPI_GROUP_RANK(NEW_GROUP, MPITEST_CURRENT_RANK, IERR)
      IF (IERR .NE. MPI_SUCCESS) THEN 
        WRITE(INFOBUF,99) 'MPI_GROUP_RANK() returned',
     $     IERR, ' in MPITEST_CREATE_COMMUNICATOR()'
        CALL MPITEST_MESSAGE(MPITEST_NONFATAL, INFOBUF) 
        INFOBUF=' ' 
        CALL MPI_ERROR_STRING(IERR, INFOBUF, ERRSIZE, ERR)
        CALL MPITEST_MESSAGE(MPITEST_FATAL, INFOBUF) 
      END IF

      CALL MPI_COMM_CREATE(MPI_COMM_WORLD, NEW_GROUP, COMM, IERR)
      IF (IERR .NE. MPI_SUCCESS) THEN 
        WRITE(INFOBUF,99) 'MPI_COMM_CREATE() returned',
     $     IERR, ' in MPITEST_CREATE_COMMUNICATOR()'
        CALL MPITEST_MESSAGE(MPITEST_NONFATAL, INFOBUF) 
        INFOBUF=' ' 
        CALL MPI_ERROR_STRING(IERR, INFOBUF, ERRSIZE, ERR)
        CALL MPITEST_MESSAGE(MPITEST_FATAL, INFOBUF) 
      END IF

      CALL MPI_GROUP_FREE(NEW_GROUP, IERR)
      IF (IERR .NE. MPI_SUCCESS) THEN 
        WRITE(INFOBUF,99) 'MPI_GROUP_FREE(NEW_GROUP) returned',
     $     IERR, ' in MPITEST_CREATE_COMMUNICATOR()'
        CALL MPITEST_MESSAGE(MPITEST_NONFATAL, INFOBUF) 
        INFOBUF=' ' 
        CALL MPI_ERROR_STRING(IERR, INFOBUF, ERRSIZE, ERR)
        CALL MPITEST_MESSAGE(MPITEST_FATAL, INFOBUF) 
      END IF

      CALL MPI_GROUP_FREE(OLD_GROUP, IERR)
      IF (IERR .NE. MPI_SUCCESS) THEN 
        WRITE(INFOBUF,99) 'MPI_GROUP_FREE(OLD_GROUP) returned',
     $     IERR, ' in MPITEST_CREATE_COMMUNICATOR()'
        CALL MPITEST_MESSAGE(MPITEST_NONFATAL, INFOBUF) 
        INFOBUF=' ' 
        CALL MPI_ERROR_STRING(IERR, INFOBUF, ERRSIZE, ERR)
        CALL MPITEST_MESSAGE(MPITEST_FATAL, INFOBUF) 
      END IF

      SIZE = COMM_SIZE

      END
C     end of 'subroutine MPITEST_create_communicator()'

      SUBROUTINE COUNT_ELEMENTS_INCR_INDEX(INDEX, TOTAL)
C     ******************************************************************
C     Return the number of communicators of the type indexed by 'index'
C     in the configuration array MPITEST_comms().  Increment the index value to 
C     the index of the next communicator type token.
C
C     Arguments
C     index       INPUT/OUTPUT integer current index.  On entry, this 
C                  pointsto the index in array of the current
C                  comm_type specifier.  On exit, points
C                  to the index in array of the next comm_type specifier.
C
C     total       OUTPUT integer running count of the number of
C                  communicators of this type
C
C     History :
C     12/08/95        Created       Greg Morrow
C     ******************************************************************
      INCLUDE 'mpitestf.h'
      INTEGER INDEX, TOTAL

      INTEGER INDEX2, START, ENDD, INC, TYPE, ELEMENT, I

      TOTAL = 0
      INDEX2 = INDEX

      TYPE = MPITEST_COMMS(INDEX2)
      INDEX2 = INDEX2 + 1

      DO 100 I = 1, MPITEST_CFGSIZ
        ELEMENT = MPITEST_COMMS(INDEX2)
C       Exit conditions
        IF (ELEMENT .EQ. MPITEST_END_TOKEN) GO TO 200
        IF (ELEMENT .LT. MPITEST_COMM_COMMIN) GO TO 200

        TOTAL = TOTAL + 1

        IF (ELEMENT .EQ. MPITEST_COMM_COMINC) THEN
          INDEX2 = INDEX2 + 4
        ELSE IF (ELEMENT .EQ. MPITEST_COMM_RNKLST) THEN
          INDEX2 = INDEX2 + 2 + MPITEST_COMMS(INDEX2+1)
        ELSE
          INDEX2 = INDEX2 + 1
        END IF
 100  CONTINUE

 200  CONTINUE

      INDEX = INDEX2
      END
C     end of 'subroutine COUNT_ELEMENTS_INCR_INDEX()'



      SUBROUTINE MPITEST_NUM_COMM_SIZES(COMM_COUNT)
C     ******************************************************************
C     Return the number of communicators as dictated by the 
C     configuration array MPITEST_comms().
C
C     Arguments
C      comm_count    OUTPUT integer number of communicators defined in
C                     MPITEST_comms() array.
C
C     History :
C     12/08/95        Created       Greg Morrow
C     ******************************************************************
      INCLUDE 'mpitestf.h'
      INTEGER COMM_COUNT

      INTEGER I, INDEX, ELEMENT, TOTAL, NUMINTER, ITEMP
      INTEGER TYPE_IS_INC
      INTEGER TYPE_IS_LIST
      CHARACTER*(IOSIZE) INFOBUF

 99   FORMAT(CHAR_FMT, INT_FMT, CHAR_FMT)

      TOTAL = 0
      TYPE_IS_INC = 0
      TYPE_IS_LIST= 0
      NUMINTER = 0
      
C     Replace 'particular node tokens' and do error checking on the
C     elements of MPITEST_comms().

      DO 100 INDEX=1, MPITEST_CFGSIZ
C       The element in question
        ELEMENT = MPITEST_COMMS(INDEX)

C       Break condition, last token flag
        IF (ELEMENT .EQ. MPITEST_END_TOKEN) THEN
          GO TO 200
C
C   Set type_is_inc to .true. if the current
C   communicator size token is MPITEST_COMM_cominc (incremented 
C   node-spec), .false. otherwise.
C
        ELSE IF ((ELEMENT .EQ. MPITEST_COMM_WORLD) .OR.
     $       (ELEMENT .EQ. MPITEST_COMM_SELF)) THEN
          TYPE_IS_INC = 0
        ELSE IF ((ELEMENT .EQ. MPITEST_COMM_DUP) .OR.
     $       (ELEMENT .EQ. MPITEST_COMM_SPLIT)  .OR.
     $       (ELEMENT .EQ. MPITEST_COMM_CREATE) .OR.
     $       (ELEMENT .EQ. MPITEST_COMM_INTER)  .OR.
     $       (ELEMENT .EQ. MPITEST_COMM_MERGE)) THEN
          IF (MPITEST_COMMS(INDEX+1) .EQ. MPITEST_COMM_COMINC) THEN
            TYPE_IS_INC = 1
          ELSE
            TYPE_IS_INC = 0
          END IF
          IF (MPITEST_COMMS(INDEX+1) .EQ. MPITEST_COMM_RNKLST) THEN
            TYPE_IS_LIST = 1
          ELSE
            TYPE_IS_LIST = 0
          END IF
        END IF

C 
C   Replace the token 'MPITEST_COMM_LASTRNK' (last rank) with an actual
C   integer node spec.  If this is an incremented comm, then
C   'last rank' means the node spec for the last rank,
C   (i.e. MPITEST_nump-1).  If this token is the size of a comm,
C   (i.e. (type_is_inc .eq. .false.)) then MPITEST_nump is permissible,
C   so leave it.
C
        IF (ELEMENT .EQ. MPITEST_COMM_LASTRNK) THEN
          MPITEST_COMMS(INDEX) = MPITEST_NUMP-1

        ELSE IF (((ELEMENT .GT. MPITEST_NUMP) .AND. 
     $        (TYPE_IS_INC .NE. 5)) .OR.
     $       ((ELEMENT .EQ. MPITEST_NUMP) .AND.
     $        ((TYPE_IS_INC .EQ. 3) .OR. (TYPE_IS_INC .EQ.4) .OR.
     $         (TYPE_IS_LIST .GT. 3) ))) THEN
           MPITEST_COMMS(INDEX) = MPITEST_NUMP-1
        END IF

        IF (TYPE_IS_INC .NE. 0) THEN
          TYPE_IS_INC = TYPE_IS_INC + 1
        END IF
        IF (TYPE_IS_LIST .NE. 0) THEN
          TYPE_IS_LIST = TYPE_IS_LIST + 1
        END IF
         
 100  CONTINUE

 200  CONTINUE

      INDEX = 1
      DO 500 I=1, MPITEST_CFGSIZ
        ELEMENT = MPITEST_COMMS(INDEX)
         
C       Break if end of array
        IF (ELEMENT .EQ. MPITEST_END_TOKEN) GO TO 600

        IF (ELEMENT .EQ. MPITEST_COMM_WORLD) THEN
          TOTAL = TOTAL + 1
          INDEX = INDEX + 1
        ELSE IF (ELEMENT .EQ. MPITEST_COMM_SELF) THEN
          TOTAL = TOTAL + 1
          INDEX = INDEX + 1
        ELSE IF (ELEMENT .EQ. MPITEST_COMM_DUP) THEN
          CALL COUNT_ELEMENTS_INCR_INDEX(INDEX, ITEMP)
          TOTAL = TOTAL+ITEMP
        ELSE IF (ELEMENT .EQ. MPITEST_COMM_CREATE) THEN
          CALL COUNT_ELEMENTS_INCR_INDEX(INDEX, ITEMP)
          TOTAL = TOTAL+ITEMP
        ELSE IF (ELEMENT .EQ. MPITEST_COMM_SPLIT) THEN
          CALL COUNT_ELEMENTS_INCR_INDEX(INDEX, ITEMP)
          TOTAL = TOTAL + ITEMP
        ELSE IF (ELEMENT .EQ. MPITEST_COMM_INTER) THEN
          NUMINTER = NUMINTER + 1
          CALL COUNT_ELEMENTS_INCR_INDEX(INDEX, ITEMP)
          TOTAL = TOTAL + ITEMP
          CALL COUNT_ELEMENTS_INCR_INDEX(INDEX, ITEMP)
          TOTAL = TOTAL + ITEMP
        ELSE IF (ELEMENT .EQ. MPITEST_COMM_MERGE) THEN
          NUMINTER = NUMINTER + 1
          CALL COUNT_ELEMENTS_INCR_INDEX(INDEX, ITEMP)
          TOTAL = TOTAL + ITEMP
          CALL COUNT_ELEMENTS_INCR_INDEX(INDEX, ITEMP)
          TOTAL = TOTAL + ITEMP
        ELSE
          WRITE(INFOBUF,99)
     $      'Improperly defined MPITEST_COMMS() element',
     $      ELEMENT, ' detected in MPITEST_NUM_COMM_SIZES()'
          CALL MPITEST_MESSAGE(MPITEST_FATAL, INFOBUF) 
        END IF
 500  CONTINUE

 600  CONTINUE
      
      COMM_COUNT = TOTAL - NUMINTER
      END
C     end of 'subroutine MPITEST_num_comm_sizes()'
      


      SUBROUTINE MPITEST_GET_COMM_SIZE(TYPE, INDEX, SIZE)
C     ******************************************************************
C     Given a communicator type token and an index into the
C     MPITEST_comm_sizes() array, determine the communicator size.
C   
C     Arguments :
C     type        INPUT integer the communicator type token for the comm
C                  in question
C     index       INPUT integer the index into MPITEST_comm_sizes() of the
C                  comm in question
C     size        OUTPUT integer the size of the communicator.
C   
C     History :
C     12/08/95        Created       Greg Morrow
C     ******************************************************************
      INCLUDE 'mpitestf.h'
      INTEGER TYPE, INDEX, SIZE
      INTEGER START, ENDD, INC, ELEMENT

      IF (TYPE .EQ. MPITEST_COMM_WORLD) THEN
        SIZE = MPITEST_NUMP
      ELSE IF (TYPE .EQ. MPITEST_COMM_SELF) THEN
        SIZE = 1
      ELSE
        ELEMENT = MPITEST_COMMS(INDEX)
        IF (ELEMENT .EQ. MPITEST_COMM_COMINC) THEN
          START = MPITEST_COMMS(INDEX + 1)
          ENDD = MPITEST_COMMS(INDEX + 2)
          INC = MPITEST_COMMS(INDEx + 3)
          SIZE = 1 + (ENDD-START)/INC
        ELSE IF (ELEMENT .EQ. MPITEST_COMM_RNKLST) THEN
          SIZE = MPITEST_COMMS(INDEX+1)
        ELSE IF (ELEMENT .EQ. MPITEST_COMM_ALL) THEN
          SIZE = MPITEST_NUMP
        ELSE IF (ELEMENT .EQ. MPITEST_COMM_ALLBUT1) THEN
          SIZE = MPITEST_NUMP-1
        ELSE IF (ELEMENT .EQ. MPITEST_COMM_HALF) THEN
          SIZE = MPITEST_NUMP/2
        ELSE IF (ELEMENT .EQ. MPITEST_COMM_ONE) THEN
          SIZE = 1
        ELSE
          SIZE = ELEMENT
        END IF 
      END IF

      END
C     end of 'subroutine MPITEST_get_comm_size()'



      SUBROUTINE FIND_COMM_INDEX(INDEX1, COUNT, INDEX)
C     ******************************************************************
C     Given an index into the MPITEST_comms() array is the index of a
C     communicator type token in the array, and a count which is
C     the rank among comms of the type array[index] whose size is desired,
C     return the index in array() of the specified communicator.
C
C     Arguments :
C     index1   INPUT integer index in MPITEST_comm_sizes() of the
C               current comm_type token.
C
C     count    INPUT integer rank among communicators in the current
C               incantation of the current comm_type whose size is desired.
C
C     index    OUTPUT integer index into the array to be used in future
C               calls to create communicators.
C
C     Algorithm :
C     Count through the communicators until we are on the one
C     specified by count.  Then return its index.
C
C     History :
C     12/08/95        Created       Greg Morrow
C     ******************************************************************
      INCLUDE 'mpitestf.h'
      INTEGER INDEX1, COUNT, INDEX

      INTEGER STEP, SIZE, ELEMENT, TYPE, START, ENDD, INC, I

      STEP = 1
      INDEX = INDEX1
      TYPE = MPITEST_COMMS(INDEX)

      INDEX = INDEX + 1
      ELEMENT = MPITEST_COMMS(INDEX)

      DO 100 I=1,MPITEST_CFGSIZ
C       Break criteria
        IF (STEP .EQ. COUNT) GO TO 200

        IF (ELEMENT .EQ. MPITEST_COMM_COMINC) THEN
          INDEX = INDEX + 4
        ELSE IF (ELEMENT .EQ. MPITEST_COMM_RNKLST) THEN
          INDEX = INDEX + 1
          SIZE = MPITEST_COMMS(INDEX)
          INDEX = INDEX + SIZE + 1
        ELSE
          INDEX = INDEX + 1
        END IF
        STEP = STEP + 1
        ELEMENT = MPITEST_COMMS(INDEX)

 100  CONTINUE

 200  CONTINUE

      END
C     end of 'subroutine FIND_COMM_INDEX()'



      SUBROUTINE MPITEST_GET_COMM_INDEX(ITER, COMM_INDEX)
C     ******************************************************************
C     Determine the index in the MPITEST_comms() array of the
C     communicator for iteration i of the communicator
C     loop.
C
C     Arguments :
C     iter        INPUT integer iteration for which we want the communicator
C                  size.
C
C     comm_index  OUTPUT integer the ith communicator's index in
C                  MPITEST_comms().
C
C     Note that if the size token is MPITEST_comm_inc, then the index
C     returned is that of the 'start' element (i.e. the first element
C     after the MPITEST_comm_inc token.)
C
C     History :
C     12/08/95        Created       Greg Morrow
C     ******************************************************************
      INCLUDE 'mpitestf.h'
      INTEGER ITER
      INTEGER COMM_INDEX

      INTEGER INDEX, ITERATION, ELEMENT, INCREMENT, THE_INDEX, MAX_I
      INTEGER TEMP_INDEX, I
      SAVE MAX_I
      CHARACTER*(IOSIZE) INFOBUF

 99   FORMAT(CHAR_FMT, INT_FMT, CHAR_FMT)

      INDEX = 1
      TEMP_INDEX = 1
      ITERATION = 1
      THE_INDEX = -1

      IF (ITER .EQ. 1) THEN
        CALL MPITEST_NUM_COMM_SIZES(MAX_I)
      ELSE IF (ITER .GT. MAX_I) THEN
        WRITE(INFOBUF,99) 'Iteration count',
     $         ITER, ' too large in MPITEST_COMM_INDEX()'
        CALL MPITEST_MESSAGE(MPITEST_FATAL, INFOBUF)
      END IF

      DO 100 I=1,MPITEST_CFGSIZ
        IF (THE_INDEX .NE. -1) GO TO 200
        IF (ITERATION .GT. ITER) GO TO 200
        ELEMENT = MPITEST_COMMS(INDEX)
        IF (ELEMENT .EQ. MPITEST_END_TOKEN) GO TO 200

        IF (ELEMENT .EQ. MPITEST_COMM_WORLD) THEN
          IF (ITERATION .EQ. ITER) THE_INDEX = INDEX
          INDEX = INDEX + 1
          ITERATION = ITERATION + 1

        ELSE IF (ELEMENT .EQ. MPITEST_COMM_SELF) THEN
          IF (ITERATION .EQ. ITER) THE_INDEX = INDEX
          INDEX = INDEX + 1
          ITERATION = ITERATION + 1

        ELSE IF ((ELEMENT .EQ. MPITEST_COMM_DUP)  .OR.
     $          (ELEMENT .EQ. MPITEST_COMM_SPLIT).OR.
     $          (ELEMENT .EQ. MPITEST_COMM_CREATE)) THEN
          TEMP_INDEX = INDEX
          CALL COUNT_ELEMENTS_INCR_INDEX(TEMP_INDEX, INCREMENT)
          IF (ITER .LT. (ITERATION + INCREMENT)) THEN
            CALL FIND_COMM_INDEX(INDEX, ITER-ITERATION+1, THE_INDEX)
          ELSE
            ITERATION = ITERATION + INCREMENT
            INDEX = TEMP_INDEX
          END IF
        ELSE IF ((ELEMENT .EQ. MPITEST_COMM_INTER) .OR.
     $          (ELEMENT .EQ. MPITEST_COMM_MERGE)) THEN
          IF (ITER .EQ. ITERATION) THE_INDEX = INDEX
          INDEX = INDEX + 1
          ITERATION = ITERATION - 1
        ELSE
          WRITE(INFOBUF,99) 
     $       'Improperly defined MPITEST_COMMS() element',
     $       ELEMENT, ' detected in MPITEST_COMM_INDEX()'
          CALL MPITEST_MESSAGE(MPITEST_FATAL, INFOBUF)
        END IF

 100  CONTINUE
 200  CONTINUE
      COMM_INDEX = THE_INDEX
      RETURN
      END
C     end of 'subroutine MPITEST_get_comm_index()'




      SUBROUTINE MPITEST_GET_COMM_TYPE(ITER, COMM_TYPE)
C     ******************************************************************
C     Determine the communicator type for iteration i of the communicator
C     loop, as specified in the array MPITEST_comm_sizes(), defined in
C     mpitest_cfgf.h .
C
C     Arguments :
C     iter        INPUT integer iteration for which we want the
C                  communicator type.
C
C     comm_type   OUTPUT integer MPITEST communicator type token
C                  corresponding to the ith communicator.
C
C     History :
C     12/08/95        Created       Greg Morrow
C     ******************************************************************
      INCLUDE 'mpitestf.h'
      INTEGER ITER, COMM_TYPE

      INTEGER INDEX, TEMP_INDEX, ITERATION, ELEMENT, INCR, TYPE, MAX_I
      INTEGER I
      SAVE MAX_I
      CHARACTER*(IOSIZE) INFOBUF

 99   FORMAT(CHAR_FMT, INT_FMT, CHAR_FMT)

      INDEX = 1
      TEMP_INDEX = 1
      ITERATION = 1
      TYPE = 0

      IF (ITER .EQ. 1) THEN
        CALL MPITEST_NUM_COMM_SIZES(MAX_I)
      ELSE IF (ITER .GT. MAX_I) THEN
        WRITE(INFOBUF,99) 'Iteration count',
     $         ITER, ' too large in MPITEST_GET_COMM_TYPE()'
        CALL MPITEST_MESSAGE(MPITEST_FATAL, INFOBUF)
      END IF

      DO 100 I=1,MPITEST_CFGSIZ
C       Break conditions
        IF (TYPE .NE. 0) GO TO 200
        IF (ITERATION .GT. ITER) GO TO 200
        ELEMENT = MPITEST_COMMS(INDEX)
        IF (ELEMENT .EQ. MPITEST_END_TOKEN) GO TO 200
         
        IF (ELEMENT .EQ. MPITEST_COMM_WORLD) THEN
          IF (ITERATION .EQ. ITER) TYPE = MPITEST_COMM_WORLD
          INDEX = INDEX + 1
          ITERATION = ITERATION + 1

        ELSE IF (ELEMENT .EQ. MPITEST_COMM_SELF) THEN
          IF (ITERATION .EQ. ITER) TYPE = MPITEST_COMM_SELF
          INDEX = INDEX + 1
          ITERATION = ITERATION + 1

        ELSE IF (ELEMENT .EQ. MPITEST_COMM_DUP) THEN
          TEMP_INDEX = INDEX
          CALL COUNT_ELEMENTS_INCR_INDEX(TEMP_INDEX, INCR)
          IF (ITER .LT. (ITERATION + INCR)) THEN
            TYPE = MPITEST_COMM_DUP
          ELSE
            ITERATION = ITERATION + INCR
            INDEX = TEMP_INDEX
          END IF

        ELSE IF (ELEMENT .EQ. MPITEST_COMM_SPLIT) THEN
          TEMP_INDEX = INDEX
          CALL COUNT_ELEMENTS_INCR_INDEX(TEMP_INDEX, INCR)
          IF (ITER .LT. (ITERATION + INCR)) THEN
            TYPE = MPITEST_COMM_SPLIT
          ELSE
            ITERATION = ITERATION + INCR
            INDEX = TEMP_INDEX
          END IF

        ELSE IF (ELEMENT .EQ. MPITEST_COMM_CREATE) THEN
          TEMP_INDEX = INDEX
          CALL COUNT_ELEMENTS_INCR_INDEX(TEMP_INDEX, INCR)
          IF (ITER .LT. (ITERATION + INCR)) THEN
            TYPE = MPITEST_COMM_CREATE
          ELSE
            ITERATION = ITERATION + INCR
            INDEX = TEMP_INDEX
          END IF

        ELSE IF (ELEMENT .EQ. MPITEST_COMM_INTER) THEN
          IF (ITER .EQ. ITERATION) TYPE = MPITEST_COMM_INTER
          INDEX = INDEX + 1
          ITERATION = ITERATION - 1

        ELSE IF (ELEMENT .EQ. MPITEST_COMM_MERGE) THEN
          IF (ITER .EQ. ITERATION) TYPE = MPITEST_COMM_MERGE
          INDEX = INDEX + 1
          ITERATION = ITERATION - 1

        ELSE
          WRITE(INFOBUF,99) 'Improperly defined MPITEST_COMMS element',
     $      ELEMENT, ' detected in MPITEST_GET_COMM_TYPE()'
          CALL MPITEST_MESSAGE(MPITEST_FATAL, INFOBUF)
        END IF

 100  CONTINUE

 200  CONTINUE

      COMM_TYPE = TYPE
      END
C     end of 'subroutine MPITEST_get_comm_type()'


      SUBROUTINE MPITEST_FREE_COMMUNICATOR(COMM_TYPE, COMM)
C     ******************************************************************
C     Free an MPI communicator.
C     
C     Arguments : 
C     integer comm_type    The communicator type, as defined in mpitestf.h
C     
C     integer comm         The handle to the communicator to be freed.
C     
C     If comm_type = MPITEST_COMM_WORLD (i.e. comm=MPI_COMM_WORLD) or 
C     MPITEST_COMM_SELF then it is not freed.  For the other type of
C     communicators, comm is only freed if it is not MPI_COMM_NULL.
C     
C     
C     History :
C     12/07/95        Created       Greg Morrow
C     ******************************************************************
      INCLUDE 'mpitestf.h'
      INCLUDE 'mpif.h'

      INTEGER COMM_TYPE
      INTEGER COMM

      INTEGER IERR, SIZE, ERR, ERRSIZE
      CHARACTER*(IOSIZE) INFOBUF
 99   FORMAT(CHAR_FMT, INT_FMT, CHAR_FMT)

      IF ((COMM_TYPE .EQ. MPITEST_COMM_WORLD) .OR. 
     $    (COMM_TYPE .EQ. MPITEST_COMM_SELF)) THEN
         COMM = MPI_COMM_NULL

      ELSE
        IF (COMM .NE. MPI_COMM_NULL) THEN
          CALL MPI_COMM_FREE(COMM, IERR)
          IF (IERR .NE. MPI_SUCCESS) THEN 
            WRITE(INFOBUF,99) 'MPI_COMM_FREE() returned',
     $         IERR, ' in MPITEST_FREE_COMMUNICATOR()'
            CALL MPITEST_MESSAGE(MPITEST_NONFATAL, INFOBUF) 
            INFOBUF=' ' 
            CALL MPI_ERROR_STRING(IERR, INFOBUF, ERRSIZE, ERR)
            CALL MPITEST_MESSAGE(MPITEST_FATAL, INFOBUF) 
          END IF
        END IF
      END IF

      END
C     end 'subroutine MPITEST_free_communicator()'
C
      integer function MPITEST_datatype_has_sticky_lb( type, lb_off )
      integer type, lb_off
C/***********************************************************************
C  Test to see if a datatype has a sticky LB, and if so return the offset
C
C  Arguments:
C  MPI_Datatype type       MPI datatype to test for presence of sticky LB
C  MPI_Aint *lb_off_p      pointer to region to store sticky LB value if
C                          there is one.
C
C  Return value: 0 if no sticky LB, -1 on error, 1 otherwise.  If there is
C                a sticky LB, its offset in the type is returned in the
C                region pointed to by lb_off_p.
C
C  We grab the LB of the type as is, then we explicitly set a sticky LB at
C  an offset one above this value.  If there is a sticky LB on the type
C  already, then the LB of the resulting type will be identical to the
C  original type; otherwise we know that there is no sticky LB on the type.
C
C  History:
C
C  11/11/2002   Rob Ross (ANL) Created to fix broken LB tests (they make
C               poor assumptions about the types that MPITEST_get_datatypes
C	       will return, assuming that they do not have sticky LBs).
C
C See the corresponding routine in libmpitest.c for specifics
C************************************************************************/
      INCLUDE 'mpif.h'
      integer tmptype
      integer orig_lb, new_lb
      integer err, blks(2)
      integer disps(2)
      integer types(2)
      
      data blks/1,1/

      MPITEST_datatype_has_sticky_lb = -1
      call MPI_Type_lb(type, orig_lb, err )
      if (err .ne. MPI_SUCCESS) return
      
      types(1) = MPI_LB
      types(2) = type
      disps(1) = orig_lb + 1
      disps(2) = 0

      call MPI_Type_struct(2, blks, disps, types, tmptype, err )
      if (err .ne. MPI_SUCCESS) return
      
      call MPI_Type_lb(tmptype, new_lb, err )
      call MPI_Type_free(tmptype, err)
      if (err .ne. MPI_SUCCESS) return
      if (new_lb .eq. orig_lb) then
C     there is a sticky LB 
         lb_off = orig_lb
         MPITEST_datatype_has_sticky_lb = 1
      else 
         MPITEST_datatype_has_sticky_lb = 0
      endif
      return
      end

      integer function MPITEST_datatype_has_sticky_ub( type, ub_off)
C/***********************************************************************
C  Test to see if a datatype has a sticky UB, and if so return the offset
C
C  Arguments:
C  MPI_Datatype type       MPI datatype to test for presence of sticky UB
C  MPI_Aint *ub_off_p      pointer to region to store sticky UB value if
C                          there is one.
C
C  Return value: 0 if no sticky UB, -1 on error, 1 otherwise.  If there is
C                a sticky UB, its offset in the type is returned in the
C                region pointed to by ub_off_p.
C
C  We grab the UB of the type as is, then we explicitly set a sticky UB at
C  an offset one below this value.  If there is a sticky UB on the type
C  already, then the UB of the resulting type will be identical to the
C  original type; otherwise we know that there is no sticky UB on the type.
C
C  History:
C
C  11/11/2002   Rob Ross (ANL) Created to fix broken UB tests (they make
C               poor assumptions about the types that MPITEST_get_datatypes
C	       will return, assuming that they do not have sticky UBs).
C************************************************************************/
      INCLUDE 'mpif.h'
      integer tmptype
      integer orig_ub, new_ub
      integer err, blks(2)
      integer disps(2)
      integer types(2)

      data blks/1,1/

      MPITEST_datatype_has_sticky_ub = -1

      call MPI_Type_ub(type, orig_ub, err)
      if (err .ne. MPI_SUCCESS) return 
      
      types(1) = MPI_UB
      types(2) = type
      disps(1) = orig_ub - 1
      disps(2) = 0

      call MPI_Type_struct(2, blks, disps, types, tmptype, err )
      if (err .ne. MPI_SUCCESS) return
    
      call MPI_Type_ub(tmptype, new_ub, err)
      call MPI_Type_free(tmptype, err)
      if (err .ne. MPI_SUCCESS) return
      if (new_ub .eq. orig_ub) then
C     there is a sticky UB
         ub_off = orig_ub
         MPITEST_datatype_has_sticky_ub = 1
      else 
         MPITEST_datatype_has_sticky_ub = 0
      endif
      return
      end


