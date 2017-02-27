#include "foptions.h"

      SUBROUTINE SET_DEFAULT_OPS(OP_ARRAY, NUMBER)
C ************************************************************************
C  Sets up the default operation array.  Returns the number of default 
C  operations.
C ************************************************************************
      INCLUDE 'mpitestf.h'
      INCLUDE 'mpif.h'

      INTEGER OP_ARRAY(10)
      INTEGER NUMBER


      OP_ARRAY(1) = MPI_MAX
      OP_ARRAY(2) = MPI_MIN
      OP_ARRAY(3) = MPI_SUM
      OP_ARRAY(4) = MPI_PROD
      OP_ARRAY(5) = MPI_LAND
      OP_ARRAY(6) = MPI_LOR
      OP_ARRAY(7) = MPI_LXOR
      OP_ARRAY(8) = MPI_BAND
      OP_ARRAY(9) = MPI_BOR
      OP_ARRAY(10) =MPI_BXOR
  
      NUMBER = 10

      END


      SUBROUTINE HAS_OP(OP, TEST_TYPE, HASOP)
C ************************************************************************
C Determines whether a particular operation may be applied to a particular 
C data type, as specified in section 4.9.2 of the MPI Standard.
C ************************************************************************
      INCLUDE 'mpitestf.h'
      INCLUDE 'externalf.h'

      INTEGER OP
      INTEGER TEST_TYPE
      LOGICAL HASOP

      IF (TEST_TYPE .EQ. MPITEST_INTEGER) THEN
C       Logical operations not supported on fortran integers
        IF (((OP .GE. 1) .AND. (OP .LE. 4)) .OR.
     $      ((OP .GE. 8) .AND. (OP .LE. 10))) THEN
          HASOP = .TRUE.
        ELSE
          HASOP = .FALSE.
        END IF

      ELSE IF ((TEST_TYPE .EQ. MPITEST_REAL) .OR.
     $         (TEST_TYPE .EQ. MPITEST_DOUBLE_PRECISION)) THEN
C       Only the first four operations (min, max, sum, product) apply
C       to float and double
        IF ((OP .GE. 1) .AND. (OP .LE. 4)) THEN
          HASOP = .TRUE.
        ELSE
          HASOP = .FALSE.
        END IF

      ELSE IF (TEST_TYPE .EQ. MPITEST_LOGICAL) THEN
C       Only the logical operations apply
        IF ((OP .GE. 5) .AND. (OP .LE. 7)) THEN
          HASOP = .TRUE.
        ELSE
          HASOP = .FALSE.
        END IF

      ELSE IF (TEST_TYPE .EQ. MPITEST_COMPLEX) THEN
C       Only sum, product apply
        IF ((OP .GE. 3) .AND. (OP .LE. 4)) THEN
          HASOP = .TRUE.
        ELSE
          HASOP = .FALSE.
        END IF

      ELSE
        HASOP = .FALSE.
      END IF

      END


      SUBROUTINE APPLY_COMPLEX_OP(OP_INDEX, X1, X2, ANSWER)
C ***************************************************************************
C
C ***************************************************************************
C
      INTEGER OP_INDEX
      COMPLEX X1, X2
      COMPLEX ANSWER

      IF (OP_INDEX .EQ. 3) THEN
         ANSWER = X1 + X2
      ELSE IF (OP_INDEX .EQ. 4) THEN
         ANSWER = X1 * X2
      END IF

      END


      SUBROUTINE MY_INT_BAND(X1, X2, ANSWER)
C ***************************************************************************
C     Bitwise AND operation for INTEGER
C ***************************************************************************
C
      INCLUDE 'mpitestf.h'
      INCLUDE 'mpif.h'

      INTEGER X1, X2, ANSWER
      INTEGER REMAIN, ACC, SIZE, ERROR, IERR, I

      CHARACTER*(MAX_BUFF_SIZE) BITS1, BITS2
      CHARACTER*(IOSIZE) INFOBUF

      CALL MPI_TYPE_SIZE(MPI_INTEGER, SIZE, ERROR)

      IF (ERROR .NE. MPI_SUCCESS) THEN
         CALL MPI_ERROR_STRING(ERROR, INFOBUF, SIZE, IERR)
         CALL MPITEST_MESSAGE(MPITEST_FATAL, INFOBUF)
      ELSE
C        Initializes input bits strings
         DO 100 I = 1, SIZE * 8, 1
            BITS1(I:I) = '0'
            BITS2(I:I) = '0'
 100     CONTINUE

         ACC = X1
         I = 1

 200     IF (ACC .EQ. 0) THEN
            GO TO 300
         ELSE
            REMAIN = MOD(ACC, 2)
            ACC = ACC / 2
            IF (REMAIN .EQ. 0) THEN
               BITS1(I:I) = '0'
            ELSE
               BITS1(I:I) = '1'
            END IF
            I = I + 1
            GO TO 200
         END IF

 300     ACC = X2
         I = 1

 400     IF (ACC .EQ. 0) THEN
            GO TO 500
         ELSE
            REMAIN = MOD(ACC, 2)
            ACC = ACC / 2
            IF (REMAIN .EQ. 0) THEN
               BITS2(I:I) = '0'
            ELSE
               BITS2(I:I) = '1'
            END IF
            I = I + 1
            GO TO 400
         END IF

C        Bitwise AND
 500     ANSWER = 0

         DO 600 I = SIZE * 8, 1, -1
            IF ((BITS1(I:I) .EQ. '1') .AND. (BITS2(I:I) .EQ. '1')) THEN
               ANSWER = ANSWER * 2 + 1
            ELSE
               ANSWER = ANSWER * 2
            END IF
 600     CONTINUE

      END IF

      END


      SUBROUTINE MY_INT_BOR(X1, X2, ANSWER)
C ***************************************************************************
C     Bitwise OR operation for INTEGER
C ***************************************************************************
C
      INCLUDE 'mpitestf.h'
      INCLUDE 'mpif.h'

      INTEGER X1, X2, ANSWER
      INTEGER REMAIN, ACC, SIZE, ERROR, IERR, I

      CHARACTER*(MAX_BUFF_SIZE) BITS1, BITS2
      CHARACTER*(IOSIZE) INFOBUF

      CALL MPI_TYPE_SIZE(MPI_INTEGER, SIZE, ERROR)

      IF (ERROR .NE. MPI_SUCCESS) THEN
         CALL MPI_ERROR_STRING(ERROR, INFOBUF, SIZE, IERR)
         CALL MPITEST_MESSAGE(MPITEST_FATAL, INFOBUF)
      ELSE
C        Initializes input bits strings
         DO 100 I = 1, SIZE * 8, 1
            BITS1(I:I) = '0'
            BITS2(I:I) = '0'
 100     CONTINUE

         ACC = X1
         I = 1

 200     IF (ACC .EQ. 0) THEN
            GO TO 300
         ELSE
            REMAIN = MOD(ACC, 2)
            ACC = ACC / 2
            IF (REMAIN .EQ. 0) THEN
               BITS1(I:I) = '0'
            ELSE
               BITS1(I:I) = '1'
            END IF
            I = I + 1
            GO TO 200
         END IF

 300     ACC = X2
         I = 1

 400     IF (ACC .EQ. 0) THEN
            GO TO 500
         ELSE
            REMAIN = MOD(ACC, 2)
            ACC = ACC / 2
            IF (REMAIN .EQ. 0) THEN
               BITS2(I:I) = '0'
            ELSE
               BITS2(I:I) = '1'
            END IF
            I = I + 1
            GO TO 400
         END IF

C        Bitwise OR
 500     ANSWER = 0

         DO 600 I = SIZE * 8, 1, -1
            IF ((BITS1(I:I) .EQ. '1') .OR. (BITS2(I:I) .EQ. '1')) THEN
               ANSWER = ANSWER * 2 + 1
            ELSE
               ANSWER = ANSWER * 2
            END IF
 600     CONTINUE

      END IF

      END


      SUBROUTINE MY_INT_BXOR(X1, X2, ANSWER)
C ***************************************************************************
C     Bitwise XOR operation for INTEGER
C ***************************************************************************
C
      INCLUDE 'mpitestf.h'
      INCLUDE 'mpif.h'

      INTEGER X1, X2, ANSWER
      INTEGER REMAIN, ACC, SIZE, ERROR, IERR, I

      CHARACTER*(MAX_BUFF_SIZE) BITS1, BITS2
      CHARACTER*(IOSIZE) INFOBUF

      CALL MPI_TYPE_SIZE(MPI_INTEGER, SIZE, ERROR)

      IF (ERROR .NE. MPI_SUCCESS) THEN
         CALL MPI_ERROR_STRING(ERROR, INFOBUF, SIZE, IERR)
         CALL MPITEST_MESSAGE(MPITEST_FATAL, INFOBUF)
      ELSE
C        Initializes input bits strings
         DO 100 I = 1, SIZE * 8, 1
            BITS1(I:I) = '0'
            BITS2(I:I) = '0'
 100     CONTINUE

         ACC = X1
         I = 1

 200     IF (ACC .EQ. 0) THEN
            GO TO 300
         ELSE
            REMAIN = MOD(ACC, 2)
            ACC = ACC / 2
            IF (REMAIN .EQ. 0) THEN
               BITS1(I:I) = '0'
            ELSE
               BITS1(I:I) = '1'
            END IF
            I = I + 1
            GO TO 200
         END IF

 300     ACC = X2
         I = 1

 400     IF (ACC .EQ. 0) THEN
            GO TO 500
         ELSE
            REMAIN = MOD(ACC, 2)
            ACC = ACC / 2
            IF (REMAIN .EQ. 0) THEN
               BITS2(I:I) = '0'
            ELSE
               BITS2(I:I) = '1'
            END IF
            I = I + 1
            GO TO 400
         END IF

C        Bitwise XOR
 500     ANSWER = 0

         DO 600 I = SIZE * 8, 1, -1
            IF (BITS1(I:I) .NE. BITS2(I:I)) THEN
               ANSWER = ANSWER * 2 + 1
            ELSE
               ANSWER = ANSWER * 2
            END IF
 600     CONTINUE

      END IF

      END


      SUBROUTINE APPLY_INT_OP(OP_INDEX, X1, X2, ANSWER)
C ***************************************************************************
C Applies a binary operator to the two integers x1 and x2, returning the 
C result.  The binary operation is determined by the integer op_index.  The 
C mapping of op_index to operation is determined by the array 
C MPITEST_default_ops[], which is set at runtime in the main test code.
C ***************************************************************************
      INTEGER OP_INDEX
      INTEGER X1
      INTEGER X2
      INTEGER ANSWER
  
      EXTERNAL MY_INT_BAND
      EXTERNAL MY_INT_BOR
      EXTERNAL MY_INT_BXOR

      IF (OP_INDEX .EQ. 1) THEN
C       maximum
        ANSWER = MAX(X1, X2)
      ELSE IF (OP_INDEX .EQ. 2) THEN
C       minimum
        ANSWER = MIN(X1, X2)
      ELSE IF (OP_INDEX .EQ. 3) THEN
C       sum
        ANSWER = X1 + X2
      ELSE IF (OP_INDEX .EQ. 4) THEN
C       product
        ANSWER = X1*X2
      ELSE IF (OP_INDEX .EQ. 8) THEN
C       logical and
C        ANSWER = AND(X1, X2)
         CALL MY_INT_BAND(X1, X2, ANSWER)
      ELSE IF (OP_INDEX .EQ. 9) THEN
C       logical or
C        ANSWER = OR(X1, X2)
         CALL MY_INT_BOR(X1, X2, ANSWER)
      ELSE IF (OP_INDEX .EQ. 10) THEN
C       logical xor
C        ANSWER = XOR(X1, X2)
         CALL MY_INT_BXOR(X1, X2, ANSWER)
      END IF
      END


      SUBROUTINE APPLY_DOUBLE_OP(OP_INDEX, X1, X2, ANSWER)
C ***************************************************************************
C Applies a binary operator to the two doubles x1 and x2, returning the 
C result.  The binary operation is determined by the integer op_index.  The 
C mapping of op_index to operation is determined by the array 
C MPITEST_default_ops[], which is set at runtime in the main test code.
C ***************************************************************************
      INTEGER OP_INDEX
      DOUBLE PRECISION X1, X2
      DOUBLE PRECISION ANSWER

      IF (OP_INDEX .EQ. 1) THEN
C       maximum
        ANSWER = MAX(X1, X2)
      ELSE IF (OP_INDEX .EQ. 2) THEN
C       minimum
        ANSWER = MIN(X1, X2)
      ELSE IF (OP_INDEX .EQ. 3) THEN
C       sum
        ANSWER = X1 + X2
      ELSE IF (OP_INDEX .EQ. 4) THEN
C       product
        ANSWER = X1*X2
      END IF
      END


      SUBROUTINE APPLY_REAL_OP(OP_INDEX, X1, X2, ANSWER)
C ***************************************************************************
C Applies a binary operator to the two doubles x1 and x2, returning the 
C result.  The binary operation is determined by the integer op_index.  The 
C mapping of op_index to operation is determined by the array 
C MPITEST_default_ops[], which is set at runtime in the main test code.
C ***************************************************************************
      INTEGER OP_INDEX
      REAL X1, X2
      REAL ANSWER

      IF (OP_INDEX .EQ. 1) THEN
C       maximum
        ANSWER = MAX(X1, X2)
      ELSE IF (OP_INDEX .EQ. 2) THEN
C       minimum
        ANSWER = MIN(X1, X2)
      ELSE IF (OP_INDEX .EQ. 3) THEN
C       sum
        ANSWER = X1 + X2
      ELSE IF (OP_INDEX .EQ. 4) THEN
C       product
        ANSWER = X1*X2
      END IF
      END


      SUBROUTINE APPLY_LOGICAL_OP(OP_INDEX, X1, X2, ANSWER)
C ***************************************************************************
C
C ***************************************************************************
      INTEGER OP_INDEX
      LOGICAL X1, X2
      LOGICAL ANSWER

      IF (OP_INDEX .EQ. 5) THEN
         ANSWER = X1 .AND. X2
      ELSE IF (OP_INDEX .EQ. 6) THEN
         ANSWER = X1 .OR. X2
      ELSE IF (OP_INDEX .EQ. 7) THEN
         IF (X1 .NEQV. X2) THEN
            ANSWER = .TRUE.
         ELSE
            ANSWER = .FALSE.
         END IF
      END IF
      END


      SUBROUTINE APPLY_OP(OP_INDEX, X1, X2, ANSWER, TYPE)
C ************************************************************************
C
C ************************************************************************
C
      INCLUDE 'mpitestf.h'

      INTEGER OP_INDEX, TYPE
      MPITEST_BUF_TYPE X1, X2, ANSWER

      EXTERNAL APPLY_INT_OP
      EXTERNAL APPLY_REAL_OP
      EXTERNAL APPLY_DOUBLE_OP
      EXTERNAL APPLY_LOGICAL_OP
      EXTERNAL APPLY_COMPLEX_OP

      IF (TYPE .EQ. MPITEST_INTEGER) THEN
         CALL APPLY_INT_OP(OP_INDEX, X1, X2, ANSWER)
      ELSE IF (TYPE .EQ. MPITEST_REAL) THEN
         CALL APPLY_REAL_OP(OP_INDEX, X1, X2, ANSWER)
      ELSE IF (TYPE .EQ. MPITEST_DOUBLE_PRECISION) THEN
         CALL APPLY_DOUBLE_OP(OP_INDEX, X1, X2, ANSWER)
      ELSE IF (TYPE .EQ. MPITEST_LOGICAL) THEN
         CALL APPLY_LOGICAL_OP(OP_INDEX, X1, X2, ANSWER)
      ELSE IF (TYPE .EQ. MPITEST_COMPLEX) THEN
         CALL APPLY_COMPLEX_OP(OP_INDEX, X1, X2, ANSWER)
      END IF

      END


      SUBROUTINE GET_REDUCE_ANSWER(OP_INDEX, NUMP, ANSWER, TYPE)
C ************************************************************************
C Apply the binary operation specified by op_index to the numbers 
C (0, 1, 2, ..., nump-1), and fill in the dataTamplate object based on the 
C results.  The mapping of op_index to operation is determined by the array 
C MPITEST_default_ops[], which is set at runtime in the main test code. 
C In order for the answer produced by this routine to match the
C answer generated by the MPI_Reduce() operation in the test code, the
C send buffer of process with rank "i" must have been initialized with "i". 
C 
C This routine applies the operation to both integers and to doubles, 
C in case the double and float buffers are initialized differently than the 
C integer buffers.
C ************************************************************************
      INCLUDE 'mpitestf.h'
      INCLUDE 'externalf.h'

      INTEGER OP_INDEX
      INTEGER NUMP, TYPE
      MPITEST_BUF_TYPE ANSWER

      INTEGER I1, I2
      REAL    R1, R2
      DOUBLE PRECISION D1, D2
      COMPLEX C1, C2
      LOGICAL L1, L2

      EXTERNAL APPLY_INT_OP
      EXTERNAL APPLY_REAL_OP
      EXTERNAL APPLY_DOUBLE_OP
      EXTERNAL APPLY_LOGICAL_OP
      EXTERNAL APPLY_COMPLEX_OP

      IF (NUMP .EQ. 1) THEN
        CALL MPITEST_DATATEMPLATE_INIT(ANSWER, 1, TYPE)
        RETURN
      END IF

      I1 = 1
      I2 = 2

      R1 = 1.0
      R2 = 2.0

      D1 = 1.0
      D2 = 2.0

      CALL MPITEST_INT_TO_LOGICAL(1, L1)
      CALL MPITEST_INT_TO_LOGICAL(2, L2)

      C1 = CMPLX(1)
      C2 = CMPLX(2)

      IF (TYPE .EQ. MPITEST_INTEGER) THEN
         CALL APPLY_INT_OP(OP_INDEX, I1, I2, ANSWER)
      ELSE IF (TYPE .EQ. MPITEST_REAL) THEN
         CALL APPLY_REAL_OP(OP_INDEX, R1, R2, ANSWER)
      ELSE IF (TYPE .EQ. MPITEST_DOUBLE_PRECISION) THEN
         CALL APPLY_DOUBLE_OP(OP_INDEX, D1, D2, ANSWER)
      ELSE IF (TYPE .EQ. MPITEST_LOGICAL) THEN
         CALL APPLY_LOGICAL_OP(OP_INDEX, L1, L2, ANSWER)
      ELSE IF (TYPE .EQ. MPITEST_COMPLEX) THEN
         CALL APPLY_COMPLEX_OP(OP_INDEX, C1, C2, ANSWER)
      END IF

      IF (TYPE .EQ. MPITEST_INTEGER) THEN
         I1 = 3
         DO 100 I2=3, NUMP
           IF (I2 .GT. NUMP) GO TO 101
           IF ((I1 .GT. 2) .AND. (OP_INDEX .EQ. 4)) I1=1
           CALL APPLY_INT_OP(OP_INDEX, ANSWER, I1, ANSWER)
           I1 = I1 + 1
 100     CONTINUE
 101     CONTINUE
      ELSE IF (TYPE .EQ. MPITEST_REAL) THEN
         R1 = 3
C        WDG - Use integer for loop index
         DO 200 I2=3, NUMP
           R2 = I2
           IF (I2 .GT. NUMP) GO TO 201
           IF ((R1 .GT. 2) .AND. (OP_INDEX .EQ. 4)) R1=1
           CALL APPLY_REAL_OP(OP_INDEX, ANSWER, R1, ANSWER)
           R1 = R1 + 1
 200     CONTINUE
 201     CONTINUE
      ELSE IF (TYPE .EQ. MPITEST_COMPLEX) THEN
         I1 = 3
         C1 = CMPLX(I1)
         DO 300 I2=3, NUMP
           IF (I2 .GT. NUMP) GO TO 301
           IF ((I1 .GT. 2) .AND. (OP_INDEX .EQ. 4)) THEN
             C1=1
             I1=1
           END IF
           CALL APPLY_COMPLEX_OP(OP_INDEX, ANSWER, C1, ANSWER)
           I1 = I1 + 1
           C1 = CMPLX(I1)
 300     CONTINUE
 301     CONTINUE
      ELSE IF (TYPE .EQ. MPITEST_DOUBLE_PRECISION) THEN
         D1 = 3
C        WDG - Use integer for loop index
         DO 400 I2=3, NUMP
           D2 = I2
           IF (I2 .GT. NUMP) GO TO 401
           IF ((D1 .GT. 2) .AND. (OP_INDEX .EQ. 4)) D1=1
           CALL APPLY_DOUBLE_OP(OP_INDEX, ANSWER, D1, ANSWER)
           D1 = D1 + 1
 400     CONTINUE
 401     CONTINUE
      ELSE IF (TYPE .EQ. MPITEST_LOGICAL) THEN
         I1 = 3
         DO 500 I2=3, NUMP
           CALL MPITEST_INT_TO_LOGICAL(I1, L1)
           IF (I2 .GT. NUMP) GO TO 501
           CALL APPLY_LOGICAL_OP(OP_INDEX, ANSWER, L1, ANSWER)
           I1 = I1 + 1
 500     CONTINUE
 501     CONTINUE
      END IF

C      IF ((OP_INDEX .GE. 5) .AND. (OP_INDEX .LE. 7)) THEN
C        IANSWER = AND(IANSWER, 1)
C      END IF

C
C     Now take care of the real datatypes
C
C      IF (OP_INDEX .LE. 4) THEN
C        CALL APPLY_FP_OP(OP_INDEX, DX1, DX2, DANSWER)
C        DX2 = 3.0
C        DO 200 X2=3, NUMP
C          IF (X2 .GT. NUMP) GO TO 201
C          IF ((DX2 .GT. 2.0) .AND. (OP_INDEX .EQ. 4)) DX2=1.0
C          CALL APPLY_FP_OP(OP_INDEX, DANSWER, DX2, DANSWER)
C          DX2 = DX2 + 1.0
C 200    CONTINUE
C 201    CONTINUE
C
C-      answer->Float = (float) danswer
C-      answer->Double = danswer
C
C      END IF
 
  
      END
