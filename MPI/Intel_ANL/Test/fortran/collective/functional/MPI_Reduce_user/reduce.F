#include "foptions.h"

      INTEGER FUNCTION ADDOP(INVEC, INOUTVEC, LEN, DTYPE)
C *************************************************************************
C   Commutative user Op (addition)
C *************************************************************************
      INCLUDE 'mpitestf.h'
      INCLUDE 'mpif.h'

      INTEGER  INVEC(*)
      INTEGER  INOUTVEC(*)
      INTEGER  LEN
      INTEGER  DTYPE
      INTEGER  I

      EXTERNAL ADDOPR
      EXTERNAL ADDOPD
      EXTERNAL ADDOPC

      IF (LEN .EQ. 0) GO TO 101
      IF (DTYPE .EQ. MPI_INTEGER) THEN
        DO 100 I=1,LEN
          INOUTVEC(I) = INVEC(I) + INOUTVEC(I)
 100    CONTINUE
      ELSE IF (DTYPE .EQ. MPI_REAL) THEN
        CALL ADDOPR(INVEC, INOUTVEC, LEN)
      ELSE IF (DTYPE .EQ. MPI_DOUBLE_PRECISION) THEN
        CALL ADDOPD(INVEC, INOUTVEC, LEN)
      ELSE IF (DTYPE .EQ. MPI_COMPLEX) THEN
        CALL ADDOPC(INVEC, INOUTVEC, LEN)
      END IF

 101  CONTINUE
      ADDOP = 0

      END


      SUBROUTINE ADDOPR(INVEC, INOUTVEC, LEN)
C *************************************************************************
C   Commutative user Op (addition) for REAL
C *************************************************************************

      REAL     INVEC(*)
      REAL     INOUTVEC(*)
      INTEGER  LEN
      INTEGER  I

      DO 100 I=1,LEN
        INOUTVEC(I) = INVEC(I) + INOUTVEC(I)
 100  CONTINUE

      END


      SUBROUTINE ADDOPD(INVEC, INOUTVEC, LEN)
C *************************************************************************
C   Commutative user Op (addition) for DOUBLE PRECISION
C *************************************************************************

      DOUBLE PRECISION  INVEC(*)
      DOUBLE PRECISION  INOUTVEC(*)
      INTEGER  LEN
      INTEGER  I

      DO 100 I=1,LEN
        INOUTVEC(I) = INVEC(I) + INOUTVEC(I)
 100  CONTINUE

      END

      SUBROUTINE ADDOPC(INVEC, INOUTVEC, LEN)
C *************************************************************************
C   Commutative user Op (addition) for COMPLEX
C *************************************************************************

      COMPLEX  INVEC(*)
      COMPLEX  INOUTVEC(*)
      INTEGER  LEN
      INTEGER  I

      DO 100 I=1,LEN
        INOUTVEC(I) = INVEC(I) + INOUTVEC(I)
 100  CONTINUE

      END


      INTEGER FUNCTION INCOP(INVEC, INOUTVEC, LEN, DTYPE)
C *************************************************************************
C Non-commutative user Op.
C *************************************************************************
      INCLUDE 'mpitestf.h'
      INCLUDE 'mpif.h'

      INTEGER  INVEC(*)
      INTEGER  INOUTVEC(*)
      INTEGER  LEN
      INTEGER  DTYPE
      INTEGER  I

      EXTERNAL INCOPR
      EXTERNAL INCOPD
      EXTERNAL INCOPC

      IF (LEN .EQ. 0) GO TO 101
      IF (DTYPE .EQ. MPI_INTEGER) THEN
        DO 100 I=1,LEN
          IF (INOUTVEC(I) .GT. INVEC(I)) THEN
            INOUTVEC(I) = INVEC(I)
          ELSE
            INOUTVEC(I) = MPITEST_NUMP + 2
          END IF
 100    CONTINUE
      ELSE IF (DTYPE .EQ. MPI_REAL) THEN
        CALL INCOPR(INVEC, INOUTVEC, LEN)
      ELSE IF (DTYPE .EQ. MPI_DOUBLE_PRECISION) THEN
        CALL INCOPD(INVEC, INOUTVEC, LEN)
      ELSE IF (DTYPE .EQ. MPI_COMPLEX) THEN
        CALL INCOPC(INVEC, INOUTVEC, LEN)
      END IF

 101  CONTINUE
      INCOP = 0
      END


      SUBROUTINE INCOPR(INVEC, INOUTVEC, LEN)
C *************************************************************************
C Non-commutative user Op for REAL.
C *************************************************************************
      INCLUDE 'mpitestf.h'

      REAL     INVEC(*)
      REAL     INOUTVEC(*)
      INTEGER  LEN
      INTEGER  DTYPE
      INTEGER  I

      DO 100 I=1,LEN
        IF (INOUTVEC(I) .GT. INVEC(I)) THEN
          INOUTVEC(I) = INVEC(I)
        ELSE
          INOUTVEC(I) = MPITEST_NUMP + 2
        END IF
 100  CONTINUE

      END

      SUBROUTINE INCOPD(INVEC, INOUTVEC, LEN)
C *************************************************************************
C Non-commutative user Op for DOUBLE PRECISION.
C *************************************************************************
      INCLUDE 'mpitestf.h'

      DOUBLE PRECISION  INVEC(*)
      DOUBLE PRECISION  INOUTVEC(*)
      INTEGER  LEN
      INTEGER  DTYPE
      INTEGER  I

      DO 100 I=1,LEN
        IF (INOUTVEC(I) .GT. INVEC(I)) THEN
          INOUTVEC(I) = INVEC(I)
        ELSE
          INOUTVEC(I) = MPITEST_NUMP + 2
        END IF
 100  CONTINUE

      END

      SUBROUTINE INCOPC(INVEC, INOUTVEC, LEN)
C *************************************************************************
C Non-commutative user Op for COMPLEX.
C *************************************************************************
      INCLUDE 'mpitestf.h'

      COMPLEX  INVEC(*)
      COMPLEX  INOUTVEC(*)
      INTEGER  LEN
      INTEGER  DTYPE
      INTEGER  I

      DO 100 I=1,LEN
        IF (REAL(INOUTVEC(I)) .GT. REAL(INVEC(I))) THEN
          INOUTVEC(I) = INVEC(I)
        ELSE
          INOUTVEC(I) = CMPLX(MPITEST_NUMP + 2)
        END IF
 100  CONTINUE

      END

      SUBROUTINE SET_DEFAULT_OPS(OP_ARRAY, NUMBER)
C ************************************************************************
C  Sets up the default operation array.  Returns the number of default 
C  operations.
C ************************************************************************
      INCLUDE 'mpitestf.h'
      INCLUDE 'mpif.h'
      INCLUDE 'externalf.h'

      INTEGER OP_ARRAY(10)
      INTEGER NUMBER

      EXTERNAL ADDOP
      EXTERNAL INCOP

      CHARACTER*(IOSIZE) INFOBUF
      INTEGER IERR
      INTEGER ERRSIZE
      INTEGER ERR

 99   FORMAT(A,INT_FMT)


      CALL MPI_OP_CREATE(ADDOP, .TRUE., OP_ARRAY(1), IERR)
      IF (IERR .NE. MPI_SUCCESS) THEN
        WRITE(INFOBUF,99) 'MPI_OP_CREATE() returned', IERR
        CALL MPITEST_MESSAGE(MPITEST_NONFATAL, INFOBUF)
        INFOBUF=' '
        CALL MPI_ERROR_STRING(IERR, INFOBUF, ERRSIZE, ERR)
        CALL MPITEST_MESSAGE(MPITEST_FATAL, INFOBUF)
      END IF
 
      CALL MPI_OP_CREATE(INCOP, .FALSE., OP_ARRAY(2), IERR)
      IF (IERR .NE. MPI_SUCCESS) THEN
        WRITE(INFOBUF,99) 'MPI_OP_CREATE() returned', IERR
        CALL MPITEST_MESSAGE(MPITEST_NONFATAL, INFOBUF)
        INFOBUF=' '
        CALL MPI_ERROR_STRING(IERR, INFOBUF, ERRSIZE, ERR)
        CALL MPITEST_MESSAGE(MPITEST_FATAL, INFOBUF)
      END IF

      NUMBER = 2

      END


      SUBROUTINE HAS_OP(OP, TEST_TYPE, HASOP)
C ************************************************************************
C Determines whether a particular operation may be applied to a particular 
C data type.
C ************************************************************************
      INCLUDE 'mpitestf.h'
      INCLUDE 'externalf.h'

      INTEGER OP
      INTEGER TEST_TYPE
      LOGICAL HASOP

      IF ((TEST_TYPE .EQ. MPITEST_INTEGER) .OR.
     $    (TEST_TYPE .EQ. MPITEST_REAL) .OR.
     $    (TEST_TYPE .EQ. MPITEST_DOUBLE_PRECISION) .OR.
     $    (TEST_TYPE .EQ. MPITEST_COMPLEX)) THEN
        HASOP = .TRUE.
      ELSE
        HASOP = .FALSE.
      END IF

      END


      SUBROUTINE GET_INT_REDUCE(OP_INDEX, NUMP, ANSWER)
C ************************************************************************
C Get result of integer Reduction
C ************************************************************************
C
      INCLUDE 'mpitestf.h'
      INCLUDE 'mpif.h'
      INCLUDE 'externalf.h'

      INTEGER OP_INDEX, NUMP, IERR
      INTEGER ADDOP, INCOP

      INTEGER I
      INTEGER INVEC(MAX_RANKS), ANSWER, INOUTVEC(MAX_RANKS)

      IF (NUMP .EQ. 1) THEN
         ANSWER = 1

      ELSE IF (NUMP .GT. 1) THEN
         INVEC(1) = 1
         INOUTVEC(1) = 2

         IF (OP_INDEX .EQ. 1) THEN
            IERR = ADDOP(INVEC, INOUTVEC, 1, MPI_INTEGER)
         ELSE IF (OP_INDEX .EQ. 2) THEN
            IERR = INCOP(INVEC, INOUTVEC, 1, MPI_INTEGER)
         END IF

         IF (NUMP .GT. 2) THEN
            DO 100 I = 3, NUMP
C
C              Since the operator may not be commutative
C
               INVEC(1) = INOUTVEC(1)

               INOUTVEC(1) = I

               IF (OP_INDEX .EQ. 1) THEN
                  IERR = ADDOP(INVEC, INOUTVEC, 1, MPI_INTEGER)
               ELSE IF (OP_INDEX .EQ. 2) THEN
                  IERR = INCOP(INVEC, INOUTVEC, 1, MPI_INTEGER)
               END IF

 100        CONTINUE

         END IF

         ANSWER = INOUTVEC(1)

      END IF

      END


      SUBROUTINE GET_REAL_REDUCE(OP_INDEX, NUMP, ANSWER)
C ************************************************************************
C Get result of integer Reduction
C ************************************************************************
C
      INCLUDE 'mpitestf.h'
      INCLUDE 'mpif.h'
      INCLUDE 'externalf.h'

      INTEGER OP_INDEX, NUMP
      INTEGER IERR, ADDOP, INCOP

      INTEGER I
      REAL INVEC(MAX_RANKS), ANSWER, INOUTVEC(MAX_RANKS)

      IF (NUMP .EQ. 1) THEN
         ANSWER = 1

      ELSE IF (NUMP .GT. 1) THEN
         INVEC(1) = 1
         INOUTVEC(1) = 2

         IF (OP_INDEX .EQ. 1) THEN
            IERR = ADDOP(INVEC, INOUTVEC, 1, MPI_REAL)
         ELSE IF (OP_INDEX .EQ. 2) THEN
            IERR = INCOP(INVEC, INOUTVEC, 1, MPI_REAL)
         END IF

         IF (NUMP .GT. 2) THEN
            DO 100 I = 3, NUMP
C
C              Since the operator may not be commutative
C
               INVEC(1) = INOUTVEC(1)

               INOUTVEC(1) = I

               IF (OP_INDEX .EQ. 1) THEN
                  IERR = ADDOP(INVEC, INOUTVEC, 1, MPI_REAL)
               ELSE IF (OP_INDEX .EQ. 2) THEN
                  IERR = INCOP(INVEC, INOUTVEC, 1, MPI_REAL)
               END IF

 100        CONTINUE

         END IF

         ANSWER = INOUTVEC(1)

      END IF

      END


      SUBROUTINE GET_COMPLEX_REDUCE(OP_INDEX, NUMP,
     &                              ANSWER)
C ************************************************************************
C Get result of integer Reduction
C ************************************************************************
C
      INCLUDE 'mpitestf.h'
      INCLUDE 'mpif.h'
      INCLUDE 'externalf.h'

      INTEGER OP_INDEX, NUMP
      INTEGER IERR, ADDOP, INCOP

      INTEGER I
      COMPLEX INVEC(MAX_RANKS), ANSWER, INOUTVEC(MAX_RANKS)

      IF (NUMP .EQ. 1) THEN
         ANSWER = 1

      ELSE IF (NUMP .GT. 1) THEN
         INVEC(1) = 1
         INOUTVEC(1) = 2

         IF (OP_INDEX .EQ. 1) THEN
            IERR = ADDOP(INVEC, INOUTVEC, 1, MPI_COMPLEX)
         ELSE IF (OP_INDEX .EQ. 2) THEN
            IERR = INCOP(INVEC, INOUTVEC, 1, MPI_COMPLEX)
         END IF

         IF (NUMP .GT. 2) THEN
            DO 100 I = 3, NUMP
C
C              Since the operator may not be commutative
C
               INVEC(1) = INOUTVEC(1)

               INOUTVEC(1) = I

               IF (OP_INDEX .EQ. 1) THEN
                  IERR = ADDOP(INVEC, INOUTVEC, 1, MPI_COMPLEX)
               ELSE IF (OP_INDEX .EQ. 2) THEN
                  IERR = INCOP(INVEC, INOUTVEC, 1, MPI_COMPLEX)
               END IF

 100        CONTINUE

         END IF

         ANSWER = INOUTVEC(1)

      END IF

      END


      SUBROUTINE GET_DOUBLE_REDUCE(OP_INDEX, NUMP,
     &                              ANSWER)
C ************************************************************************
C Get result of integer Reduction
C ************************************************************************
C
      INCLUDE 'mpitestf.h'
      INCLUDE 'mpif.h'
      INCLUDE 'externalf.h'

      INTEGER OP_INDEX, NUMP
      INTEGER IERR, ADDOP, INCOP

      INTEGER I
      DOUBLE PRECISION INVEC(MAX_RANKS), ANSWER, INOUTVEC(MAX_RANKS)

      IF (NUMP .EQ. 1) THEN
         ANSWER = 1

      ELSE IF (NUMP .GT. 1) THEN
         INVEC(1) = 1
         INOUTVEC(1) = 2

         IF (OP_INDEX .EQ. 1) THEN
            IERR = ADDOP(INVEC, INOUTVEC, 1, MPI_DOUBLE_PRECISION)
         ELSE IF (OP_INDEX .EQ. 2) THEN
            IERR = INCOP(INVEC, INOUTVEC, 1, MPI_DOUBLE_PRECISION)
         END IF

         IF (NUMP .GT. 2) THEN
            DO 100 I = 3, NUMP
C
C              Since the operator may not be commutative
C
               INVEC(1) = INOUTVEC(1)

               INOUTVEC(1) = I

               IF (OP_INDEX .EQ. 1) THEN
                  IERR = ADDOP(INVEC, INOUTVEC, 1, MPI_DOUBLE_PRECISION)
               ELSE IF (OP_INDEX .EQ. 2) THEN
                  IERR = INCOP(INVEC, INOUTVEC, 1, MPI_DOUBLE_PRECISION)
               END IF

 100        CONTINUE

         END IF

         ANSWER = INOUTVEC(1)

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
C
      INCLUDE 'mpitestf.h'
      INCLUDE 'externalf.h'

      INTEGER OP_INDEX
      INTEGER NUMP, TYPE
      MPITEST_BUF_TYPE ANSWER

      IF (TYPE .EQ. MPITEST_INTEGER) THEN
         CALL GET_INT_REDUCE(OP_INDEX, NUMP, ANSWER)
      ELSE IF (TYPE .EQ. MPITEST_REAL) THEN
         CALL GET_REAL_REDUCE(OP_INDEX, NUMP, ANSWER)
      ELSE IF (TYPE .EQ. MPITEST_DOUBLE_PRECISION) THEN
         CALL GET_DOUBLE_REDUCE(OP_INDEX, NUMP, ANSWER)
      ELSE IF (TYPE .EQ. MPITEST_COMPLEX) THEN
         CALL GET_COMPLEX_REDUCE(OP_INDEX, NUMP, ANSWER)
      END IF

      END

