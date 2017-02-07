
      SUBROUTINE do_some_work2()
        IMPLICIT NONE
        REAL i
        DOUBLE PRECISION sum
        INTRINSIC sqrt
        INCLUDE "omp_testsuite.f"
        sum = 0.0
        i = 0
        DO WHILE (i < LOOPCOUNT)
           sum = sum + sqrt(i)
           i = i + 1
        END DO
      END

!********************************************************************

      INTEGER FUNCTION test_par_do_private()
        IMPLICIT NONE
        INTEGER sum,known_sum, i, i2, i3
        INCLUDE "omp_testsuite.f"
        sum = 0

!$omp parallel do reduction(+:sum) private(i2) schedule(static,1)
        DO i=1, LOOPCOUNT
          i2 = i
!$omp flush
          CALL do_some_work2()
!$omp flush
          sum = sum + i2
        END DO
!$omp end parallel do
          known_sum = (LOOPCOUNT*(LOOPCOUNT+1))/2
        IF ( known_sum .EQ. sum ) THEN
          test_par_do_private = 1
        ELSE
          test_par_do_private = 0
        END IF
      END
!This is the main driver to invoke different test functions
      SUBROUTINE mpc_user_main
      IMPLICIT NONE
      INTEGER failed, success !Number of failed/succeeded tests
      INTEGER num_tests,crosschecked, crossfailed, j
      INTEGER temp,temp1
      INCLUDE "omp_testsuite.f"

      INTEGER test_par_do_private


      CHARACTER*50 logfilename !Pointer to logfile
      INTEGER result 

      num_tests = 0
      crosschecked = 0
      crossfailed = 0
      result = 1
      failed = 0

      !Open a new logfile or overwrite the existing one.
!      WRITE (*,*) "Enter logFilename:" 
!      READ  (*,*) logfilename

 
      WRITE (*,*) "######## OpenMP Validation Suite V 3.0a ######"
      WRITE (*,*) "## Repetitions:", N 
      WRITE (*,*) "## Loop Count :", LOOPCOUNT
      WRITE (*,*) "##############################################"
      WRITE (*,*)

      crossfailed=0
      result=1
      WRITE (*,*) "--------------------------------------------------"
      WRITE (*,*) "Testing omp parallel do private"
      WRITE (*,*) "--------------------------------------------------"
      WRITE (*,*) 
      WRITE (*,*) "testname: test_par_do_private"
      WRITE (*,*) "(Crosstests should fail)"
      WRITE (*,*)
      
      DO j = 1, N
        temp =  test_par_do_private()
        IF (temp .EQ. 1) THEN
          WRITE (*,*)  j, ". test successfull."
          success = success + 1
        ELSE
          WRITE (*,*) "Error: ",j, ". test failed."
          failed = failed + 1
        ENDIF
      END DO

      
      IF (failed .EQ. 0) THEN
        WRITE (*,*) "Directive worked without errors."
        result = 0
        WRITE (*,*) "Result:",result
      ELSE
        WRITE (*,*) "Directive failed the test ", failed, " times."
        result = failed * 100 / N
        WRITE (*,*) "Result:",result
      ENDIF
      CALL EXIT (result)
      END SUBROUTINE 
