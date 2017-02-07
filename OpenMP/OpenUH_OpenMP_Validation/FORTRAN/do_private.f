
      SUBROUTINE do_some_work()
        IMPLICIT NONE
        INTEGER i
        INTRINSIC sqrt
        DOUBLE PRECISION sum

        INCLUDE "omp_testsuite.f"
        sum=0.0
        DO i=0, LOOPCOUNT-1
          sum = sum + sqrt(REAL(i))
        ENDDO

      END

      INTEGER FUNCTION test_do_private()
        IMPLICIT NONE
        INTEGER sum, known_sum

        INTEGER sum0, sum1, i
        COMMON /orphvars/ sum0, sum1, i
        

        INCLUDE "omp_testsuite.f"

        sum  = 0
        sum0 = 0
        sum1 = 0

!$omp parallel private(sum1)
        sum0 = 0
        sum1 = 0


!$omp do private(sum0) schedule(static,1)
        DO i=1, LOOPCOUNT
          sum0 = sum1
!$omp flush
          sum0 = sum0 + i
          CALL do_some_work()
!$omp flush
!          print *, sum0
          sum1 = sum0
        END DO
!$omp end do


!$omp critical
        sum = sum + sum1
!$omp end critical
!$omp end parallel

        known_sum = (LOOPCOUNT*(LOOPCOUNT+1))/2
!        print *, "sum:", sum, "known_sum", known_sum
        IF ( known_sum .EQ. sum) THEN
          test_do_private = 1
        ELSE
          test_do_private = 0
        END IF
      END FUNCTION
!This is the main driver to invoke different test functions
      SUBROUTINE mpc_user_main
      IMPLICIT NONE
      INTEGER failed, success !Number of failed/succeeded tests
      INTEGER num_tests,crosschecked, crossfailed, j
      INTEGER temp,temp1
      INCLUDE "omp_testsuite.f"

      INTEGER test_do_private


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
      WRITE (*,*) "Testing omp do private"
      WRITE (*,*) "--------------------------------------------------"
      WRITE (*,*) 
      WRITE (*,*) "testname: test_do_private"
      WRITE (*,*) "(Crosstests should fail)"
      WRITE (*,*)
      
      DO j = 1, N
        temp =  test_do_private()
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
