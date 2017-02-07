
      INTEGER FUNCTION test_do_firstprivate()
        IMPLICIT NONE
        INTEGER sum, known_sum
        INTEGER numthreads
        INTEGER omp_get_num_threads

        INTEGER sum0, sum1, i
        COMMON /orphvars/ sum0, sum1, i

  
        INCLUDE "omp_testsuite.f"
  
        sum = 0
        sum0 = 12345
        sum1 = 0
  
  
!$omp parallel firstprivate(sum1)
!$omp single
        numthreads = omp_get_num_threads()
!$omp end single



!$omp do firstprivate(sum0)
        DO i=1,LOOPCOUNT
          sum0 = sum0 + i
          sum1 = sum0
        END DO
!$omp end do



!$omp critical
        WRITE (1,*) sum0
        sum = sum + sum1
!$omp end critical
!$omp end parallel


        known_sum=12345*numthreads+ (LOOPCOUNT*(LOOPCOUNT+1))/2
        IF ( known_sum .EQ. sum ) THEN
          test_do_firstprivate = 1
        ELSE
          WRITE (1,*) "Found sum was", sum, "instead of", known_sum
          test_do_firstprivate = 0
        END IF
      END FUNCTION
!This is the main driver to invoke different test functions
      SUBROUTINE mpc_user_main
      IMPLICIT NONE
      INTEGER failed, success !Number of failed/succeeded tests
      INTEGER num_tests,crosschecked, crossfailed, j
      INTEGER temp,temp1
      INCLUDE "omp_testsuite.f"

      INTEGER test_do_firstprivate


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
      WRITE (*,*) "Testing omp do firstprivate"
      WRITE (*,*) "--------------------------------------------------"
      WRITE (*,*) 
      WRITE (*,*) "testname: test_do_firstprivate"
      WRITE (*,*) "(Crosstests should fail)"
      WRITE (*,*)
      
      DO j = 1, N
        temp =  test_do_firstprivate()
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
