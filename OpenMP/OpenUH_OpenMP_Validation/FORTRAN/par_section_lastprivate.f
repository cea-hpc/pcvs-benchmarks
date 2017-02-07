
      INTEGER FUNCTION test_par_section_lastprivate()
        IMPLICIT NONE
        INTEGER sum, sum0, known_sum, i ,i0
        sum = 0
        sum0 = 0
        i0 = -1
!$omp parallel sections lastprivate(i0) private(i,sum0)
!$omp section
        sum0 = 0
        DO i=1, 399
          sum0 = sum0 + i
          i0=i
        END DO
!$omp critical
        sum = sum + sum0
!$omp end critical
!$omp section
        sum0 = 0
        DO i=400, 699
          sum0 = sum0 + i
          i0 = i
        END DO
!$omp critical
        sum = sum + sum0
!$omp end critical
!$omp section
        sum0 = 0
        DO i=700, 999
          sum0 = sum0 + i
          i0 = i
        END DO
!$omp critical
        sum = sum + sum0
!$omp end critical
!$omp end parallel sections
        known_sum = (999*1000)/2
!        print *, "sum", sum, "ks", known_sum, i0
        IF ( known_sum .EQ. sum .AND. i0 .EQ. 999 ) THEN
          test_par_section_lastprivate = 1
        ELSE
          test_par_section_lastprivate = 0
        END IF
      END
!This is the main driver to invoke different test functions
      SUBROUTINE mpc_user_main
      IMPLICIT NONE
      INTEGER failed, success !Number of failed/succeeded tests
      INTEGER num_tests,crosschecked, crossfailed, j
      INTEGER temp,temp1
      INCLUDE "omp_testsuite.f"

      INTEGER test_par_section_lastprivate


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
      WRITE (*,*) "Testing omp parallel sections lastprivate"
      WRITE (*,*) "--------------------------------------------------"
      WRITE (*,*) 
      WRITE (*,*) "testname: test_par_section_lastprivate"
      WRITE (*,*) "(Crosstests should fail)"
      WRITE (*,*)
      
      DO j = 1, N
        temp =  test_par_section_lastprivate()
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
