
! Changelog:
      INTEGER FUNCTION test_omp_copyin()
        IMPLICIT NONE
        INTEGER known_sum
		
!        INTEGER, SAVE::sum1 
!        implicitly by omp_threadprivate, see spec25 Chap. 2.8.2
        INTEGER sum1
        COMMON /csum1/ sum1
        INTEGER sum, i, threads
        COMMON /orphvars/ sum, i, threads
!   C. Niethammer 30.11.06: moved threadprivate statement into the orphaned
!      function
!$omp threadprivate(/csum1/)
		

        sum = 0
        sum1 = 7
        threads = 0
		
!$omp parallel copyin(sum1)
!        print *,"sum1",sum1
!$omp do
        DO i=1, 999
          sum1 = sum1 + i
        END DO
!$omp critical
        sum = sum + sum1
        threads = threads + 1
!$omp end critical
!$omp end parallel
		
        known_sum = 999*1000/2 + 7*threads
        IF ( known_sum .EQ. sum ) THEN
           test_omp_copyin = 1
        ELSE
           test_omp_copyin = 0
        END IF
      END
!This is the main driver to invoke different test functions
      SUBROUTINE mpc_user_main
      IMPLICIT NONE
      INTEGER failed, success !Number of failed/succeeded tests
      INTEGER num_tests,crosschecked, crossfailed, j
      INTEGER temp,temp1
      INCLUDE "omp_testsuite.f"

      INTEGER test_omp_copyin


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
      WRITE (*,*) "Testing omp parallel copyin"
      WRITE (*,*) "--------------------------------------------------"
      WRITE (*,*) 
      WRITE (*,*) "testname: test_omp_copyin"
      WRITE (*,*) "(Crosstests should fail)"
      WRITE (*,*)
      
      DO j = 1, N
        temp =  test_omp_copyin()
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
