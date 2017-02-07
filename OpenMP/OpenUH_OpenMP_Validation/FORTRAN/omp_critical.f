
      INTEGER FUNCTION test_omp_critical()
        IMPLICIT NONE
        INTEGER known_sum
        
        INTEGER i,j,myi,myj, sum
        COMMON /orphvars/ sum, myi, myj
        
        sum = 0
        myi = 0
        myj = 500
!$omp parallel
!$omp sections

!$omp section
        DO i = 0 , 499
                
                
!$omp critical
                
           sum = sum + myi
           myi = myi + 1
                
!$omp end critical
                
                
        END DO

!$omp section
        DO j = 500 , 999
                
                
!$omp critical
                
           sum = sum + myj
           myj = myj + 1
                
!$omp end critical
                
                
        END DO
!$omp end sections
!$omp end parallel
        known_sum = 999*1000/2
        IF ( known_sum .EQ. sum ) THEN
          test_omp_critical = 1
        ELSE
          WRITE (1,*) "Found sum was", sum, "instead", known_sum
          test_omp_critical = 0
        END IF
      END
!This is the main driver to invoke different test functions
      SUBROUTINE mpc_user_main
      IMPLICIT NONE
      INTEGER failed, success !Number of failed/succeeded tests
      INTEGER num_tests,crosschecked, crossfailed, j
      INTEGER temp,temp1
      INCLUDE "omp_testsuite.f"

      INTEGER test_omp_critical


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
      WRITE (*,*) "Testing omp critical"
      WRITE (*,*) "--------------------------------------------------"
      WRITE (*,*) 
      WRITE (*,*) "testname: test_omp_critical"
      WRITE (*,*) "(Crosstests should fail)"
      WRITE (*,*)
      
      DO j = 1, N
        temp =  test_omp_critical()
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
