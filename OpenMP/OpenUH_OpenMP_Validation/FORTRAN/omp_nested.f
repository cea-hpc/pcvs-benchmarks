
      INTEGER FUNCTION test_omp_nested()
        IMPLICIT NONE
        INCLUDE "omp_testsuite.f"

        INTEGER counter
        COMMON /orphvars/ counter


        counter =0
        
        
!$      CALL OMP_SET_NESTED(.TRUE.)
!#ifdef _OPENMP
!       CALL OMP_SET_NESTED(.TRUE.) 
!#endif
        
        

!$omp parallel
        
!$omp critical
          counter = counter + 1
!$omp end critical

!$omp parallel
!$omp critical
          counter = counter - 1
!$omp end critical
!$omp end parallel
        
!$omp end parallel
        
        IF (counter .EQ. 0 ) THEN
           WRITE (1,*) "Counter was 0"
           test_omp_nested = 1
        ELSE
           WRITE (1,*) "Counter was", counter
           test_omp_nested = 0
        END IF 
      END FUNCTION
!This is the main driver to invoke different test functions
      SUBROUTINE mpc_user_main
      IMPLICIT NONE
      INTEGER failed, success !Number of failed/succeeded tests
      INTEGER num_tests,crosschecked, crossfailed, j
      INTEGER temp,temp1
      INCLUDE "omp_testsuite.f"

      INTEGER test_omp_nested


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
      WRITE (*,*) "Testing nestedtest"
      WRITE (*,*) "--------------------------------------------------"
      WRITE (*,*) 
      WRITE (*,*) "testname: test_omp_nested"
      WRITE (*,*) "(Crosstests should fail)"
      WRITE (*,*)
      
      DO j = 1, N
        temp =  test_omp_nested()
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
