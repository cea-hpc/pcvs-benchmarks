
      INTEGER FUNCTION test_single_copyprivate()
        IMPLICIT NONE
        INTEGER omp_get_thread_num
        INCLUDE "omp_testsuite.f"

        INTEGER i,j,thread,nr_iterations,result
        COMMON /orphvars/ nr_iterations,result


        result=0
        nr_iterations=0

!$omp parallel private(i,j,thread)
        
        DO i=0,LOOPCOUNT-1
          thread=OMP_GET_THREAD_NUM()
!$omp single 
          nr_iterations=nr_iterations+1
          j=i
!$omp end single copyprivate(j)
!$omp critical
          result=result+j-i;
!$omp end critical
        END DO
        
!$omp end parallel
        IF(result .EQ. 0 .AND. 
     &     nr_iterations .EQ. LOOPCOUNT) THEN
          test_single_copyprivate=1
        ELSE
          test_single_copyprivate=0
        END IF
      END FUNCTION
!This is the main driver to invoke different test functions
      SUBROUTINE mpc_user_main
      IMPLICIT NONE
      INTEGER failed, success !Number of failed/succeeded tests
      INTEGER num_tests,crosschecked, crossfailed, j
      INTEGER temp,temp1
      INCLUDE "omp_testsuite.f"

      INTEGER test_single_copyprivate


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
      WRITE (*,*) "Testing omp single copyprivate"
      WRITE (*,*) "--------------------------------------------------"
      WRITE (*,*) 
      WRITE (*,*) "testname: test_single_copyprivate"
      WRITE (*,*) "(Crosstests should fail)"
      WRITE (*,*)
      
      DO j = 1, N
        temp =  test_single_copyprivate()
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
