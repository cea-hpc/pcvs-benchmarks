
        INTEGER FUNCTION test_omp_flush()
        IMPLICIT NONE
        INTEGER result1, dummy, rank
        INTEGER omp_get_thread_num
        
        INTEGER result2
        COMMON /orphvars/ result2
        
        result1=0
        result2=0
!$omp parallel private(rank)
        rank = omp_get_thread_num()
!$omp barrier
        IF ( rank .EQ. 1 ) THEN
          result2 = 3
          
          
!$omp flush(result2)
          
          
          dummy = result2
        END IF
        IF ( rank .EQ. 0 ) THEN
          call sleep(1)
          
          
!$omp flush(result2)
          
          
          result1 = result2
        END IF
!$omp end parallel

!        PRINT *,"1:", result1, "2:", result2, "dummy", dummy
        IF ( (result1 .EQ. result2) .AND. (result2 .EQ. dummy) .AND.
     &       (result2 .EQ. 3) ) THEN
           test_omp_flush = 1
        ELSE
           test_omp_flush = 0
        END IF
      END
!This is the main driver to invoke different test functions
      SUBROUTINE mpc_user_main
      IMPLICIT NONE
      INTEGER failed, success !Number of failed/succeeded tests
      INTEGER num_tests,crosschecked, crossfailed, j
      INTEGER temp,temp1
      INCLUDE "omp_testsuite.f"

      INTEGER test_omp_flush


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
      WRITE (*,*) "Testing omp flush"
      WRITE (*,*) "--------------------------------------------------"
      WRITE (*,*) 
      WRITE (*,*) "testname: test_omp_flush"
      WRITE (*,*) "(Crosstests should fail)"
      WRITE (*,*)
      
      DO j = 1, N
        temp =  test_omp_flush()
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
