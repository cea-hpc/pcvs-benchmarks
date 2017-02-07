
      INTEGER FUNCTION test_omp_master_3()
        IMPLICIT NONE
        INTEGER omp_get_thread_num
        
        INTEGER nthreads, executing_thread
        INTEGER tid_result ! counts up the number of wrong thread no.
                           ! for the master thread
        COMMON /orphvars/ nthreads, executing_thread, tid_result
        
        tid_result = 0
        nthreads=0
        executing_thread=-1

!$omp parallel
        
        
!$omp master
        
        if (omp_get_thread_num() .ne. 0) then
!$omp critical
            tid_result = tid_result + 1
!$omp end critical
        end if
!$omp critical
        nthreads = nthreads + 1
!$omp end critical
        executing_thread=omp_get_thread_num()
        
!$omp end master
        
        
!$omp end parallel

        IF ( (nthreads .EQ. 1) .AND. (executing_thread .EQ. 0) .AND.
     &       (tid_result .EQ. 0) ) THEN
          test_omp_master_3 = 1
        ELSE
          test_omp_master_3 = 0
        END IF
      END
!This is the main driver to invoke different test functions
      SUBROUTINE mpc_user_main
      IMPLICIT NONE
      INTEGER failed, success !Number of failed/succeeded tests
      INTEGER num_tests,crosschecked, crossfailed, j
      INTEGER temp,temp1
      INCLUDE "omp_testsuite.f"

      INTEGER test_omp_master_3


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
      WRITE (*,*) "Testing omp master"
      WRITE (*,*) "--------------------------------------------------"
      WRITE (*,*) 
      WRITE (*,*) "testname: test_omp_master_3"
      WRITE (*,*) "(Crosstests should fail)"
      WRITE (*,*)
      
      DO j = 1, N
        temp =  test_omp_master_3()
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
