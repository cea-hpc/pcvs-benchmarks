
      INCLUDE "omp_my_sleep.f"

      INTEGER FUNCTION test_omp_task()
        IMPLICIT NONE
        INCLUDE "omp_testsuite.f"
        
        INTEGER omp_get_num_threads, omp_get_thread_num
        EXTERNAL my_sleep
        INTEGER myj
        INTEGER i,j
        INTEGER tids(NUM_TASKS)
        COMMON /orphvars/ j,tids
        
!$omp parallel private(myj) shared(j)
!$omp single
        do i=1, NUM_TASKS
        j = i
        
        myj = j
        
!$omp task
        
          call my_sleep(SLEEPTIME)
          tids(myj) = omp_get_thread_num()
        
!$omp end task
        
        
        end do
!$omp end single
!$omp end parallel

        test_omp_task = 0

        ! check if more than one thread executed the tasks.
        do i=1, NUM_TASKS
          if (tids(1) .ne. tids(i)) then
               test_omp_task = 1
          end if
        end do

      END FUNCTION
!This is the main driver to invoke different test functions
      SUBROUTINE mpc_user_main
      IMPLICIT NONE
      INTEGER failed, success !Number of failed/succeeded tests
      INTEGER num_tests,crosschecked, crossfailed, j
      INTEGER temp,temp1
      INCLUDE "omp_testsuite.f"

      INTEGER test_omp_task


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
      WRITE (*,*) "Testing omp task"
      WRITE (*,*) "--------------------------------------------------"
      WRITE (*,*) 
      WRITE (*,*) "testname: test_omp_task"
      WRITE (*,*) "(Crosstests should fail)"
      WRITE (*,*)
      
      DO j = 1, N
        temp =  test_omp_task()
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
