
      INCLUDE "omp_my_sleep.f"

      INTEGER FUNCTION test_omp_task_untied()
        IMPLICIT NONE
        INCLUDE "omp_testsuite.f"
        
        EXTERNAL my_sleep
        INTEGER omp_get_num_threads, omp_get_thread_num
        INTEGER myj
        INTEGER i,j
        INTEGER cnt
        INTEGER start_tid(NUM_TASKS)
        INTEGER current_tid(NUM_TASKS)
        COMMON /orphvars/ j, cnt, start_tid, current_tid
        

        cnt = 0
        do i = 1, NUM_TASKS
          start_tid(i) = 0
          current_tid(i) = 0
        end do

!$omp parallel private(myj) shared(j)
!$omp single
        do i=1, NUM_TASKS
        j = i
        
        myj = j
!$omp task untied
          call my_sleep(SLEEPTIME)
          start_tid(myj) = omp_get_thread_num()
!$omp taskwait
      if (MOD(start_tid(myj),2) .ne. 0) then
        call my_sleep(SLEEPTIME)
        current_tid(myj) = omp_get_thread_num()
      
       else
        current_tid(myj) = omp_get_thread_num()
       end if
!$omp end task
        
        end do
!$omp end single
!$omp end parallel

        test_omp_task_untied = 0

        ! check if at least one untied task switched threads
        do i=1, NUM_TASKS
          if (current_tid(i) .ne. start_tid(i)) then
               test_omp_task_untied = 1
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

      INTEGER test_omp_task_untied


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
      WRITE (*,*) "Testing omp task untied"
      WRITE (*,*) "--------------------------------------------------"
      WRITE (*,*) 
      WRITE (*,*) "testname: test_omp_task_untied"
      WRITE (*,*) "(Crosstests should fail)"
      WRITE (*,*)
      
      DO j = 1, N
        temp =  test_omp_task_untied()
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
