
      INCLUDE "omp_my_sleep.f"

      INTEGER FUNCTION test_omp_taskwait()
        IMPLICIT NONE
        INCLUDE "omp_testsuite.f"
        INTEGER result1, result2
        INTEGER array(NUM_TASKS)
        INTEGER i, myi
        
        external my_sleep
        

        result1 = 0
        result2 = 0

        ! fill array
        do i = 1, NUM_TASKS
          array(i) = 0
        end do

!$omp parallel shared(i) private(myi)
!$omp single
        do i=1, NUM_TASKS
         ! First we have to store the value of the loop index in a new variable
         ! which will be private for each task because otherwise it will be
         ! overwritten if the execution of the task takes longer than the time
         ! which is needed to enter the next step of the loop!

         myi = i

!$omp task
          call my_sleep(SLEEPTIME)
          array(myi) = 1
!$omp end task
        end do

        
        
!$omp taskwait
        
        

        ! check if all tasks were finished
        do i=1, NUM_TASKS
          if (array(i) .ne. 1) then
              result1 = result1 + 1
          end if
        end do

        ! generate some more tasks which now shall overwrite the valuesin the
        ! array
        do i=1, NUM_TASKS
          myi = i
!$omp task
          array(myi) = 2
!$omp end task
        end do

!$omp end single
!$omp end parallel

        ! final check, if all array elements contain the right values
        do i=1, NUM_TASKS
          if (array(i) .ne. 2) then
            result2 = result2 + 1
          end if
        end do

        if ( (result1 .eq. 0) .and. (result2 .eq. 0) ) then
            test_omp_taskwait = 1
        else
            test_omp_taskwait = 0
        end if

      END FUNCTION
!This is the main driver to invoke different test functions
      SUBROUTINE mpc_user_main
      IMPLICIT NONE
      INTEGER failed, success !Number of failed/succeeded tests
      INTEGER num_tests,crosschecked, crossfailed, j
      INTEGER temp,temp1
      INCLUDE "omp_testsuite.f"

      INTEGER test_omp_taskwait


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
      WRITE (*,*) "Testing omp taskwait"
      WRITE (*,*) "--------------------------------------------------"
      WRITE (*,*) 
      WRITE (*,*) "testname: test_omp_taskwait"
      WRITE (*,*) "(Crosstests should fail)"
      WRITE (*,*)
      
      DO j = 1, N
        temp =  test_omp_taskwait()
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
