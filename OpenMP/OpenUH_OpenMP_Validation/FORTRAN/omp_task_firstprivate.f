
      INCLUDE "omp_my_sleep.f"

      INTEGER FUNCTION test_omp_task_firstprivate()
        IMPLICIT NONE
        INCLUDE "omp_testsuite.f"
        INTEGER j,i
        
        external my_sleep
        INTEGER my_sum
        INTEGER known_sum
        INTEGER rslt
        COMMON /orphvars/ my_sum, known_sum, rslt
        

        my_sum = 1234
        known_sum = 1234 + (LOOPCOUNT * (LOOPCOUNT + 1)) / 2

!$omp parallel private(j)
!$omp single
        do i=1, NUM_TASKS
        
!$omp task firstprivate(my_sum)
          do j = 0, LOOPCOUNT
!$omp flush
            my_sum = my_sum + j
          end do

          ! check if calculated my_sum was right
          if (my_sum .ne. known_sum) then
!$omp critical
            rslt = rslt + 1
!$omp end critical
          end if
!$omp end task
        
        end do
!$omp end single
!$omp end parallel

        if (rslt .eq. 0) then
            test_omp_task_firstprivate = 1
        else
            test_omp_task_firstprivate = 0
        end if

      END FUNCTION
!This is the main driver to invoke different test functions
      SUBROUTINE mpc_user_main
      IMPLICIT NONE
      INTEGER failed, success !Number of failed/succeeded tests
      INTEGER num_tests,crosschecked, crossfailed, j
      INTEGER temp,temp1
      INCLUDE "omp_testsuite.f"

      INTEGER test_omp_task_firstprivate


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
      WRITE (*,*) "Testing omp task firstprivate"
      WRITE (*,*) "--------------------------------------------------"
      WRITE (*,*) 
      WRITE (*,*) "testname: test_omp_task_firstprivate"
      WRITE (*,*) "(Crosstests should fail)"
      WRITE (*,*)
      
      DO j = 1, N
        temp =  test_omp_task_firstprivate()
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
