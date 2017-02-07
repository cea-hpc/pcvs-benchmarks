

      SUBROUTINE do_some_work3()
        REAL i
        INTRINSIC sqrt
        DOUBLE PRECISION sum
        INCLUDE "omp_testsuite.f"
        sum = 0.0
        DO WHILE (i < LOOPCOUNT-1)
          sum = sum + sqrt(i)
          i = i + 1
        END DO
      END

      INTEGER FUNCTION test_omp_barrier()
        IMPLICIT NONE
        INTEGER sleeptime
        INTEGER omp_get_thread_num
        INTEGER result1, result2, rank
        result1 = 0
        result2 = 0
        sleeptime = 1
!$omp parallel private(rank)
        rank = omp_get_thread_num()
!        PRINT *, "rank", rank
        IF ( rank .EQ. 1 ) THEN
          CALL sleep(sleeptime)
          result2 = 3
        END IF
        
        
!$omp barrier
        
        
        IF ( rank .EQ. 0 ) THEN
          result1 = result2
        END IF
!$omp end parallel
        IF ( result1 .EQ. 3 ) THEN
           test_omp_barrier = 1
        ELSE
           test_omp_barrier = 0
        END IF
      END
!This is the main driver to invoke different test functions
      SUBROUTINE mpc_user_main
      IMPLICIT NONE
      INTEGER failed, success !Number of failed/succeeded tests
      INTEGER num_tests,crosschecked, crossfailed, j
      INTEGER temp,temp1
      INCLUDE "omp_testsuite.f"

      INTEGER test_omp_barrier


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
      WRITE (*,*) "Testing omp barrier"
      WRITE (*,*) "--------------------------------------------------"
      WRITE (*,*) 
      WRITE (*,*) "testname: test_omp_barrier"
      WRITE (*,*) "(Crosstests should fail)"
      WRITE (*,*)
      
      DO j = 1, N
        temp =  test_omp_barrier()
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
