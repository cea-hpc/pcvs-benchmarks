
      INTEGER FUNCTION test_omp_num_threads()
        IMPLICIT NONE
        INTEGER i, max_threads
        INTEGER omp_get_num_threads

        INTEGER failed,threads,nthreads,tmp
        COMMON /orphvars/ failed,threads,nthreads


        failed = 0
        max_threads = 0
         
!$omp parallel
!$omp master
        max_threads = OMP_GET_NUM_THREADS()       
!$omp end master
!$omp end parallel
!         print *, "max threads:",max_threads

!Yi Wen added omp_Set_dynamics here to make sure num_threads clause work
!Thanks to Dr. Yin Ma in Absoft. should be not be called before the test loop
!becuase it allows the dynamic adjustment of the number of threads at runtime
!instead of using the max_threads set. 

        !CALL OMP_SET_DYNAMIC(.TRUE.)
        DO threads = 1, max_threads
          nthreads = 0
           
!$omp parallel num_threads(threads) reduction(+:failed)
!          print *, threads, omp_get_num_threads()
          tmp = omp_get_num_threads()
          IF ( threads .NE. tmp ) THEN
            failed = failed + 1
            WRITE (1,*) "Error: found ", tmp, " instead of ",
     &          threads, " threads"
          END IF
!$omp atomic
          nthreads = nthreads + 1
!$omp end parallel
          
!            print *, threads, nthreads
          IF ( nthreads .NE. threads ) THEN
          
            failed = failed + 1
          END IF
        END DO

        IF(failed .NE. 0) THEN
          test_omp_num_threads = 0
        ELSE
          test_omp_num_threads = 1
        END IF
      END FUNCTION
!This is the main driver to invoke different test functions
      SUBROUTINE mpc_user_main
      IMPLICIT NONE
      INTEGER failed, success !Number of failed/succeeded tests
      INTEGER num_tests,crosschecked, crossfailed, j
      INTEGER temp,temp1
      INCLUDE "omp_testsuite.f"

      INTEGER test_omp_num_threads


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
      WRITE (*,*) "Testing omp_get_num_threads"
      WRITE (*,*) "--------------------------------------------------"
      WRITE (*,*) 
      WRITE (*,*) "testname: test_omp_num_threads"
      WRITE (*,*) "(Crosstests should fail)"
      WRITE (*,*)
      
      DO j = 1, N
        temp =  test_omp_num_threads()
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
