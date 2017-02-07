

      INTEGER FUNCTION test_do_schedule_static()
        IMPLICIT NONE
        INTEGER omp_get_thread_num,omp_get_num_threads
        CHARACTER*30 logfile
        INTEGER threads
        INTEGER count
        INTEGER ii
        INTEGER result

        INTEGER CFSMAX_SIZE
        PARAMETER (CFSMAX_SIZE = 1000)
        INTEGER i,tids(0:CFSMAX_SIZE-1), tid, chunk_size
        COMMON /orphvars/ i,tid,tids,chunk_size


        chunk_size = 7
        result = 0
        ii = 0

!$omp parallel 
!$omp single
        threads = omp_get_num_threads()
!$omp end single
!$omp end parallel

        IF ( threads .LT. 2) THEN
          PRINT *,"This test only works with at least two threads"
          WRITE(1,*) "This test only works with at least two threads"
          test_do_schedule_static = 0
          STOP
        ELSE
          WRITE(1,*) "Using an internal count of ",CFSMAX_SIZE
          WRITE(1,*) "Using a specified chunksize of ",chunk_size
    
!$omp parallel private(tid) shared(tids)
          tid = omp_get_thread_num()

!$omp do schedule(static,chunk_size)
          DO i = 0 ,CFSMAX_SIZE -1
            tids(i) = tid
          END DO
!$omp end do

!$omp end parallel

          DO i = 0, CFSMAX_SIZE-1
!... round-robin for static chunk
            ii = mod( i/chunk_size,threads)
            IF (tids(i) .NE. ii ) THEN
              result = result + 1
              WRITE(1,*)"Iteration ",i,"should be assigned to ",
     &           ii,"instead of ",tids(i)
            END IF
          END DO
          IF ( result .EQ. 0 )THEN
            test_do_schedule_static = 1
          ELSE
            test_do_schedule_static = 0
          END IF
        END IF
      END FUNCTION
!This is the main driver to invoke different test functions
      SUBROUTINE mpc_user_main
      IMPLICIT NONE
      INTEGER failed, success !Number of failed/succeeded tests
      INTEGER num_tests,crosschecked, crossfailed, j
      INTEGER temp,temp1
      INCLUDE "omp_testsuite.f"

      INTEGER test_do_schedule_static


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
      WRITE (*,*) "Testing omp do schedule(static)"
      WRITE (*,*) "--------------------------------------------------"
      WRITE (*,*) 
      WRITE (*,*) "testname: test_do_schedule_static"
      WRITE (*,*) "(Crosstests should fail)"
      WRITE (*,*)
      
      DO j = 1, N
        temp =  test_do_schedule_static()
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
