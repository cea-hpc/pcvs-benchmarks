


      INTEGER FUNCTION test_do_schedule_dynamic()
        IMPLICIT NONE
        CHARACTER*30 logfile
        INTEGER omp_get_thread_num,omp_get_num_threads
        INTEGER threads
        INTEGER count, tmp_count
        INTEGER,ALLOCATABLE:: tmp(:)
        INTEGER ii
        INTEGER result


        INTEGER CFDMAX_SIZE
        PARAMETER (CFDMAX_SIZE = 1000)
        INTEGER i,tids(0:CFDMAX_SIZE-1),tid,chunk_size
        COMMON /orphvars/ i,tids,tid,chunk_size


        chunk_size = 7
        count = 0
        tmp_count = 0
        result = 0
        ii = 0

!$omp parallel private(tid)
        tid = omp_get_thread_num()

!$omp do schedule(dynamic,chunk_size)
        DO i=0, CFDMAX_SIZE-1
          tids(i) = tid
        END DO
!$omp end do

!$omp end parallel

        DO i=0, CFDMAX_SIZE - 2
          IF ( tids(i) .ne. tids(i+1) ) THEN
            count = count + 1
          END IF
        END DO
 
        ALLOCATE( tmp(0:count) )
        tmp(0) = 1
 
        DO i = 0, CFDMAX_SIZE - 2
          IF ( tmp_count .GT. count ) THEN
            WRITE(*,*) "--------------------"
            WRITE(*,*) "Testinternal Error: List too small!!!"
            WRITE(*,*) "--------------------"
            GOTO 10
          END If
          IF ( tids(i) .NE. tids(i+1) ) then
            tmp_count = tmp_count + 1
            tmp(tmp_count) = 1
          ELSE
            tmp(tmp_count) = tmp(tmp_count) +1
          END IF 
        END DO          

!... is dynamic statement working? 

 10     DO i=0, count -1
          IF ( MOD(tmp(i),chunk_size) .ne. 0 ) THEN
! ... it is possible for 2 adjacent chunks assigned to a same thread 
            result = result + 1
            WRITE(1,*) "The intermediate dispatch has wrong chunksize."
          END IF
        END DO

        IF ( MOD(tmp(count), chunk_size) .NE. 
     &     MOD (CFDMAX_SIZE, chunk_size) ) THEN
          result = result + 1
          WRITE(1,*) "the last dispatch has wrong chunksize."
        END IF
         
        IF ( result .eq. 0) THEN
          test_do_schedule_dynamic = 1
        ELSE
          test_do_schedule_dynamic = 0
        END IF
      END FUNCTION
!This is the main driver to invoke different test functions
      SUBROUTINE mpc_user_main
      IMPLICIT NONE
      INTEGER failed, success !Number of failed/succeeded tests
      INTEGER num_tests,crosschecked, crossfailed, j
      INTEGER temp,temp1
      INCLUDE "omp_testsuite.f"

      INTEGER test_do_schedule_dynamic


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
      WRITE (*,*) "Testing omp do schedule(dynamic)"
      WRITE (*,*) "--------------------------------------------------"
      WRITE (*,*) 
      WRITE (*,*) "testname: test_do_schedule_dynamic"
      WRITE (*,*) "(Crosstests should fail)"
      WRITE (*,*)
      
      DO j = 1, N
        temp =  test_do_schedule_dynamic()
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
