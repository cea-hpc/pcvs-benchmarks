
!Yi Wen modified this function from his own understanding of the semantics
!of C version at 05042004
!The undeestanding is that sum0 and myvalue can be local static variables
!of the chk_omp_threadprivate function. There is no need to use common
!block
      INTEGER FUNCTION test_omp_threadprivate()
        IMPLICIT NONE
        INTEGER sum, known_sum, i , iter, rank,size, failed
        INTEGER omp_get_num_threads, omp_get_thread_num
        REAL my_random
        REAL, ALLOCATABLE:: data(:)
        INTEGER random_size
        INTRINSIC random_number
        INTRINSIC random_seed
        EXTERNAL omp_set_dynamic

!Yi Wen modified at 05042004 : add "save"
        INTEGER, SAVE:: sum0
        REAL, SAVE::myvalue
!Yi Wen commented two common blocks
!	common/csum0/ sum0
!	common/cmyvalue/ myvalue
!!!!!!!!!!$omp threadprivate(/csum0/,/cmyvalue/)
		
!$omp threadprivate(sum0,myvalue)
		
        INCLUDE "omp_testsuite.f"

        sum = 0
        failed = 0
        sum0=0
        myvalue=0
        random_size=45
        CALL omp_set_dynamic(.FALSE.)
!$omp parallel
        sum0 = 0
!$omp do
        DO i=1, LOOPCOUNT
          sum0 = sum0 + i
        END DO
!$omp end do
!$omp critical
        sum = sum + sum0
!$omp end critical
!$omp end parallel
        known_sum = (LOOPCOUNT*(LOOPCOUNT+1))/2
        IF ( known_sum .NE. sum ) THEN
          PRINT *, ' known_sum =', known_sum, ', sum =',sum
        END IF

        CALL omp_set_dynamic(.FALSE.)

!$omp parallel
!$omp master
        size = omp_get_num_threads()
        ALLOCATE ( data(size) )
!$omp end master
!$omp end parallel
        CALL RANDOM_SEED(SIZE=random_size)
        DO iter = 0, 99
          CALL RANDOM_NUMBER(HARVEST=my_random)
!$omp parallel private(rank)
          rank = omp_get_thread_num()+1
          myvalue = my_random + rank
          data(rank) = myvalue
!$omp end parallel
!$omp parallel private(rank)
          rank = omp_get_thread_num()+1
          IF ( myvalue .NE. data(rank) ) THEN
            failed = failed + 1
            PRINT *, ' myvalue =',myvalue,' data(rank)=', data(rank)
          END IF
!$omp end parallel
        END DO
        DEALLOCATE( data)
        IF ( (known_sum .EQ. sum) .AND. (failed .NE. 1) ) THEN
          test_omp_threadprivate = 1
        else
          test_omp_threadprivate = 0 
        end if
      END
!This is the main driver to invoke different test functions
      SUBROUTINE mpc_user_main
      IMPLICIT NONE
      INTEGER failed, success !Number of failed/succeeded tests
      INTEGER num_tests,crosschecked, crossfailed, j
      INTEGER temp,temp1
      INCLUDE "omp_testsuite.f"

      INTEGER test_omp_threadprivate


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
      WRITE (*,*) "Testing omp threadprivate"
      WRITE (*,*) "--------------------------------------------------"
      WRITE (*,*) 
      WRITE (*,*) "testname: test_omp_threadprivate"
      WRITE (*,*) "(Crosstests should fail)"
      WRITE (*,*)
      
      DO j = 1, N
        temp =  test_omp_threadprivate()
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
