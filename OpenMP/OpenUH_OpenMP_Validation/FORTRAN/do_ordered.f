
      INTEGER FUNCTION i_islarger(i)
        IMPLICIT NONE
        INTEGER i, islarger
        INTEGER last_i
        COMMON /mycom/ last_i
        IF ( i .GT. last_i) THEN
          islarger = 1
        ELSE
          islarger = 0
        END If
        last_i = i
        i_islarger = islarger
      END

      INTEGER FUNCTION test_do_ordered()
        IMPLICIT NONE
        INTEGER known_sum, is_larger
        INTEGER last_i
        INTEGER i_islarger
        COMMON /mycom/ last_i




        INTEGER sum, i, my_islarger
        COMMON /orphvars/ my_islarger, sum


        sum = 0
        is_larger = 1
        last_i = 0
!$omp parallel private(my_islarger)
        my_islarger = 1
!$omp do schedule(static,1) ordered
        DO i=1, 99


!$omp ordered

          IF (i_islarger(i) .EQ. 1 .AND. my_islarger .EQ. 1) THEN
            my_islarger = 1
          ELSE
            my_islarger = 0
          END IF
          sum = sum + i

!$omp end ordered


        END DO
!$omp end do
!$omp critical
        IF (is_larger .EQ. 1 .AND. my_islarger .EQ. 1 ) THEN
          is_larger = 1
        ELSE
          is_larger = 0
        END IF
!$omp end critical
!$omp end parallel
        known_sum = (99*100)/2
        IF ( known_sum .EQ. sum .AND. is_larger .EQ. 1) THEN
          test_do_ordered = 1
        ELSE
          test_do_ordered = 0
        END IF
      END
!This is the main driver to invoke different test functions
      SUBROUTINE mpc_user_main
      IMPLICIT NONE
      INTEGER failed, success !Number of failed/succeeded tests
      INTEGER num_tests,crosschecked, crossfailed, j
      INTEGER temp,temp1
      INCLUDE "omp_testsuite.f"

      INTEGER test_do_ordered


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
      WRITE (*,*) "Testing do ordered"
      WRITE (*,*) "--------------------------------------------------"
      WRITE (*,*) 
      WRITE (*,*) "testname: test_do_ordered"
      WRITE (*,*) "(Crosstests should fail)"
      WRITE (*,*)
      
      DO j = 1, N
        temp =  test_do_ordered()
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
