
      LOGICAL FUNCTION check_is_larger(i)
        implicit none
        INTEGER :: i
        INTEGER, save :: last_i
        LOGICAL :: is_larger

        if (i .eq. 1) last_i = 0

        is_larger = (i .ge. last_i) .and. ((i-last_i) .le. 1)
        last_i = i

        check_is_larger = is_larger

      END FUNCTION check_is_larger

      INTEGER FUNCTION test_do_collapse()
        IMPLICIT NONE
        INTEGER i, j

        LOGICAL check_is_larger
        LOGICAL my_is_larger
        LOGICAL is_larger
        COMMON /orphvars/ is_larger


        INCLUDE "omp_testsuite.f"

        is_larger = .true.

!$omp parallel private(my_is_larger)

        my_is_larger = .true.
!$omp do private(i,j) schedule(static,1) collapse(2)
!$omp+   ordered
        DO i=1,100
          
          DO j=1,00
          
!$omp ordered
            my_is_larger = check_is_larger(i) .and. my_is_larger
!$omp end ordered
          
          END DO
        END DO
!$omp end do
!$omp critical
        is_larger = is_larger .and. my_is_larger
!$omp end critical

!$omp end parallel

      if (is_larger) then
        test_do_collapse = 1
      else
        test_do_collapse = 0
      end if
      END FUNCTION
!This is the main driver to invoke different test functions
      SUBROUTINE mpc_user_main
      IMPLICIT NONE
      INTEGER failed, success !Number of failed/succeeded tests
      INTEGER num_tests,crosschecked, crossfailed, j
      INTEGER temp,temp1
      INCLUDE "omp_testsuite.f"

      INTEGER test_do_collapse


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
      WRITE (*,*) "Testing omp do collapse"
      WRITE (*,*) "--------------------------------------------------"
      WRITE (*,*) 
      WRITE (*,*) "testname: test_do_collapse"
      WRITE (*,*) "(Crosstests should fail)"
      WRITE (*,*)
      
      DO j = 1, N
        temp =  test_do_collapse()
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
