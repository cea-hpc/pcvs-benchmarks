
! **********************************************************
! Helper function is_larger
! **********************************************************
      INTEGER FUNCTION i_islarger2(i)
        IMPLICIT NONE
        INTEGER i
        INTEGER last_i,islarger
        COMMON /com/ last_i
        INCLUDE "omp_testsuite.f"
!        print *, "last_i",last_i, "i", i
! last_i is a global variable
        IF ( i .GT. last_i ) THEN
          islarger = 1
        ELSE
          islarger = 0
        END IF
        last_i = i
        i_islarger2 = islarger
      END FUNCTION

      INTEGER FUNCTION test_par_do_ordered()
        IMPLICIT NONE
        COMMON /com/ last_i
        INTEGER known_sum,i, last_i

        INTEGER is_larger,sum,i_islarger2
        COMMON /orphvars/ is_larger,sum,i

        
        sum=0
        is_larger=1
        last_i=0
!$omp parallel do schedule(static, 1) ordered
        DO i=1, 99
                
		
!$omp ordered
		
        IF( i_islarger2(i) .EQ. 1 .AND. is_larger .EQ. 1 ) THEN  
          is_larger = 1
        ELSE
          is_larger = 0
        END IF
        sum = sum + i
		
!$omp end ordered
		
                
        END DO
!$omp end parallel do
        known_sum = (99*100)/2
!Yi Wen; Sun compiler will fail sometimes
!        print *, "sum", sum, "ks", known_sum, "la", is_larger
        IF ( known_sum .EQ. sum .AND. is_larger .EQ. 1 ) THEN
           test_par_do_ordered = 1
        ELSE
           test_par_do_ordered = 0
        END IF
      END FUNCTION
!This is the main driver to invoke different test functions
      SUBROUTINE mpc_user_main
      IMPLICIT NONE
      INTEGER failed, success !Number of failed/succeeded tests
      INTEGER num_tests,crosschecked, crossfailed, j
      INTEGER temp,temp1
      INCLUDE "omp_testsuite.f"

      INTEGER test_par_do_ordered


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
      WRITE (*,*) "Testing omp parallel do ordered"
      WRITE (*,*) "--------------------------------------------------"
      WRITE (*,*) 
      WRITE (*,*) "testname: test_par_do_ordered"
      WRITE (*,*) "(Crosstests should fail)"
      WRITE (*,*)
      
      DO j = 1, N
        temp =  test_par_do_ordered()
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
