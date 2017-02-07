
!********************************************************************
! Function: omp_workshare
! 
! by Chunhua Liao, University of Houston
! Oct. 2005 - First version
! 
! The idea for the test is that if WORKSHARE is not present,
! the array assignment in PARALLEL region will be executed by each 
! thread and then wrongfully repeated several times.
!
! TODO:Do we need test for WHERE and FORALL?
! A simple test for WHERE and FORALL is added by Zhenying Liu
!********************************************************************
        INTEGER FUNCTION test_omp_workshare()
        IMPLICIT NONE
        INTEGER result,i
        INTEGER scalar02,scalar12,scalar22,scalar32,count
        REAL, DIMENSION(1000)::FF

        INTEGER scalar0,scalar1,scalar2,scalar3
        INTEGER, DIMENSION(1000)::AA,BB,CC
        REAL, DIMENSION(1000)::DD
        COMMON /orphvars/ scalar0,scalar1,scalar2,scalar3,
     &      AA,BB,CC,DD


        result=0
        scalar0=0
        scalar02=0
        scalar1=0
        scalar12=0
        scalar2=0
        scalar22=0
        scalar3=0
        scalar32=0
 
        count = 0

        AA=0
        BB=0

        do i=1,1000
          CC(i) = i
          FF(i) = 1.0/i
        end do

!$OMP PARALLEL

!$OMP   WORKSHARE

! test if work is divided or not for array assignment
        AA=AA+1

! test if scalar assignment is treated as a single unit of work
        scalar0=scalar0+1 

! test if atomic is treated as a single unit of work
!$OMP ATOMIC
        scalar1=scalar1+1 
! test if critical is treated as a single unit of work
!$OMP CRITICAL
        scalar2=scalar2+1
!$OMP END CRITICAL

! test if PARALLEL is treated as a single unit of work
!$OMP PARALLEL
        scalar3=scalar3+1
!$OMP END PARALLEL

        WHERE ( CC .ne. 0 ) DD = 1.0/CC

        FORALL (I=1:1000) CC(i) = -i

!$OMP   END WORKSHARE

!$OMP END PARALLEL

!sequential equivalent statements for comparison 
       BB=BB+1
       scalar02=scalar02+1
       scalar12=scalar12+1
       scalar22=scalar22+1
       scalar32=scalar32+1

!      write (1,*) "ck:sum of AA is",SUM(AA)," sum of BB is ",sum(BB)
       if (SUM(AA)/=SUM(BB)) then
            write(1,*) "Array assignment has some problem"
            result=result +1
       endif
       if (scalar0/=scalar02) then
          write(1,*) "Scalar assignment has some problem"
          result = result +1
       endif
       if (scalar1/=scalar12) then
          write(1,*) "Atomic inside WORKSHARE has some problem"
         result = result +1
       endif
       if (scalar2/=scalar22) then
          write(1,*) "CRITICAL inside WORKSHARE has some problem"
         result = result +1
       endif
       if (scalar3/=scalar32) then
           write(1,*) "PARALLEL inside WORKSHARE has some problem"
           result = result +1
       endif
       do i=1,1000
         if ( abs( DD(i)- FF(i)) .gt. 1.0E-4 ) then
	    count = count + 1
         end if
       end do
       if ( count .ne. 0 ) then
           result = result + 1
           write(1,*) "WHERE has some problem"
       end if

       count = 0
       do i=1,1000
         if ( CC(i) .ne. -i ) then
            count = count + 1
         end if
       end do
       if ( count .ne. 0 ) then
           result = result + 1
           write(1,*) "FORALL has some problem"
       end if


!if anything is wrong, set return value to 0
       if (result==0) then
          test_omp_workshare = 1
       else
          test_omp_workshare = 0
       end if
       end
!This is the main driver to invoke different test functions
      SUBROUTINE mpc_user_main
      IMPLICIT NONE
      INTEGER failed, success !Number of failed/succeeded tests
      INTEGER num_tests,crosschecked, crossfailed, j
      INTEGER temp,temp1
      INCLUDE "omp_testsuite.f"

      INTEGER test_omp_workshare


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
      WRITE (*,*) "Testing omp workshare"
      WRITE (*,*) "--------------------------------------------------"
      WRITE (*,*) 
      WRITE (*,*) "testname: test_omp_workshare"
      WRITE (*,*) "(Crosstests should fail)"
      WRITE (*,*)
      
      DO j = 1, N
        temp =  test_omp_workshare()
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
