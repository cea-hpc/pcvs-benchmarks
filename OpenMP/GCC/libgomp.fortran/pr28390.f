! PR fortran/28390
      subroutine mpc_user_main
      integer i
!$omp parallel do lastprivate(i)
      do i=1,100
      end do
      if (i.ne.101) call abort
      end
