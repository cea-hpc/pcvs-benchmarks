! { dg-options "-ffixed-form" }
      subroutine mpc_user_main
      if (b().ne.2) call abort
      contains
      subroutine a
!$omp parallel
!$omp	end	parallel
      end subroutine a
      function b()
      integer :: b
	  b = 1
!$	b = 2
      end function b
      end
