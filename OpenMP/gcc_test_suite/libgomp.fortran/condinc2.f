! { dg-options "-fno-openmp" }
      subroutine mpc_user_main
      logical l
      l = .true.
C$    include 'condinc1.inc'
      return
      end
