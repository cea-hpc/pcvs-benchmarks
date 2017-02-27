  ! { dg-options "-fopenmp" }
subroutine mpc_user_main
  logical l
  l = .false.
    !$ include 'condinc1.inc'
  stop 2
end
