! PR fortran/42866
! { dg-do run }

subroutine mpc_user_main
  integer, allocatable :: a(:)
  allocate (a(16))
  a = 0
  !$omp parallel
    !$omp sections reduction(+:a)
      a = a + 1
    !$omp section
      a = a + 2
    !$omp end sections
  !$omp end parallel
  if (any (a.ne.3)) call abort
  deallocate (a)
end