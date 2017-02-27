! PR fortran/49792
! { dg-do run }
! { dg-options "-std=f2003 -fall-intrinsics" }

subroutine reverse(n, a)
  integer :: n
  real(kind=8) :: a(n)
!$omp parallel workshare
  a(:) = a(n:1:-1)
!$omp end parallel workshare
end subroutine reverse

subroutine mpc_user_main
  integer :: b(16)
  integer, allocatable :: a(:)
  b = 1
!$omp parallel workshare
  a = b
!$omp end parallel workshare
  if (size(a).ne.size(b)) call abort()
  if (any (a.ne.b)) call abort()
end subroutine
