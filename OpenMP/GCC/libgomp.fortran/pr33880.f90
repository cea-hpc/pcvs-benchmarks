! PR middle-end/33880
! { dg-do run }

subroutine mpc_user_main
  integer :: i, j
  call something ()
  !$omp parallel do
  do i = 1, 1000
    !$omp atomic
      j = j + 1
  end do
  if (j .ne. 1000) call abort
contains
  subroutine something()
    i = 0
    j = 0
  end subroutine something
end subroutine                    
