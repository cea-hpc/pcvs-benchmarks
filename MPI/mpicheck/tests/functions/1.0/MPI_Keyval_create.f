
        
subroutine var_0(val, ierr)
    integer val, ierr
end subroutine

subroutine var_1(val, ierr)
    integer val, ierr
end subroutine
        program main
        include 'mpif.h'
        
        EXTERNAL var_0
       EXTERNAL var_1
       INTEGER var_2
       INTEGER var_3
       INTEGER var_4
        call mpi_keyval_create(var_0, var_1, var_2, var_3, var_4)
       call pmpi_keyval_create(var_0, var_1, var_2, var_3, var_4)
        end program main
    