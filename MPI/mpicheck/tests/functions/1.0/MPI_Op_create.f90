
        
subroutine var_0(val, ierr)
    integer val, ierr
end subroutine
        program main
        use mpi
        
        EXTERNAL var_0
       LOGICAL var_1
       INTEGER var_2
       INTEGER var_3
        call mpi_op_create(var_0, var_1, var_2, var_3)
       call pmpi_op_create(var_0, var_1, var_2, var_3)
        end program main
    