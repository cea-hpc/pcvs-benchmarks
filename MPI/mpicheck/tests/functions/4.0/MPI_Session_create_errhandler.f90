
        
subroutine var_0(val, ierr)
    integer val, ierr
end subroutine
        program main
        use mpi
        
        EXTERNAL var_0
       INTEGER var_1
       INTEGER var_2
        call mpi_session_create_errhandler(var_0, var_1, var_2)
       call pmpi_session_create_errhandler(var_0, var_1, var_2)
        end program main
    