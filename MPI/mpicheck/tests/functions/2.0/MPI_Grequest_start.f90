
        
subroutine var_0(val, ierr)
    integer val, ierr
end subroutine

subroutine var_1(val, ierr)
    integer val, ierr
end subroutine

subroutine var_2(val, ierr)
    integer val, ierr
end subroutine
        program main
        use mpi
        
        EXTERNAL var_0
       EXTERNAL var_1
       EXTERNAL var_2
       INTEGER(KIND=MPI_ADDRESS_KIND) var_3
       INTEGER var_4
       INTEGER var_5
        call mpi_grequest_start(var_0, var_1, var_2, var_3, var_4, var_5)
       call pmpi_grequest_start(var_0, var_1, var_2, var_3, var_4, var_5)
        end program main
    