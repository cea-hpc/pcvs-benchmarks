
        
subroutine var_1(val, ierr)
    integer val, ierr
end subroutine

subroutine var_2(val, ierr)
    integer val, ierr
end subroutine

subroutine var_3(val, ierr)
    integer val, ierr
end subroutine
        program main
        include 'mpif.h'
        
        CHARACTER*(10) var_0
       EXTERNAL var_1
       EXTERNAL var_2
       EXTERNAL var_3
       INTEGER(KIND=MPI_ADDRESS_KIND) var_4
       INTEGER var_5
        call mpi_register_datarep_c(var_0, var_1, var_2, var_3, var_4, var_5)
       call pmpi_register_datarep_c(var_0, var_1, var_2, var_3, var_4, var_5)
        end program main
    