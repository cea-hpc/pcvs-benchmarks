
        
        program main
        use mpi
        
        TYPE(INTEGER) var_0(10)
       INTEGER(KIND=MPI_ADDRESS_KIND) var_1
       INTEGER var_2
        call mpi_get_address(var_0, var_1, var_2)
       call pmpi_get_address(var_0, var_1, var_2)
        end program main
    