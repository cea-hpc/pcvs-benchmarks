
        
        program main
        use mpi_f08
        
        INTEGER :: var_0
       CHARACTER(MPI_MAX_ERROR_STRING) :: var_1
       INTEGER :: var_2
       INTEGER :: var_3
        call mpi_error_string(var_0, var_1, var_2, var_3)
       call pmpi_error_string(var_0, var_1, var_2, var_3)
        end program main
    