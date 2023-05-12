
        
        program main
        use mpi
        
        INTEGER var_0
       INTEGER var_1
        call mpi_request_free(var_0, var_1)
       call pmpi_request_free(var_0, var_1)
        end program main
    