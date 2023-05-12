
        
        program main
        use mpi_f08
        
        TYPE(INTEGER), DIMENSION(10) :: var_0
       INTEGER :: var_1
        call mpi_free_mem(var_0, var_1)
       call pmpi_free_mem(var_0, var_1)
        end program main
    