
        
        program main
        use mpi
        
        TYPE(INTEGER) var_0(10)
       INTEGER var_1
        call mpi_free_mem(var_0, var_1)
       call pmpi_free_mem(var_0, var_1)
        end program main
    