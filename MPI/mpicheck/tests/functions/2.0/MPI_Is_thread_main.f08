
        
        program main
        use mpi_f08
        
        LOGICAL :: var_0
       INTEGER :: var_1
        call mpi_is_thread_main(var_0, var_1)
       call pmpi_is_thread_main(var_0, var_1)
        end program main
    