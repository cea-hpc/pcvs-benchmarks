
        
        program main
        use mpi
        
        INTEGER var_2
       INTEGER var_3
       INTEGER var_4
        call mpi_init_thread(var_2, var_3, var_4)
       call pmpi_init_thread(var_2, var_3, var_4)
        end program main
    