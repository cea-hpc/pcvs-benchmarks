
        
        program main
        use mpi_f08
        
        INTEGER :: var_0
       INTEGER :: var_1
        call mpi_comm_free_keyval(var_0, var_1)
       call pmpi_comm_free_keyval(var_0, var_1)
        end program main
    