
        
        program main
        use mpi_f08
        
        INTEGER :: var_0
       INTEGER :: var_1
        call mpi_win_free_keyval(var_0, var_1)
       call pmpi_win_free_keyval(var_0, var_1)
        end program main
    