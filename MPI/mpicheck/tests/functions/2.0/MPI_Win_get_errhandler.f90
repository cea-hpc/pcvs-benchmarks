
        
        program main
        use mpi
        
        INTEGER var_0
       INTEGER var_1
       INTEGER var_2
        call mpi_win_get_errhandler(var_0, var_1, var_2)
       call pmpi_win_get_errhandler(var_0, var_1, var_2)
        end program main
    