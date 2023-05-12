
        
        program main
        use mpi
        
        INTEGER var_0
       INTEGER var_1
       INTEGER var_2
       INTEGER var_3
        call mpi_win_create_dynamic(var_0, var_1, var_2, var_3)
       call pmpi_win_create_dynamic(var_0, var_1, var_2, var_3)
        end program main
    