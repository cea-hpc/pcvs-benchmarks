
        
        program main
        use mpi
        
        INTEGER var_0(10)
       INTEGER var_1
       INTEGER var_2
       INTEGER var_3
        call mpi_get_count(var_0, var_1, var_2, var_3)
       call pmpi_get_count(var_0, var_1, var_2, var_3)
        end program main
    