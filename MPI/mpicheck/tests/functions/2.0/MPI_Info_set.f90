
        
        program main
        use mpi
        
        INTEGER var_0
       CHARACTER*(10) var_1
       CHARACTER*(10) var_2
       INTEGER var_3
        call mpi_info_set(var_0, var_1, var_2, var_3)
       call pmpi_info_set(var_0, var_1, var_2, var_3)
        end program main
    