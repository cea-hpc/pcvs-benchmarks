
        
        program main
        use mpi
        
        INTEGER var_0
       CHARACTER*(10) var_1
       INTEGER var_2
       CHARACTER*(10) var_3
       LOGICAL var_4
       INTEGER var_5
        call mpi_info_get(var_0, var_1, var_2, var_3, var_4, var_5)
       call pmpi_info_get(var_0, var_1, var_2, var_3, var_4, var_5)
        end program main
    