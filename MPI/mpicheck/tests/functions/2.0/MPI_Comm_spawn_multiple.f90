
        
        program main
        use mpi
        
        INTEGER var_0
       CHARACTER*(10) var_1
       CHARACTER*(10) var_2
       INTEGER var_3(10)
       INTEGER var_4(10)
       INTEGER var_5
       INTEGER var_6
       INTEGER var_7
       INTEGER var_8
       INTEGER var_9
        call mpi_comm_spawn_multiple(var_0, var_1, var_2, var_3, var_4, var_5, var_6, var_7, &
       var_8, var_9)
       call pmpi_comm_spawn_multiple(var_0, var_1, var_2, var_3, var_4, var_5, var_6, var_7, &
       var_8, var_9)
        end program main
    