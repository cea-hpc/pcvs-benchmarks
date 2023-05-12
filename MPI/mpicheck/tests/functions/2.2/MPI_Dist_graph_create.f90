
        
        program main
        use mpi
        
        INTEGER var_0
       INTEGER var_1
       INTEGER var_2(10)
       INTEGER var_3(10)
       INTEGER var_4(10)
       INTEGER var_5(10)
       INTEGER var_6
       LOGICAL var_7
       INTEGER var_8
       INTEGER var_9
        call mpi_dist_graph_create(var_0, var_1, var_2, var_3, var_4, var_5, var_6, var_7, var_8, &
       var_9)
       call pmpi_dist_graph_create(var_0, var_1, var_2, var_3, var_4, var_5, var_6, var_7, var_8, &
       var_9)
        end program main
    