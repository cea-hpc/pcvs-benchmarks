
        
        program main
        include 'mpif.h'
        
        INTEGER var_0
       INTEGER var_1
       INTEGER var_2(10)
       INTEGER var_3(10)
       INTEGER var_4
       INTEGER var_5(10)
       INTEGER var_6(10)
       INTEGER var_7
        call mpi_dist_graph_neighbors(var_0, var_1, var_2, var_3, var_4, var_5, var_6, var_7)
       call pmpi_dist_graph_neighbors(var_0, var_1, var_2, var_3, var_4, var_5, var_6, var_7)
        end program main
    