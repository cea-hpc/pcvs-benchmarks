
        
        program main
        use mpi_f08
        
        TYPE(MPI_Comm) :: var_0
       INTEGER :: var_1
       INTEGER, DIMENSION(10) :: var_2
       INTEGER, DIMENSION(10) :: var_3
       INTEGER :: var_4
       INTEGER, DIMENSION(10) :: var_5
       INTEGER, DIMENSION(10) :: var_6
       INTEGER :: var_7
        call mpi_dist_graph_neighbors(var_0, var_1, var_2, var_3, var_4, var_5, var_6, var_7)
       call pmpi_dist_graph_neighbors(var_0, var_1, var_2, var_3, var_4, var_5, var_6, var_7)
        end program main
    