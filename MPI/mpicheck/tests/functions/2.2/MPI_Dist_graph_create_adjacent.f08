
        
        program main
        use mpi_f08
        
        TYPE(MPI_Comm) :: var_0
       INTEGER :: var_1
       INTEGER, DIMENSION(10) :: var_2
       INTEGER, DIMENSION(10) :: var_3
       INTEGER :: var_4
       INTEGER, DIMENSION(10) :: var_5
       INTEGER, DIMENSION(10) :: var_6
       TYPE(MPI_Info) :: var_7
       LOGICAL :: var_8
       TYPE(MPI_Comm) :: var_9
       INTEGER :: var_10
        call mpi_dist_graph_create_adjacent(var_0, var_1, var_2, var_3, var_4, var_5, var_6, &
       var_7, var_8, var_9, var_10)
       call pmpi_dist_graph_create_adjacent(var_0, var_1, var_2, var_3, var_4, var_5, var_6, &
       var_7, var_8, var_9, var_10)
        end program main
    