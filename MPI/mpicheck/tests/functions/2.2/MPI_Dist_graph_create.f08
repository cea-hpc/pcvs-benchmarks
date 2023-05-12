
        
        program main
        use mpi_f08
        
        TYPE(MPI_Comm) :: var_0
       INTEGER :: var_1
       INTEGER, DIMENSION(10) :: var_2
       INTEGER, DIMENSION(10) :: var_3
       INTEGER, DIMENSION(10) :: var_4
       INTEGER, DIMENSION(10) :: var_5
       TYPE(MPI_Info) :: var_6
       LOGICAL :: var_7
       TYPE(MPI_Comm) :: var_8
       INTEGER :: var_9
        call mpi_dist_graph_create(var_0, var_1, var_2, var_3, var_4, var_5, var_6, var_7, var_8, &
       var_9)
       call pmpi_dist_graph_create(var_0, var_1, var_2, var_3, var_4, var_5, var_6, var_7, var_8, &
       var_9)
        end program main
    