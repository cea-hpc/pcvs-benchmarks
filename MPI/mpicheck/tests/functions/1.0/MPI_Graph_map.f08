
        
        program main
        use mpi_f08
        
        TYPE(MPI_Comm) :: var_0
       INTEGER :: var_1
       INTEGER, DIMENSION(10) :: var_2
       INTEGER, DIMENSION(10) :: var_3
       INTEGER :: var_4
       INTEGER :: var_5
        call mpi_graph_map(var_0, var_1, var_2, var_3, var_4, var_5)
       call pmpi_graph_map(var_0, var_1, var_2, var_3, var_4, var_5)
        end program main
    