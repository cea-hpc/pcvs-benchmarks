
        
        program main
        use mpi_f08
        
        INTEGER :: var_0
       CHARACTER(10), DIMENSION(10) :: var_1
       CHARACTER(10), DIMENSION(10, 10) :: var_2
       INTEGER, DIMENSION(10) :: var_3
       TYPE(MPI_Info), DIMENSION(10) :: var_4
       INTEGER :: var_5
       TYPE(MPI_Comm) :: var_6
       TYPE(MPI_Comm) :: var_7
       INTEGER, DIMENSION(10) :: var_8
       INTEGER :: var_9
        call mpi_comm_spawn_multiple(var_0, var_1, var_2, var_3, var_4, var_5, var_6, var_7, &
       var_8, var_9)
       call pmpi_comm_spawn_multiple(var_0, var_1, var_2, var_3, var_4, var_5, var_6, var_7, &
       var_8, var_9)
        end program main
    
