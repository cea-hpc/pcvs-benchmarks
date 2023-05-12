
        
        program main
        use mpi_f08
        
        TYPE(INTEGER), DIMENSION(10) :: var_0
       INTEGER(KIND=MPI_COUNT_KIND) :: var_1
       TYPE(MPI_Datatype) :: var_2
       TYPE(INTEGER), DIMENSION(10) :: var_3
       INTEGER(KIND=MPI_COUNT_KIND) :: var_4
       TYPE(MPI_Datatype) :: var_5
       TYPE(MPI_Comm) :: var_6
       INTEGER :: var_7
        call mpi_neighbor_allgather_c(var_0, var_1, var_2, var_3, var_4, var_5, var_6, var_7)
       call pmpi_neighbor_allgather_c(var_0, var_1, var_2, var_3, var_4, var_5, var_6, var_7)
        end program main
    