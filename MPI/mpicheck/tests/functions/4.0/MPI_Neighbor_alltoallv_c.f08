
        
        program main
        use mpi_f08
        
        TYPE(INTEGER), DIMENSION(10) :: var_0
       INTEGER(KIND=MPI_COUNT_KIND), DIMENSION(10) :: var_1
       INTEGER(KIND=MPI_ADDRESS_KIND), DIMENSION(10) :: var_2
       TYPE(MPI_Datatype) :: var_3
       TYPE(INTEGER), DIMENSION(10) :: var_4
       INTEGER(KIND=MPI_COUNT_KIND), DIMENSION(10) :: var_5
       INTEGER(KIND=MPI_ADDRESS_KIND), DIMENSION(10) :: var_6
       TYPE(MPI_Datatype) :: var_7
       TYPE(MPI_Comm) :: var_8
       INTEGER :: var_9
        call mpi_neighbor_alltoallv_c(var_0, var_1, var_2, var_3, var_4, var_5, var_6, var_7, &
       var_8, var_9)
       call pmpi_neighbor_alltoallv_c(var_0, var_1, var_2, var_3, var_4, var_5, var_6, var_7, &
       var_8, var_9)
        end program main
    