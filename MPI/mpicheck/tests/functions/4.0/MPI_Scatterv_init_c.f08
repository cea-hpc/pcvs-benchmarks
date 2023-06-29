
        
        program main
        use mpi_f08
        
        TYPE(INTEGER), DIMENSION(10) :: var_0
       INTEGER(KIND=MPI_COUNT_KIND), DIMENSION(10) :: var_1
       INTEGER(KIND=MPI_ADDRESS_KIND), DIMENSION(10) :: var_2
       TYPE(MPI_Datatype) :: var_3
       TYPE(INTEGER), DIMENSION(10) :: var_4
       INTEGER(KIND=MPI_COUNT_KIND) :: var_5
       TYPE(MPI_Datatype) :: var_6
       INTEGER :: var_7
       TYPE(MPI_Comm) :: var_8
       TYPE(MPI_Info) :: var_9
       TYPE(MPI_Request) :: var_10
       INTEGER :: var_11
        call mpi_scatterv_init(var_0, var_1, var_2, var_3, var_4, var_5, var_6, var_7, var_8, &
       var_9, var_10, var_11)
       call pmpi_scatterv_init(var_0, var_1, var_2, var_3, var_4, var_5, var_6, var_7, var_8, &
       var_9, var_10, var_11)
        end program main
    