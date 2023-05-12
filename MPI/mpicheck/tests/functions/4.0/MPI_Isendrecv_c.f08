
        
        program main
        use mpi_f08
        
        TYPE(INTEGER), DIMENSION(10) :: var_0
       INTEGER(KIND=MPI_COUNT_KIND) :: var_1
       TYPE(MPI_Datatype) :: var_2
       INTEGER :: var_3
       INTEGER :: var_4
       TYPE(INTEGER), DIMENSION(10) :: var_5
       INTEGER(KIND=MPI_COUNT_KIND) :: var_6
       TYPE(MPI_Datatype) :: var_7
       INTEGER :: var_8
       INTEGER :: var_9
       TYPE(MPI_Comm) :: var_10
       TYPE(MPI_Request) :: var_11
       INTEGER :: var_12
        call mpi_isendrecv_c(var_0, var_1, var_2, var_3, var_4, var_5, var_6, var_7, var_8, var_9, &
       var_10, var_11, var_12)
       call pmpi_isendrecv_c(var_0, var_1, var_2, var_3, var_4, var_5, var_6, var_7, var_8, &
       var_9, var_10, var_11, var_12)
        end program main
    