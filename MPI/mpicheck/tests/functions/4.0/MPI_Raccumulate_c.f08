
        
        program main
        use mpi_f08
        
        TYPE(INTEGER), DIMENSION(10) :: var_0
       INTEGER(KIND=MPI_COUNT_KIND) :: var_1
       TYPE(MPI_Datatype) :: var_2
       INTEGER :: var_3
       INTEGER(KIND=MPI_ADDRESS_KIND) :: var_4
       INTEGER(KIND=MPI_COUNT_KIND) :: var_5
       TYPE(MPI_Datatype) :: var_6
       TYPE(MPI_Op) :: var_7
       TYPE(MPI_Win) :: var_8
       TYPE(MPI_Request) :: var_9
       INTEGER :: var_10
        call mpi_raccumulate(var_0, var_1, var_2, var_3, var_4, var_5, var_6, var_7, var_8, var_9, &
       var_10)
       call pmpi_raccumulate(var_0, var_1, var_2, var_3, var_4, var_5, var_6, var_7, var_8, &
       var_9, var_10)
        end program main
    