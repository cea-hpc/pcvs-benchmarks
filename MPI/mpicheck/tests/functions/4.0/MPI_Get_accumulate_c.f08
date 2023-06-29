
        
        program main
        use mpi_f08
        
        TYPE(INTEGER), DIMENSION(10) :: var_0
       INTEGER(KIND=MPI_COUNT_KIND) :: var_1
       TYPE(MPI_Datatype) :: var_2
       TYPE(INTEGER), DIMENSION(10) :: var_3
       INTEGER(KIND=MPI_COUNT_KIND) :: var_4
       TYPE(MPI_Datatype) :: var_5
       INTEGER :: var_6
       INTEGER(KIND=MPI_ADDRESS_KIND) :: var_7
       INTEGER(KIND=MPI_COUNT_KIND) :: var_8
       TYPE(MPI_Datatype) :: var_9
       TYPE(MPI_Op) :: var_10
       TYPE(MPI_Win) :: var_11
       INTEGER :: var_12
        call mpi_get_accumulate(var_0, var_1, var_2, var_3, var_4, var_5, var_6, var_7, var_8, &
       var_9, var_10, var_11, var_12)
       call pmpi_get_accumulate(var_0, var_1, var_2, var_3, var_4, var_5, var_6, var_7, var_8, &
       var_9, var_10, var_11, var_12)
        end program main
    