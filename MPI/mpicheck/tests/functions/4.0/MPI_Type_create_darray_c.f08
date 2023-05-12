
        
        program main
        use mpi_f08
        
        INTEGER :: var_0
       INTEGER :: var_1
       INTEGER :: var_2
       INTEGER(KIND=MPI_COUNT_KIND), DIMENSION(10) :: var_3
       INTEGER, DIMENSION(10) :: var_4
       INTEGER, DIMENSION(10) :: var_5
       INTEGER, DIMENSION(10) :: var_6
       INTEGER :: var_7
       TYPE(MPI_Datatype) :: var_8
       TYPE(MPI_Datatype) :: var_9
       INTEGER :: var_10
        call mpi_type_create_darray_c(var_0, var_1, var_2, var_3, var_4, var_5, var_6, var_7, &
       var_8, var_9, var_10)
       call pmpi_type_create_darray_c(var_0, var_1, var_2, var_3, var_4, var_5, var_6, var_7, &
       var_8, var_9, var_10)
        end program main
    