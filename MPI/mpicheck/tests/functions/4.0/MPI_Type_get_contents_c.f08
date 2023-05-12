
        
        program main
        use mpi_f08
        
        TYPE(MPI_Datatype) :: var_0
       INTEGER(KIND=MPI_COUNT_KIND) :: var_1
       INTEGER(KIND=MPI_COUNT_KIND) :: var_2
       INTEGER(KIND=MPI_COUNT_KIND) :: var_3
       INTEGER(KIND=MPI_COUNT_KIND) :: var_4
       INTEGER, DIMENSION(10) :: var_5
       INTEGER(KIND=MPI_ADDRESS_KIND), DIMENSION(10) :: var_6
       INTEGER(KIND=MPI_COUNT_KIND), DIMENSION(10) :: var_7
       TYPE(MPI_Datatype), DIMENSION(10) :: var_8
       INTEGER :: var_9
        call mpi_type_get_contents_c(var_0, var_1, var_2, var_3, var_4, var_5, var_6, var_7, &
       var_8, var_9)
       call pmpi_type_get_contents_c(var_0, var_1, var_2, var_3, var_4, var_5, var_6, var_7, &
       var_8, var_9)
        end program main
    