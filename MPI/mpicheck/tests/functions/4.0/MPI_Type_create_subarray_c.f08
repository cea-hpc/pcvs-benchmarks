
        
        program main
        use mpi_f08
        
        INTEGER :: var_0
       INTEGER(KIND=MPI_COUNT_KIND), DIMENSION(10) :: var_1
       INTEGER(KIND=MPI_COUNT_KIND), DIMENSION(10) :: var_2
       INTEGER(KIND=MPI_COUNT_KIND), DIMENSION(10) :: var_3
       INTEGER :: var_4
       TYPE(MPI_Datatype) :: var_5
       TYPE(MPI_Datatype) :: var_6
       INTEGER :: var_7
        call mpi_type_create_subarray(var_0, var_1, var_2, var_3, var_4, var_5, var_6, var_7)
       call pmpi_type_create_subarray(var_0, var_1, var_2, var_3, var_4, var_5, var_6, var_7)
        end program main
    