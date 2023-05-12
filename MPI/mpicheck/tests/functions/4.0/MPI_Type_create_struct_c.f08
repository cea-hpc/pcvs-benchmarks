
        
        program main
        use mpi_f08
        
        INTEGER(KIND=MPI_COUNT_KIND) :: var_0
       INTEGER(KIND=MPI_COUNT_KIND), DIMENSION(10) :: var_1
       INTEGER(KIND=MPI_COUNT_KIND), DIMENSION(10) :: var_2
       TYPE(MPI_Datatype), DIMENSION(10) :: var_3
       TYPE(MPI_Datatype) :: var_4
       INTEGER :: var_5
        call mpi_type_create_struct_c(var_0, var_1, var_2, var_3, var_4, var_5)
       call pmpi_type_create_struct_c(var_0, var_1, var_2, var_3, var_4, var_5)
        end program main
    