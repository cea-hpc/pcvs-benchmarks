
        
        program main
        use mpi
        
        INTEGER var_0
       INTEGER(KIND=MPI_COUNT_KIND) var_1(10)
       INTEGER(KIND=MPI_COUNT_KIND) var_2(10)
       INTEGER(KIND=MPI_COUNT_KIND) var_3(10)
       INTEGER var_4
       INTEGER var_5
       INTEGER var_6
       INTEGER var_7
        call mpi_type_create_subarray_c(var_0, var_1, var_2, var_3, var_4, var_5, var_6, var_7)
       call pmpi_type_create_subarray_c(var_0, var_1, var_2, var_3, var_4, var_5, var_6, var_7)
        end program main
    