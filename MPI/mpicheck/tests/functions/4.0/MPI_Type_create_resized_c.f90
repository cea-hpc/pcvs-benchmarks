
        
        program main
        use mpi
        
        INTEGER var_0
       INTEGER(KIND=MPI_COUNT_KIND) var_1
       INTEGER(KIND=MPI_COUNT_KIND) var_2
       INTEGER var_3
       INTEGER var_4
        call mpi_type_create_resized_c(var_0, var_1, var_2, var_3, var_4)
       call pmpi_type_create_resized_c(var_0, var_1, var_2, var_3, var_4)
        end program main
    