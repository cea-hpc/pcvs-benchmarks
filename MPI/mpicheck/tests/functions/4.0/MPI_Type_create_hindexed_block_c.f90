
        
        program main
        use mpi
        
        INTEGER(KIND=MPI_COUNT_KIND) var_0
       INTEGER(KIND=MPI_COUNT_KIND) var_1
       INTEGER(KIND=MPI_COUNT_KIND) var_2(10)
       INTEGER var_3
       INTEGER var_4
       INTEGER var_5
        call mpi_type_create_hindexed_block_c(var_0, var_1, var_2, var_3, var_4, var_5)
       call pmpi_type_create_hindexed_block_c(var_0, var_1, var_2, var_3, var_4, var_5)
        end program main
    