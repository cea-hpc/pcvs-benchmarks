
        
        program main
        use mpi
        
        TYPE(INTEGER) var_0(10)
       TYPE(INTEGER) var_1(10)
       INTEGER(KIND=MPI_COUNT_KIND) var_2
       INTEGER var_3
       INTEGER var_4
       INTEGER var_5
        call mpi_reduce_local_c(var_0, var_1, var_2, var_3, var_4, var_5)
       call pmpi_reduce_local_c(var_0, var_1, var_2, var_3, var_4, var_5)
        end program main
    