
        
        program main
        use mpi
        
        TYPE(INTEGER) var_0(10)
       INTEGER(KIND=MPI_COUNT_KIND) var_1
       INTEGER var_2
       TYPE(INTEGER) var_3(10)
       INTEGER(KIND=MPI_COUNT_KIND) var_4(10)
       INTEGER(KIND=MPI_ADDRESS_KIND) var_5(10)
       INTEGER var_6
       INTEGER var_7
       INTEGER var_8
       INTEGER var_9
        call mpi_iallgatherv_c(var_0, var_1, var_2, var_3, var_4, var_5, var_6, var_7, var_8, &
       var_9)
       call pmpi_iallgatherv_c(var_0, var_1, var_2, var_3, var_4, var_5, var_6, var_7, var_8, &
       var_9)
        end program main
    