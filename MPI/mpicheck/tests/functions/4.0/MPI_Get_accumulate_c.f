
        
        program main
        include 'mpif.h'
        
        TYPE(INTEGER) var_0(10)
       INTEGER(KIND=MPI_COUNT_KIND) var_1
       INTEGER var_2
       TYPE(INTEGER) var_3(10)
       INTEGER(KIND=MPI_COUNT_KIND) var_4
       INTEGER var_5
       INTEGER var_6
       INTEGER(KIND=MPI_ADDRESS_KIND) var_7
       INTEGER(KIND=MPI_COUNT_KIND) var_8
       INTEGER var_9
       INTEGER var_10
       INTEGER var_11
       INTEGER var_12
        call mpi_get_accumulate_c(var_0, var_1, var_2, var_3, var_4, var_5, var_6, var_7, var_8, &
       var_9, var_10, var_11, var_12)
       call pmpi_get_accumulate_c(var_0, var_1, var_2, var_3, var_4, var_5, var_6, var_7, var_8, &
       var_9, var_10, var_11, var_12)
        end program main
    