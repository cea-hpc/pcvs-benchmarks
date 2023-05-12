
        
        program main
        include 'mpif.h'
        
        TYPE(INTEGER) var_0(10)
       INTEGER(KIND=MPI_COUNT_KIND) var_1(10)
       INTEGER(KIND=MPI_ADDRESS_KIND) var_2(10)
       INTEGER var_3
       TYPE(INTEGER) var_4(10)
       INTEGER(KIND=MPI_COUNT_KIND) var_5
       INTEGER var_6
       INTEGER var_7
       INTEGER var_8
       INTEGER var_9
       INTEGER var_10
        call mpi_iscatterv_c(var_0, var_1, var_2, var_3, var_4, var_5, var_6, var_7, var_8, var_9, &
       var_10)
       call pmpi_iscatterv_c(var_0, var_1, var_2, var_3, var_4, var_5, var_6, var_7, var_8, &
       var_9, var_10)
        end program main
    