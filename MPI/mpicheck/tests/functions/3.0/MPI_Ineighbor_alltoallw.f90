
        
        program main
        use mpi
        
        TYPE(INTEGER) var_0(10)
       INTEGER var_1(10)
       INTEGER(KIND=MPI_ADDRESS_KIND) var_2(10)
       INTEGER var_3(10)
       TYPE(INTEGER) var_4(10)
       INTEGER var_5(10)
       INTEGER(KIND=MPI_ADDRESS_KIND) var_6(10)
       INTEGER var_7(10)
       INTEGER var_8
       INTEGER var_9
       INTEGER var_10
        call mpi_ineighbor_alltoallw(var_0, var_1, var_2, var_3, var_4, var_5, var_6, var_7, &
       var_8, var_9, var_10)
       call pmpi_ineighbor_alltoallw(var_0, var_1, var_2, var_3, var_4, var_5, var_6, var_7, &
       var_8, var_9, var_10)
        end program main
    