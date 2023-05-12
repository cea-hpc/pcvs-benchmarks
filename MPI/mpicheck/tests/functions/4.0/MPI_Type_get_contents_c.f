
        
        program main
        include 'mpif.h'
        
        INTEGER var_0
       INTEGER(KIND=MPI_COUNT_KIND) var_1
       INTEGER(KIND=MPI_COUNT_KIND) var_2
       INTEGER(KIND=MPI_COUNT_KIND) var_3
       INTEGER(KIND=MPI_COUNT_KIND) var_4
       INTEGER var_5(10)
       INTEGER(KIND=MPI_ADDRESS_KIND) var_6(10)
       INTEGER(KIND=MPI_COUNT_KIND) var_7(10)
       INTEGER var_8(10)
       INTEGER var_9
        call mpi_type_get_contents_c(var_0, var_1, var_2, var_3, var_4, var_5, var_6, var_7, &
       var_8, var_9)
       call pmpi_type_get_contents_c(var_0, var_1, var_2, var_3, var_4, var_5, var_6, var_7, &
       var_8, var_9)
        end program main
    