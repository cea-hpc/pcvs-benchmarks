
        
        program main
        include 'mpif.h'
        
        TYPE(INTEGER) var_0(10)
       INTEGER(KIND=MPI_COUNT_KIND) var_1
       INTEGER var_2
       INTEGER var_3
       INTEGER(KIND=MPI_ADDRESS_KIND) var_4
       INTEGER(KIND=MPI_COUNT_KIND) var_5
       INTEGER var_6
       INTEGER var_7
       INTEGER var_8
        call mpi_put_c(var_0, var_1, var_2, var_3, var_4, var_5, var_6, var_7, var_8)
       call pmpi_put_c(var_0, var_1, var_2, var_3, var_4, var_5, var_6, var_7, var_8)
        end program main
    