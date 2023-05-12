
        
        program main
        include 'mpif.h'
        
        TYPE(INTEGER) var_0(10)
       INTEGER(KIND=MPI_COUNT_KIND) var_1
       INTEGER var_2
       INTEGER var_3
       INTEGER var_4
       INTEGER var_5
        call mpi_imrecv_c(var_0, var_1, var_2, var_3, var_4, var_5)
       call pmpi_imrecv_c(var_0, var_1, var_2, var_3, var_4, var_5)
        end program main
    