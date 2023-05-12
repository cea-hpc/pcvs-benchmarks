
        
        program main
        include 'mpif.h'
        
        INTEGER var_0
       TYPE(INTEGER) var_1(10)
       INTEGER(KIND=MPI_COUNT_KIND) var_2
       INTEGER var_3
       INTEGER var_4
        call mpi_file_read_ordered_begin_c(var_0, var_1, var_2, var_3, var_4)
       call pmpi_file_read_ordered_begin_c(var_0, var_1, var_2, var_3, var_4)
        end program main
    