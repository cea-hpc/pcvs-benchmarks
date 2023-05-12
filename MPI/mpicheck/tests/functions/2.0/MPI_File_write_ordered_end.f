
        
        program main
        include 'mpif.h'
        
        INTEGER var_0
       TYPE(INTEGER) var_1(10)
       INTEGER var_2(10)
       INTEGER var_3
        call mpi_file_write_ordered_end(var_0, var_1, var_2, var_3)
       call pmpi_file_write_ordered_end(var_0, var_1, var_2, var_3)
        end program main
    