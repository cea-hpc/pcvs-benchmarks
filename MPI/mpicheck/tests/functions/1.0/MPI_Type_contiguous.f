
        
        program main
        include 'mpif.h'
        
        INTEGER var_0
       INTEGER var_1
       INTEGER var_2
       INTEGER var_3
        call mpi_type_contiguous(var_0, var_1, var_2, var_3)
       call pmpi_type_contiguous(var_0, var_1, var_2, var_3)
        end program main
    