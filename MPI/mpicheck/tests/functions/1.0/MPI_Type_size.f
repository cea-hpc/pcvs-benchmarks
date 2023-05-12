
        
        program main
        include 'mpif.h'
        
        INTEGER var_0
       INTEGER var_1
       INTEGER var_2
        call mpi_type_size(var_0, var_1, var_2)
       call pmpi_type_size(var_0, var_1, var_2)
        end program main
    