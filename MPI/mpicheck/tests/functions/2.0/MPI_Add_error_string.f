
        
        program main
        include 'mpif.h'
        
        INTEGER var_0
       CHARACTER*(10) var_1
       INTEGER var_2
        call mpi_add_error_string(var_0, var_1, var_2)
       call pmpi_add_error_string(var_0, var_1, var_2)
        end program main
    