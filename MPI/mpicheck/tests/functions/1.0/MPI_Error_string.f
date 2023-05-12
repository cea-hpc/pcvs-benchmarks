
        
        program main
        include 'mpif.h'
        
        INTEGER var_0
       CHARACTER*(10) var_1
       INTEGER var_2
       INTEGER var_3
        call mpi_error_string(var_0, var_1, var_2, var_3)
       call pmpi_error_string(var_0, var_1, var_2, var_3)
        end program main
    