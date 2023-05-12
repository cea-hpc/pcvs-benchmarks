
        
        program main
        include 'mpif.h'
        
        LOGICAL var_0
       INTEGER var_1
        call mpi_initialized(var_0, var_1)
       call pmpi_initialized(var_0, var_1)
        end program main
    