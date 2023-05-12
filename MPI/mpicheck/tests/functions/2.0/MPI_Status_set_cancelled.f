
        
        program main
        include 'mpif.h'
        
        INTEGER var_0(10)
       LOGICAL var_1
       INTEGER var_2
        call mpi_status_set_cancelled(var_0, var_1, var_2)
       call pmpi_status_set_cancelled(var_0, var_1, var_2)
        end program main
    