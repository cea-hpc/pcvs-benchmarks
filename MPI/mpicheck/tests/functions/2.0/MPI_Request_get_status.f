
        
        program main
        include 'mpif.h'
        
        INTEGER var_0
       LOGICAL var_1
       INTEGER var_2(10)
       INTEGER var_3
        call mpi_request_get_status(var_0, var_1, var_2, var_3)
       call pmpi_request_get_status(var_0, var_1, var_2, var_3)
        end program main
    