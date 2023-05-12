
        
        program main
        include 'mpif.h'
        
        INTEGER var_0
       INTEGER var_1
       INTEGER var_2
        call mpi_comm_set_errhandler(var_0, var_1, var_2)
       call pmpi_comm_set_errhandler(var_0, var_1, var_2)
        end program main
    