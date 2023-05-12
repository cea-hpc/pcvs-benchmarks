
        
        program main
        include 'mpif.h'
        
        INTEGER var_0
       INTEGER var_1
        call mpi_barrier(var_0, var_1)
       call pmpi_barrier(var_0, var_1)
        end program main
    