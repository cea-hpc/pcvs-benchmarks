
        
        program main
        include 'mpif.h'
        
        INTEGER var_0
       INTEGER var_1(10)
       INTEGER var_2(10,10)
       INTEGER var_3
        call mpi_waitall(var_0, var_1, var_2, var_3)
       call pmpi_waitall(var_0, var_1, var_2, var_3)
        end program main
    