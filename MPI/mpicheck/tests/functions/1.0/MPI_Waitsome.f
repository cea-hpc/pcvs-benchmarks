
        
        program main
        include 'mpif.h'
        
        INTEGER var_0
       INTEGER var_1(10)
       INTEGER var_2
       INTEGER var_3(10)
       INTEGER var_4(10,10)
       INTEGER var_5
        call mpi_waitsome(var_0, var_1, var_2, var_3, var_4, var_5)
       call pmpi_waitsome(var_0, var_1, var_2, var_3, var_4, var_5)
        end program main
    