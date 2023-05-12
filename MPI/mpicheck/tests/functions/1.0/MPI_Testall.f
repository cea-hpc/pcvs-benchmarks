
        
        program main
        include 'mpif.h'
        
        INTEGER var_0
       INTEGER var_1(10)
       LOGICAL var_2
       INTEGER var_3(10,10)
       INTEGER var_4
        call mpi_testall(var_0, var_1, var_2, var_3, var_4)
       call pmpi_testall(var_0, var_1, var_2, var_3, var_4)
        end program main
    