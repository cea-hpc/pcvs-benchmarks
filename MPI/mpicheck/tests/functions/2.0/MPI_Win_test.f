
        
        program main
        include 'mpif.h'
        
        INTEGER var_0
       LOGICAL var_1
       INTEGER var_2
        call mpi_win_test(var_0, var_1, var_2)
       call pmpi_win_test(var_0, var_1, var_2)
        end program main
    