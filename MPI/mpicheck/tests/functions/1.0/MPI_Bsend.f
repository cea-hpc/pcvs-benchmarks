
        
        program main
        include 'mpif.h'
        
        TYPE(INTEGER) var_0(10)
       INTEGER var_1
       INTEGER var_2
       INTEGER var_3
       INTEGER var_4
       INTEGER var_5
       INTEGER var_6
        call mpi_bsend(var_0, var_1, var_2, var_3, var_4, var_5, var_6)
       call pmpi_bsend(var_0, var_1, var_2, var_3, var_4, var_5, var_6)
        end program main
    