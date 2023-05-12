
        
        program main
        include 'mpif.h'
        
        INTEGER var_0
       INTEGER var_1
       INTEGER var_2
       INTEGER var_3
       INTEGER var_4
        call mpi_comm_split(var_0, var_1, var_2, var_3, var_4)
       call pmpi_comm_split(var_0, var_1, var_2, var_3, var_4)
        end program main
    