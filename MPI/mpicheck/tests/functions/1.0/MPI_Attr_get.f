
        
        program main
        include 'mpif.h'
        
        INTEGER var_0
       INTEGER var_1
       INTEGER var_2
       LOGICAL var_3
       INTEGER var_4
        call mpi_attr_get(var_0, var_1, var_2, var_3, var_4)
       call pmpi_attr_get(var_0, var_1, var_2, var_3, var_4)
        end program main
    