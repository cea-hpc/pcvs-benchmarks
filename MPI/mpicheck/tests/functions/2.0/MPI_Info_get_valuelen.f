
        
        program main
        include 'mpif.h'
        
        INTEGER var_0
       CHARACTER*(10) var_1
       INTEGER var_2
       LOGICAL var_3
       INTEGER var_4
        call mpi_info_get_valuelen(var_0, var_1, var_2, var_3, var_4)
       call pmpi_info_get_valuelen(var_0, var_1, var_2, var_3, var_4)
        end program main
    