
        
        program main
        include 'mpif.h'
        
        INTEGER var_2
       INTEGER var_3
        call mpi_info_create_env(var_2, var_3)
       call pmpi_info_create_env(var_2, var_3)
        end program main
    