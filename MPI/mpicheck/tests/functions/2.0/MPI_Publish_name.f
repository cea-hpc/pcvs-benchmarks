
        
        program main
        include 'mpif.h'
        
        CHARACTER*(10) var_0
       INTEGER var_1
       CHARACTER*(10) var_2
       INTEGER var_3
        call mpi_publish_name(var_0, var_1, var_2, var_3)
       call pmpi_publish_name(var_0, var_1, var_2, var_3)
        end program main
    