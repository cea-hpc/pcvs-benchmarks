
        
        program main
        include 'mpif.h'
        
        INTEGER var_0
       CHARACTER*(10) var_1
       INTEGER var_2
       INTEGER var_3
       INTEGER var_4
       INTEGER var_5
        call mpi_file_open(var_0, var_1, var_2, var_3, var_4, var_5)
       call pmpi_file_open(var_0, var_1, var_2, var_3, var_4, var_5)
        end program main
    