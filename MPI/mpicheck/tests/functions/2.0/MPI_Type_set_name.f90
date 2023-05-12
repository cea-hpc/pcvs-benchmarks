
        
        program main
        use mpi
        
        INTEGER var_0
       CHARACTER*(10) var_1
       INTEGER var_2
        call mpi_type_set_name(var_0, var_1, var_2)
       call pmpi_type_set_name(var_0, var_1, var_2)
        end program main
    