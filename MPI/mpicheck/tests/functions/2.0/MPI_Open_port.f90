
        
        program main
        use mpi
        
        INTEGER var_0
       CHARACTER*(10) var_1
       INTEGER var_2
        call mpi_open_port(var_0, var_1, var_2)
       call pmpi_open_port(var_0, var_1, var_2)
        end program main
    