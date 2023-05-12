
        
        program main
        use mpi_f08
        
        CHARACTER(10) :: var_0
       INTEGER :: var_1
        call mpi_close_port(var_0, var_1)
       call pmpi_close_port(var_0, var_1)
        end program main
    