
        
        program main
        use mpi_f08
        
        TYPE(MPI_Info) :: var_0
       CHARACTER(MPI_MAX_PORT_NAME) :: var_1
       INTEGER :: var_2
        call mpi_open_port(var_0, var_1, var_2)
       call pmpi_open_port(var_0, var_1, var_2)
        end program main
    