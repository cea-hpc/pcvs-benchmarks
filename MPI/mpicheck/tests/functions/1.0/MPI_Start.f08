
        
        program main
        use mpi_f08
        
        TYPE(MPI_Request) :: var_0
       INTEGER :: var_1
        call mpi_start(var_0, var_1)
       call pmpi_start(var_0, var_1)
        end program main
    