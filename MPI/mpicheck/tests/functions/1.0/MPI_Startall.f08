
        
        program main
        use mpi_f08
        
        INTEGER :: var_0
       TYPE(MPI_Request), DIMENSION(10) :: var_1
       INTEGER :: var_2
        call mpi_startall(var_0, var_1, var_2)
       call pmpi_startall(var_0, var_1, var_2)
        end program main
    