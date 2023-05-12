
        
        program main
        use mpi_f08
        
        INTEGER :: var_0
       INTEGER :: var_1
       TYPE(MPI_Request) :: var_2
       INTEGER :: var_3
        call mpi_pready_range(var_0, var_1, var_2, var_3)
       call pmpi_pready_range(var_0, var_1, var_2, var_3)
        end program main
    