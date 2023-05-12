
        
        program main
        use mpi_f08
        
        TYPE(MPI_Request) :: var_0
       TYPE(MPI_Status) :: var_1
       INTEGER :: var_2
        call mpi_wait(var_0, var_1, var_2)
       call pmpi_wait(var_0, var_1, var_2)
        end program main
    