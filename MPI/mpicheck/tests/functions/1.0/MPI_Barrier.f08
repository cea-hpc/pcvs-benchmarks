
        
        program main
        use mpi_f08
        
        TYPE(MPI_Comm) :: var_0
       INTEGER :: var_1
        call mpi_barrier(var_0, var_1)
       call pmpi_barrier(var_0, var_1)
        end program main
    