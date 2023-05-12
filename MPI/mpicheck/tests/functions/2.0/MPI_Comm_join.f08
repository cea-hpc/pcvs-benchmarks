
        
        program main
        use mpi_f08
        
        INTEGER :: var_0
       TYPE(MPI_Comm) :: var_1
       INTEGER :: var_2
        call mpi_comm_join(var_0, var_1, var_2)
       call pmpi_comm_join(var_0, var_1, var_2)
        end program main
    