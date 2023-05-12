
        
        program main
        use mpi_f08
        
        TYPE(MPI_Comm) :: var_0
       INTEGER :: var_1
       INTEGER :: var_2
        call mpi_comm_delete_attr(var_0, var_1, var_2)
       call pmpi_comm_delete_attr(var_0, var_1, var_2)
        end program main
    