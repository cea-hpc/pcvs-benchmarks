
        
        program main
        use mpi_f08
        
        TYPE(MPI_Comm) :: var_0
       TYPE(MPI_Info) :: var_1
       TYPE(MPI_Comm) :: var_2
       INTEGER :: var_3
        call mpi_comm_dup_with_info(var_0, var_1, var_2, var_3)
       call pmpi_comm_dup_with_info(var_0, var_1, var_2, var_3)
        end program main
    