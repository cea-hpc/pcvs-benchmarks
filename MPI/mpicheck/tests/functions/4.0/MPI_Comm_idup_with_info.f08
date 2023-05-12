
        
        program main
        use mpi_f08
        
        TYPE(MPI_Comm) :: var_0
       TYPE(MPI_Info) :: var_1
       TYPE(MPI_Comm) :: var_2
       TYPE(MPI_Request) :: var_3
       INTEGER :: var_4
        call mpi_comm_idup_with_info(var_0, var_1, var_2, var_3, var_4)
       call pmpi_comm_idup_with_info(var_0, var_1, var_2, var_3, var_4)
        end program main
    