
        
        program main
        use mpi_f08
        
        TYPE(MPI_Comm) :: var_0
       INTEGER :: var_1
       INTEGER :: var_2
       TYPE(MPI_Info) :: var_3
       TYPE(MPI_Comm) :: var_4
       INTEGER :: var_5
        call mpi_comm_split_type(var_0, var_1, var_2, var_3, var_4, var_5)
       call pmpi_comm_split_type(var_0, var_1, var_2, var_3, var_4, var_5)
        end program main
    