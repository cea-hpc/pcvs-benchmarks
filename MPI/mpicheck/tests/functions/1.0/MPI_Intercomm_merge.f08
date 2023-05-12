
        
        program main
        use mpi_f08
        
        TYPE(MPI_Comm) :: var_0
       LOGICAL :: var_1
       TYPE(MPI_Comm) :: var_2
       INTEGER :: var_3
        call mpi_intercomm_merge(var_0, var_1, var_2, var_3)
       call pmpi_intercomm_merge(var_0, var_1, var_2, var_3)
        end program main
    