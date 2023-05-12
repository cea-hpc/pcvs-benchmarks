
        
        program main
        use mpi_f08
        
        TYPE(MPI_Comm) :: var_0
       INTEGER :: var_1
       TYPE(MPI_Comm) :: var_2
       INTEGER :: var_3
       INTEGER :: var_4
       TYPE(MPI_Comm) :: var_5
       INTEGER :: var_6
        call mpi_intercomm_create(var_0, var_1, var_2, var_3, var_4, var_5, var_6)
       call pmpi_intercomm_create(var_0, var_1, var_2, var_3, var_4, var_5, var_6)
        end program main
    