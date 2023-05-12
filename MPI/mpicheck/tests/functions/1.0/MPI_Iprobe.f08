
        
        program main
        use mpi_f08
        
        INTEGER :: var_0
       INTEGER :: var_1
       TYPE(MPI_Comm) :: var_2
       LOGICAL :: var_3
       TYPE(MPI_Status) :: var_4
       INTEGER :: var_5
        call mpi_iprobe(var_0, var_1, var_2, var_3, var_4, var_5)
       call pmpi_iprobe(var_0, var_1, var_2, var_3, var_4, var_5)
        end program main
    