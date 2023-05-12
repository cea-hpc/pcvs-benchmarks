
        
        program main
        use mpi_f08
        
        INTEGER :: var_0
       INTEGER :: var_1
       TYPE(MPI_Comm) :: var_2
       TYPE(MPI_Status) :: var_3
       INTEGER :: var_4
        call mpi_probe(var_0, var_1, var_2, var_3, var_4)
       call pmpi_probe(var_0, var_1, var_2, var_3, var_4)
        end program main
    