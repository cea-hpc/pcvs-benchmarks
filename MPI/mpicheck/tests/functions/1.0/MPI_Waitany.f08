
        
        program main
        use mpi_f08
        
        INTEGER :: var_0
       TYPE(MPI_Request), DIMENSION(10) :: var_1
       INTEGER :: var_2
       TYPE(MPI_Status) :: var_3
       INTEGER :: var_4
        call mpi_waitany(var_0, var_1, var_2, var_3, var_4)
       call pmpi_waitany(var_0, var_1, var_2, var_3, var_4)
        end program main
    