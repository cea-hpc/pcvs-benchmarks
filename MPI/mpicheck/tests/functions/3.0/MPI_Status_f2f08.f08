
        
        program main
        use mpi_f08
        
        INTEGER, DIMENSION(10) :: var_0
       TYPE(MPI_Status) :: var_1
       INTEGER :: var_2
        call mpi_status_f2f08(var_0, var_1, var_2)
       call pmpi_status_f2f08(var_0, var_1, var_2)
        end program main
    