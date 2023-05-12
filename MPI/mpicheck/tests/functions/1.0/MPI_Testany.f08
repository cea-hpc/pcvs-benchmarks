
        
        program main
        use mpi_f08
        
        INTEGER :: var_0
       TYPE(MPI_Request), DIMENSION(10) :: var_1
       INTEGER :: var_2
       LOGICAL :: var_3
       TYPE(MPI_Status) :: var_4
       INTEGER :: var_5
        call mpi_testany(var_0, var_1, var_2, var_3, var_4, var_5)
       call pmpi_testany(var_0, var_1, var_2, var_3, var_4, var_5)
        end program main
    