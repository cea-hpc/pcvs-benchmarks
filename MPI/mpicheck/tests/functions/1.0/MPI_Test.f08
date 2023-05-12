
        
        program main
        use mpi_f08
        
        TYPE(MPI_Request) :: var_0
       LOGICAL :: var_1
       TYPE(MPI_Status) :: var_2
       INTEGER :: var_3
        call mpi_test(var_0, var_1, var_2, var_3)
       call pmpi_test(var_0, var_1, var_2, var_3)
        end program main
    