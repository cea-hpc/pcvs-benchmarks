
        
        program main
        use mpi_f08
        
        CHARACTER(MPI_MAX_PROCESSOR_NAME) :: var_0
       INTEGER :: var_1
       INTEGER :: var_2
        call mpi_get_processor_name(var_0, var_1, var_2)
       call pmpi_get_processor_name(var_0, var_1, var_2)
        end program main
    