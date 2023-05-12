
        
        program main
        use mpi
        
        TYPE(MPI_Status) var_0
       INTEGER var_1(10)
       INTEGER var_2
        call mpi_status_f082f(var_0, var_1, var_2)
       call pmpi_status_f082f(var_0, var_1, var_2)
        end program main
    