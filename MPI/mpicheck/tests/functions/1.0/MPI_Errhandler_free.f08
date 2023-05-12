
        
        program main
        use mpi_f08
        
        TYPE(MPI_Errhandler) :: var_0
       INTEGER :: var_1
        call mpi_errhandler_free(var_0, var_1)
       call pmpi_errhandler_free(var_0, var_1)
        end program main
    