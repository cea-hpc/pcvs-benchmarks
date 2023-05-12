
        
        program main
        use mpi_f08
        
        TYPE(MPI_File) :: var_0
       INTEGER :: var_1
       INTEGER :: var_2
        call mpi_file_call_errhandler(var_0, var_1, var_2)
       call pmpi_file_call_errhandler(var_0, var_1, var_2)
        end program main
    