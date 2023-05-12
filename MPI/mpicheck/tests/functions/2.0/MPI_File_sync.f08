
        
        program main
        use mpi_f08
        
        TYPE(MPI_File) :: var_0
       INTEGER :: var_1
        call mpi_file_sync(var_0, var_1)
       call pmpi_file_sync(var_0, var_1)
        end program main
    