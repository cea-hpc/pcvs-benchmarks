
        
        program main
        use mpi_f08
        
        TYPE(MPI_File) :: var_0
       TYPE(MPI_Errhandler) :: var_1
       INTEGER :: var_2
        call mpi_file_get_errhandler(var_0, var_1, var_2)
       call pmpi_file_get_errhandler(var_0, var_1, var_2)
        end program main
    