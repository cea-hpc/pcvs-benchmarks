
        
        program main
        use mpi_f08
        
        TYPE(MPI_File) :: var_0
       INTEGER(KIND=MPI_OFFSET_KIND) :: var_1
       INTEGER :: var_2
       INTEGER :: var_3
        call mpi_file_seek_shared(var_0, var_1, var_2, var_3)
       call pmpi_file_seek_shared(var_0, var_1, var_2, var_3)
        end program main
    