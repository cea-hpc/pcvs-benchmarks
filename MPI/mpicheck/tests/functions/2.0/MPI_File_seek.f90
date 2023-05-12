
        
        program main
        use mpi
        
        INTEGER var_0
       INTEGER(KIND=MPI_OFFSET_KIND) var_1
       INTEGER var_2
       INTEGER var_3
        call mpi_file_seek(var_0, var_1, var_2, var_3)
       call pmpi_file_seek(var_0, var_1, var_2, var_3)
        end program main
    