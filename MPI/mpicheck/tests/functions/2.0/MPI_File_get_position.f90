
        
        program main
        use mpi
        
        INTEGER var_0
       INTEGER(KIND=MPI_OFFSET_KIND) var_1
       INTEGER var_2
        call mpi_file_get_position(var_0, var_1, var_2)
       call pmpi_file_get_position(var_0, var_1, var_2)
        end program main
    