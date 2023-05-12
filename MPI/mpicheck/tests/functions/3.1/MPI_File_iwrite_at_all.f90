
        
        program main
        use mpi
        
        INTEGER var_0
       INTEGER(KIND=MPI_OFFSET_KIND) var_1
       TYPE(INTEGER) var_2(10)
       INTEGER var_3
       INTEGER var_4
       INTEGER var_5
       INTEGER var_6
        call mpi_file_iwrite_at_all(var_0, var_1, var_2, var_3, var_4, var_5, var_6)
       call pmpi_file_iwrite_at_all(var_0, var_1, var_2, var_3, var_4, var_5, var_6)
        end program main
    