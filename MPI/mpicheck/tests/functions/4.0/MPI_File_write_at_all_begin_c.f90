
        
        program main
        use mpi
        
        INTEGER var_0
       INTEGER(KIND=MPI_OFFSET_KIND) var_1
       TYPE(INTEGER) var_2(10)
       INTEGER(KIND=MPI_COUNT_KIND) var_3
       INTEGER var_4
       INTEGER var_5
        call mpi_file_write_at_all_begin_c(var_0, var_1, var_2, var_3, var_4, var_5)
       call pmpi_file_write_at_all_begin_c(var_0, var_1, var_2, var_3, var_4, var_5)
        end program main
    