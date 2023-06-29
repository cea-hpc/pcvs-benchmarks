
        
        program main
        use mpi_f08
        
        TYPE(MPI_File) :: var_0
       INTEGER(KIND=MPI_OFFSET_KIND) :: var_1
       TYPE(INTEGER), DIMENSION(10) :: var_2
       INTEGER(KIND=MPI_COUNT_KIND) :: var_3
       TYPE(MPI_Datatype) :: var_4
       INTEGER :: var_5
        call mpi_file_write_at_all_begin(var_0, var_1, var_2, var_3, var_4, var_5)
       call pmpi_file_write_at_all_begin(var_0, var_1, var_2, var_3, var_4, var_5)
        end program main
    