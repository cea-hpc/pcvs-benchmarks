
        
        program main
        use mpi_f08
        
        TYPE(MPI_File) :: var_0
       INTEGER(KIND=MPI_OFFSET_KIND) :: var_1
       TYPE(INTEGER), DIMENSION(10) :: var_2
       INTEGER(KIND=MPI_COUNT_KIND) :: var_3
       TYPE(MPI_Datatype) :: var_4
       TYPE(MPI_Status) :: var_5
       INTEGER :: var_6
        call mpi_file_read_at_all(var_0, var_1, var_2, var_3, var_4, var_5, var_6)
       call pmpi_file_read_at_all(var_0, var_1, var_2, var_3, var_4, var_5, var_6)
        end program main
    