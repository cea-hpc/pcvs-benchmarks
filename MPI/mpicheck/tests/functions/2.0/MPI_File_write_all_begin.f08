
        
        program main
        use mpi_f08
        
        TYPE(MPI_File) :: var_0
       TYPE(INTEGER), DIMENSION(10) :: var_1
       INTEGER :: var_2
       TYPE(MPI_Datatype) :: var_3
       INTEGER :: var_4
        call mpi_file_write_all_begin(var_0, var_1, var_2, var_3, var_4)
       call pmpi_file_write_all_begin(var_0, var_1, var_2, var_3, var_4)
        end program main
    