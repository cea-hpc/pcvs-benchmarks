
        
        program main
        use mpi_f08
        
        TYPE(MPI_File) :: var_0
       TYPE(MPI_Datatype) :: var_1
       INTEGER(KIND=MPI_COUNT_KIND) :: var_2
       INTEGER :: var_3
        call mpi_file_get_type_extent_c(var_0, var_1, var_2, var_3)
       call pmpi_file_get_type_extent_c(var_0, var_1, var_2, var_3)
        end program main
    