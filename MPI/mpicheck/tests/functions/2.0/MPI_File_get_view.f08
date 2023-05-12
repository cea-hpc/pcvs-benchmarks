
        
        program main
        use mpi_f08
        
        TYPE(MPI_File) :: var_0
       INTEGER(KIND=MPI_OFFSET_KIND) :: var_1
       TYPE(MPI_Datatype) :: var_2
       TYPE(MPI_Datatype) :: var_3
       CHARACTER(10) :: var_4
       INTEGER :: var_5
        call mpi_file_get_view(var_0, var_1, var_2, var_3, var_4, var_5)
       call pmpi_file_get_view(var_0, var_1, var_2, var_3, var_4, var_5)
        end program main
    