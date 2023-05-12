
        
        program main
        use mpi_f08
        
        TYPE(MPI_Datatype) :: var_0
       INTEGER(KIND=MPI_COUNT_KIND) :: var_1
       INTEGER :: var_2
        call mpi_type_size_c(var_0, var_1, var_2)
       call pmpi_type_size_c(var_0, var_1, var_2)
        end program main
    