
        
        program main
        use mpi_f08
        
        TYPE(MPI_Datatype) :: var_0
       INTEGER :: var_1
       INTEGER(KIND=MPI_ADDRESS_KIND) :: var_2
       INTEGER :: var_3
        call mpi_type_set_attr(var_0, var_1, var_2, var_3)
       call pmpi_type_set_attr(var_0, var_1, var_2, var_3)
        end program main
    