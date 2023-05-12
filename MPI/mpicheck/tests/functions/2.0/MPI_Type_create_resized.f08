
        
        program main
        use mpi_f08
        
        TYPE(MPI_Datatype) :: var_0
       INTEGER(KIND=MPI_ADDRESS_KIND) :: var_1
       INTEGER(KIND=MPI_ADDRESS_KIND) :: var_2
       TYPE(MPI_Datatype) :: var_3
       INTEGER :: var_4
        call mpi_type_create_resized(var_0, var_1, var_2, var_3, var_4)
       call pmpi_type_create_resized(var_0, var_1, var_2, var_3, var_4)
        end program main
    