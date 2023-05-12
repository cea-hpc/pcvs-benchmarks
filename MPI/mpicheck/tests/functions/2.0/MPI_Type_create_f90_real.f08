
        
        program main
        use mpi_f08
        
        INTEGER :: var_0
       INTEGER :: var_1
       TYPE(MPI_Datatype) :: var_2
       INTEGER :: var_3
        call mpi_type_create_f90_real(var_0, var_1, var_2, var_3)
       call pmpi_type_create_f90_real(var_0, var_1, var_2, var_3)
        end program main
    