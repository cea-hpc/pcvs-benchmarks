
        
        program main
        use mpi_f08
        
        TYPE(MPI_Datatype) :: var_0
       INTEGER :: var_1
       INTEGER :: var_2
       INTEGER :: var_4
       INTEGER :: var_5
       INTEGER :: var_6
        call mpi_type_get_envelope(var_0, var_1, var_2, var_4, var_5, var_6)
       call pmpi_type_get_envelope(var_0, var_1, var_2, var_4, var_5, var_6)
        end program main
    