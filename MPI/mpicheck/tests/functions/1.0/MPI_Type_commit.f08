
        
        program main
        use mpi_f08
        
        TYPE(MPI_Datatype) :: var_0
       INTEGER :: var_1
        call mpi_type_commit(var_0, var_1)
       call pmpi_type_commit(var_0, var_1)
        end program main
    