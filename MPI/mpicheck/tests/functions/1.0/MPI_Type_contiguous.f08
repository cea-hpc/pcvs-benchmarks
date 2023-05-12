
        
        program main
        use mpi_f08
        
        INTEGER :: var_0
       TYPE(MPI_Datatype) :: var_1
       TYPE(MPI_Datatype) :: var_2
       INTEGER :: var_3
        call mpi_type_contiguous(var_0, var_1, var_2, var_3)
       call pmpi_type_contiguous(var_0, var_1, var_2, var_3)
        end program main
    