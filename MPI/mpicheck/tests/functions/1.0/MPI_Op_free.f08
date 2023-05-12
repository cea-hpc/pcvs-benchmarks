
        
        program main
        use mpi_f08
        
        TYPE(MPI_Op) :: var_0
       INTEGER :: var_1
        call mpi_op_free(var_0, var_1)
       call pmpi_op_free(var_0, var_1)
        end program main
    