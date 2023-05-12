
        
        program main
        use mpi_f08
        
        TYPE(MPI_Group) :: var_0
       INTEGER :: var_1
        call mpi_group_free(var_0, var_1)
       call pmpi_group_free(var_0, var_1)
        end program main
    