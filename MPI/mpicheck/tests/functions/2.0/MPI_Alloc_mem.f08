
        
        program main
        use mpi_f08
        
        INTEGER(KIND=MPI_ADDRESS_KIND) :: var_0
       TYPE(MPI_Info) :: var_1
       TYPE(C_PTR) :: var_2
       INTEGER :: var_3
        call mpi_alloc_mem(var_0, var_1, var_2, var_3)
       call pmpi_alloc_mem(var_0, var_1, var_2, var_3)
        end program main
    