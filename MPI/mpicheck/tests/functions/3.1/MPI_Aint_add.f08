
        
        program main
        use mpi_f08
        
        INTEGER(KIND=MPI_ADDRESS_KIND) :: var_0
       INTEGER(KIND=MPI_ADDRESS_KIND) :: var_1
       INTEGER(KIND=MPI_ADDRESS_KIND) ret
        ret = mpi_aint_add(var_0, var_1)
       ret = pmpi_aint_add(var_0, var_1)
        end program main
    