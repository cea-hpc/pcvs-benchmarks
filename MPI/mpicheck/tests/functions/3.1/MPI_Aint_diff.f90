
        
        program main
        use mpi
        
        INTEGER(KIND=MPI_ADDRESS_KIND) var_0
       INTEGER(KIND=MPI_ADDRESS_KIND) var_1
       INTEGER(KIND=MPI_ADDRESS_KIND) ret
        ret = mpi_aint_diff(var_0, var_1)
       ret = pmpi_aint_diff(var_0, var_1)
        end program main
    