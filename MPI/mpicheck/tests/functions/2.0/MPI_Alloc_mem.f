
        
        program main
        include 'mpif.h'
        
        INTEGER(KIND=MPI_ADDRESS_KIND) var_0
       INTEGER var_1
       INTEGER(KIND=MPI_ADDRESS_KIND) var_2
       INTEGER var_3
        call mpi_alloc_mem(var_0, var_1, var_2, var_3)
       call pmpi_alloc_mem(var_0, var_1, var_2, var_3)
        end program main
    