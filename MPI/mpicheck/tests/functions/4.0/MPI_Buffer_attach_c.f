
        
        program main
        include 'mpif.h'
        
        TYPE(INTEGER) var_0(10)
       INTEGER(KIND=MPI_COUNT_KIND) var_1
       INTEGER var_2
        call mpi_buffer_attach_c(var_0, var_1, var_2)
       call pmpi_buffer_attach_c(var_0, var_1, var_2)
        end program main
    