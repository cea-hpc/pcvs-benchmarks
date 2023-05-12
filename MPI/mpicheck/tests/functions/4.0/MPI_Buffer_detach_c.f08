
        
        program main
        use mpi_f08
        
        TYPE(C_PTR) :: var_0
       INTEGER(KIND=MPI_COUNT_KIND) :: var_1
       INTEGER :: var_2
        call mpi_buffer_detach_c(var_0, var_1, var_2)
       call pmpi_buffer_detach_c(var_0, var_1, var_2)
        end program main
    