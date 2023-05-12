
        
        program main
        use mpi
        
        INTEGER var_0(10)
       INTEGER var_1
       INTEGER(KIND=MPI_COUNT_KIND) var_2
       INTEGER var_3
        call mpi_status_set_elements_c(var_0, var_1, var_2, var_3)
       call pmpi_status_set_elements_c(var_0, var_1, var_2, var_3)
        end program main
    