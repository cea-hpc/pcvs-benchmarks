
        
        program main
        use mpi
        
        INTEGER var_0
       INTEGER var_1
       INTEGER(KIND=MPI_ADDRESS_KIND) var_2
       INTEGER var_3
        call mpi_win_set_attr(var_0, var_1, var_2, var_3)
       call pmpi_win_set_attr(var_0, var_1, var_2, var_3)
        end program main
    