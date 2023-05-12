
        
        program main
        use mpi
        
        INTEGER var_0
       INTEGER var_1
       INTEGER(KIND=MPI_ADDRESS_KIND) var_2
       LOGICAL var_3
       INTEGER var_4
        call mpi_win_get_attr(var_0, var_1, var_2, var_3, var_4)
       call pmpi_win_get_attr(var_0, var_1, var_2, var_3, var_4)
        end program main
    