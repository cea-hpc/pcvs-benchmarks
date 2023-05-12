
        
        program main
        use mpi
        
        INTEGER(KIND=MPI_ADDRESS_KIND) var_0
       INTEGER(KIND=MPI_ADDRESS_KIND) var_1
       INTEGER var_2
       INTEGER var_3
       INTEGER(KIND=MPI_ADDRESS_KIND) var_4
       INTEGER var_5
       INTEGER var_6
        call mpi_win_allocate_c(var_0, var_1, var_2, var_3, var_4, var_5, var_6)
       call pmpi_win_allocate_c(var_0, var_1, var_2, var_3, var_4, var_5, var_6)
        end program main
    