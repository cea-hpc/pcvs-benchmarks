
        
        program main
        use mpi_f08
        
        TYPE(MPI_Win) :: var_0
       INTEGER :: var_1
       INTEGER(KIND=MPI_ADDRESS_KIND) :: var_2
       INTEGER :: var_3
       TYPE(C_PTR) :: var_4
       INTEGER :: var_5
        call mpi_win_shared_query(var_0, var_1, var_2, var_3, var_4, var_5)
       call pmpi_win_shared_query(var_0, var_1, var_2, var_3, var_4, var_5)
        end program main
    