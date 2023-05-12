
        
        program main
        use mpi_f08
        
        TYPE(MPI_Win) :: var_0
       TYPE(INTEGER), DIMENSION(10) :: var_1
       INTEGER(KIND=MPI_ADDRESS_KIND) :: var_2
       INTEGER :: var_3
        call mpi_win_attach(var_0, var_1, var_2, var_3)
       call pmpi_win_attach(var_0, var_1, var_2, var_3)
        end program main
    