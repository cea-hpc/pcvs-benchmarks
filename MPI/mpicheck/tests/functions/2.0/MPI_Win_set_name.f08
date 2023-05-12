
        
        program main
        use mpi_f08
        
        TYPE(MPI_Win) :: var_0
       CHARACTER(10) :: var_1
       INTEGER :: var_2
        call mpi_win_set_name(var_0, var_1, var_2)
       call pmpi_win_set_name(var_0, var_1, var_2)
        end program main
    