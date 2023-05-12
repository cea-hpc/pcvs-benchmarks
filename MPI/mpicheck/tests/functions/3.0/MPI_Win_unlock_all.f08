
        
        program main
        use mpi_f08
        
        TYPE(MPI_Win) :: var_0
       INTEGER :: var_1
        call mpi_win_unlock_all(var_0, var_1)
       call pmpi_win_unlock_all(var_0, var_1)
        end program main
    