
        
        program main
        use mpi_f08
        
        TYPE(INTEGER), DIMENSION(10) :: var_0
       INTEGER(KIND=MPI_ADDRESS_KIND) :: var_1
       INTEGER(KIND=MPI_ADDRESS_KIND) :: var_2
       TYPE(MPI_Info) :: var_3
       TYPE(MPI_Comm) :: var_4
       TYPE(MPI_Win) :: var_5
       INTEGER :: var_6
        call mpi_win_create(var_0, var_1, var_2, var_3, var_4, var_5, var_6)
       call pmpi_win_create(var_0, var_1, var_2, var_3, var_4, var_5, var_6)
        end program main
    