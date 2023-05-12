
        
        program main
        use mpi_f08
        
        INTEGER(KIND=MPI_ADDRESS_KIND) :: var_0
       INTEGER(KIND=MPI_ADDRESS_KIND) :: var_1
       TYPE(MPI_Info) :: var_2
       TYPE(MPI_Comm) :: var_3
       TYPE(C_PTR) :: var_4
       TYPE(MPI_Win) :: var_5
       INTEGER :: var_6
        call mpi_win_allocate_shared_c(var_0, var_1, var_2, var_3, var_4, var_5, var_6)
       call pmpi_win_allocate_shared_c(var_0, var_1, var_2, var_3, var_4, var_5, var_6)
        end program main
    