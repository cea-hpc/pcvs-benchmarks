
        
        program main
        use mpi_f08
        
        INTEGER :: var_0
       INTEGER :: var_1
       TYPE(MPI_Comm) :: var_2
       LOGICAL :: var_3
       TYPE(MPI_Message) :: var_4
       TYPE(MPI_Status) :: var_5
       INTEGER :: var_6
        call mpi_improbe(var_0, var_1, var_2, var_3, var_4, var_5, var_6)
       call pmpi_improbe(var_0, var_1, var_2, var_3, var_4, var_5, var_6)
        end program main
    