
        
        program main
        use mpi_f08
        
        TYPE(MPI_Session) :: var_0
       TYPE(MPI_Info) :: var_1
       INTEGER :: var_2
       INTEGER :: var_3
        call mpi_session_get_num_psets(var_0, var_1, var_2, var_3)
       call pmpi_session_get_num_psets(var_0, var_1, var_2, var_3)
        end program main
    