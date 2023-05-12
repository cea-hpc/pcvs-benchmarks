
        
        program main
        use mpi_f08
        
        TYPE(MPI_Info) :: var_0
       TYPE(MPI_Errhandler) :: var_1
       TYPE(MPI_Session) :: var_2
       INTEGER :: var_3
        call mpi_session_init(var_0, var_1, var_2, var_3)
       call pmpi_session_init(var_0, var_1, var_2, var_3)
        end program main
    