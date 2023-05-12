
        
        program main
        use mpi_f08
        
        TYPE(INTEGER), DIMENSION(10) :: var_0
       INTEGER :: var_1
       TYPE(MPI_Datatype) :: var_2
       INTEGER :: var_3
       INTEGER :: var_4
       TYPE(MPI_Comm) :: var_5
       TYPE(MPI_Request) :: var_6
       INTEGER :: var_7
        call mpi_irsend(var_0, var_1, var_2, var_3, var_4, var_5, var_6, var_7)
       call pmpi_irsend(var_0, var_1, var_2, var_3, var_4, var_5, var_6, var_7)
        end program main
    