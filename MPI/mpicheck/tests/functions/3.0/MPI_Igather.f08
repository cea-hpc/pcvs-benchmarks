
        
        program main
        use mpi_f08
        
        TYPE(INTEGER), DIMENSION(10) :: var_0
       INTEGER :: var_1
       TYPE(MPI_Datatype) :: var_2
       TYPE(INTEGER), DIMENSION(10) :: var_3
       INTEGER :: var_4
       TYPE(MPI_Datatype) :: var_5
       INTEGER :: var_6
       TYPE(MPI_Comm) :: var_7
       TYPE(MPI_Request) :: var_8
       INTEGER :: var_9
        call mpi_igather(var_0, var_1, var_2, var_3, var_4, var_5, var_6, var_7, var_8, var_9)
       call pmpi_igather(var_0, var_1, var_2, var_3, var_4, var_5, var_6, var_7, var_8, var_9)
        end program main
    