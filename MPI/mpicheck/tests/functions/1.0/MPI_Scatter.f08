
        
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
       INTEGER :: var_8
        call mpi_scatter(var_0, var_1, var_2, var_3, var_4, var_5, var_6, var_7, var_8)
       call pmpi_scatter(var_0, var_1, var_2, var_3, var_4, var_5, var_6, var_7, var_8)
        end program main
    