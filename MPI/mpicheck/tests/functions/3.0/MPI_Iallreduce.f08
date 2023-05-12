
        
        program main
        use mpi_f08
        
        TYPE(INTEGER), DIMENSION(10) :: var_0
       TYPE(INTEGER), DIMENSION(10) :: var_1
       INTEGER :: var_2
       TYPE(MPI_Datatype) :: var_3
       TYPE(MPI_Op) :: var_4
       TYPE(MPI_Comm) :: var_5
       TYPE(MPI_Request) :: var_6
       INTEGER :: var_7
        call mpi_iallreduce(var_0, var_1, var_2, var_3, var_4, var_5, var_6, var_7)
       call pmpi_iallreduce(var_0, var_1, var_2, var_3, var_4, var_5, var_6, var_7)
        end program main
    