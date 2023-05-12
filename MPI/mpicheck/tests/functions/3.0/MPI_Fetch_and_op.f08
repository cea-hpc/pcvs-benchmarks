
        
        program main
        use mpi_f08
        
        TYPE(INTEGER), DIMENSION(10) :: var_0
       TYPE(INTEGER), DIMENSION(10) :: var_1
       TYPE(MPI_Datatype) :: var_2
       INTEGER :: var_3
       INTEGER(KIND=MPI_ADDRESS_KIND) :: var_4
       TYPE(MPI_Op) :: var_5
       TYPE(MPI_Win) :: var_6
       INTEGER :: var_7
        call mpi_fetch_and_op(var_0, var_1, var_2, var_3, var_4, var_5, var_6, var_7)
       call pmpi_fetch_and_op(var_0, var_1, var_2, var_3, var_4, var_5, var_6, var_7)
        end program main
    