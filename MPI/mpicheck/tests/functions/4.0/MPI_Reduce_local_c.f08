
        
        program main
        use mpi_f08
        
        TYPE(INTEGER), DIMENSION(10) :: var_0
       TYPE(INTEGER), DIMENSION(10) :: var_1
       INTEGER(KIND=MPI_COUNT_KIND) :: var_2
       TYPE(MPI_Datatype) :: var_3
       TYPE(MPI_Op) :: var_4
       INTEGER :: var_5
        call mpi_reduce_local(var_0, var_1, var_2, var_3, var_4, var_5)
       call pmpi_reduce_local(var_0, var_1, var_2, var_3, var_4, var_5)
        end program main
    