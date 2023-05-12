
        
        program main
        use mpi_f08
        
        TYPE(INTEGER), DIMENSION(10) :: var_0
       TYPE(INTEGER), DIMENSION(10) :: var_1
       INTEGER(KIND=MPI_COUNT_KIND) :: var_2
       TYPE(MPI_Datatype) :: var_3
       TYPE(MPI_Op) :: var_4
       TYPE(MPI_Comm) :: var_5
       INTEGER :: var_6
        call mpi_exscan_c(var_0, var_1, var_2, var_3, var_4, var_5, var_6)
       call pmpi_exscan_c(var_0, var_1, var_2, var_3, var_4, var_5, var_6)
        end program main
    