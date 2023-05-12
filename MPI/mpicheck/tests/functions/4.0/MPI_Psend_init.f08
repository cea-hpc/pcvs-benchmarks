
        
        program main
        use mpi_f08
        
        TYPE(INTEGER), DIMENSION(10) :: var_0
       INTEGER :: var_1
       INTEGER(KIND=MPI_COUNT_KIND) :: var_2
       TYPE(MPI_Datatype) :: var_3
       INTEGER :: var_4
       INTEGER :: var_5
       TYPE(MPI_Comm) :: var_6
       TYPE(MPI_Info) :: var_7
       TYPE(MPI_Request) :: var_8
       INTEGER :: var_9
        call mpi_psend_init(var_0, var_1, var_2, var_3, var_4, var_5, var_6, var_7, var_8, var_9)
       call pmpi_psend_init(var_0, var_1, var_2, var_3, var_4, var_5, var_6, var_7, var_8, var_9)
        end program main
    