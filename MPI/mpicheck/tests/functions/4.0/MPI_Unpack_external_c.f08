
        
        program main
        use mpi_f08
        
        CHARACTER(10) :: var_0
       TYPE(INTEGER), DIMENSION(10) :: var_1
       INTEGER(KIND=MPI_COUNT_KIND) :: var_2
       INTEGER(KIND=MPI_COUNT_KIND) :: var_3
       TYPE(INTEGER), DIMENSION(10) :: var_4
       INTEGER(KIND=MPI_COUNT_KIND) :: var_5
       TYPE(MPI_Datatype) :: var_6
       INTEGER :: var_7
        call mpi_unpack_external(var_0, var_1, var_2, var_3, var_4, var_5, var_6, var_7)
       call pmpi_unpack_external(var_0, var_1, var_2, var_3, var_4, var_5, var_6, var_7)
        end program main
    