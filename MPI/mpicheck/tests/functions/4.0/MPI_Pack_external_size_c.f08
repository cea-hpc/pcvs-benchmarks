
        
        program main
        use mpi_f08
        
        CHARACTER(10) :: var_0
       INTEGER(KIND=MPI_COUNT_KIND) :: var_1
       TYPE(MPI_Datatype) :: var_2
       INTEGER(KIND=MPI_COUNT_KIND) :: var_3
       INTEGER :: var_4
        call mpi_pack_external_size(var_0, var_1, var_2, var_3, var_4)
       call pmpi_pack_external_size(var_0, var_1, var_2, var_3, var_4)
        end program main
    