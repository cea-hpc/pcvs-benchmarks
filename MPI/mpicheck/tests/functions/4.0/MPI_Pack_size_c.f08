
        
        program main
        use mpi_f08
        
        INTEGER(KIND=MPI_COUNT_KIND) :: var_0
       TYPE(MPI_Datatype) :: var_1
       TYPE(MPI_Comm) :: var_2
       INTEGER(KIND=MPI_COUNT_KIND) :: var_3
       INTEGER :: var_4
        call mpi_pack_size(var_0, var_1, var_2, var_3, var_4)
       call pmpi_pack_size(var_0, var_1, var_2, var_3, var_4)
        end program main
    