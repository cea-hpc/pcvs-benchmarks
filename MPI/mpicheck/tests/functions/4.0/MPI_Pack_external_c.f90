
        
        program main
        use mpi
        
        CHARACTER*(10) var_0
       TYPE(INTEGER) var_1(10)
       INTEGER(KIND=MPI_COUNT_KIND) var_2
       INTEGER var_3
       TYPE(INTEGER) var_4(10)
       INTEGER(KIND=MPI_COUNT_KIND) var_5
       INTEGER(KIND=MPI_COUNT_KIND) var_6
       INTEGER var_7
        call mpi_pack_external_c(var_0, var_1, var_2, var_3, var_4, var_5, var_6, var_7)
       call pmpi_pack_external_c(var_0, var_1, var_2, var_3, var_4, var_5, var_6, var_7)
        end program main
    