
        
        program main
        use mpi
        
        CHARACTER*(10) var_0
       TYPE(INTEGER) var_1(10)
       INTEGER(KIND=MPI_ADDRESS_KIND) var_2
       INTEGER(KIND=MPI_ADDRESS_KIND) var_3
       TYPE(INTEGER) var_4(10)
       INTEGER var_5
       INTEGER var_6
       INTEGER var_7
        call mpi_unpack_external(var_0, var_1, var_2, var_3, var_4, var_5, var_6, var_7)
       call pmpi_unpack_external(var_0, var_1, var_2, var_3, var_4, var_5, var_6, var_7)
        end program main
    