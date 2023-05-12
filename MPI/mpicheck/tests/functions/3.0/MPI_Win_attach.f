
        
        program main
        include 'mpif.h'
        
        INTEGER var_0
       TYPE(INTEGER) var_1(10)
       INTEGER(KIND=MPI_ADDRESS_KIND) var_2
       INTEGER var_3
        call mpi_win_attach(var_0, var_1, var_2, var_3)
       call pmpi_win_attach(var_0, var_1, var_2, var_3)
        end program main
    