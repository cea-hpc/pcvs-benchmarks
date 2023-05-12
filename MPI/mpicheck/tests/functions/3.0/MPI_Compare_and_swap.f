
        
        program main
        include 'mpif.h'
        
        TYPE(INTEGER) var_0(10)
       TYPE(INTEGER) var_1(10)
       TYPE(INTEGER) var_2(10)
       INTEGER var_3
       INTEGER var_4
       INTEGER(KIND=MPI_ADDRESS_KIND) var_5
       INTEGER var_6
       INTEGER var_7
        call mpi_compare_and_swap(var_0, var_1, var_2, var_3, var_4, var_5, var_6, var_7)
       call pmpi_compare_and_swap(var_0, var_1, var_2, var_3, var_4, var_5, var_6, var_7)
        end program main
    