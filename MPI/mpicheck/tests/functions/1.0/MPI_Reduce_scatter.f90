
        
        program main
        use mpi
        
        TYPE(INTEGER) var_0(10)
       TYPE(INTEGER) var_1(10)
       INTEGER var_2(10)
       INTEGER var_3
       INTEGER var_4
       INTEGER var_5
       INTEGER var_6
        call mpi_reduce_scatter(var_0, var_1, var_2, var_3, var_4, var_5, var_6)
       call pmpi_reduce_scatter(var_0, var_1, var_2, var_3, var_4, var_5, var_6)
        end program main
    