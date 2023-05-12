
        
        program main
        use mpi
        
        TYPE(INTEGER) var_0(10)
       INTEGER var_1
       INTEGER var_2
       TYPE(INTEGER) var_3(10)
       INTEGER var_4
       INTEGER var_5
       INTEGER var_6
       INTEGER var_7
        call mpi_alltoall(var_0, var_1, var_2, var_3, var_4, var_5, var_6, var_7)
       call pmpi_alltoall(var_0, var_1, var_2, var_3, var_4, var_5, var_6, var_7)
        end program main
    