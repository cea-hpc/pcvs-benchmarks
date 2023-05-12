
        
        program main
        use mpi
        
        TYPE(INTEGER) var_0(10)
       INTEGER var_1
       INTEGER var_2
       INTEGER var_3
       INTEGER var_4
       TYPE(INTEGER) var_5(10)
       INTEGER var_6
       INTEGER var_7
       INTEGER var_8
       INTEGER var_9
       INTEGER var_10
       INTEGER var_11(10)
       INTEGER var_12
        call mpi_sendrecv(var_0, var_1, var_2, var_3, var_4, var_5, var_6, var_7, var_8, var_9, &
       var_10, var_11, var_12)
       call pmpi_sendrecv(var_0, var_1, var_2, var_3, var_4, var_5, var_6, var_7, var_8, var_9, &
       var_10, var_11, var_12)
        end program main
    