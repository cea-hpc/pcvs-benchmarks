
        
        program main
        use mpi
        
        TYPE(INTEGER) var_0(10)
       INTEGER var_1
       INTEGER var_2
       INTEGER var_3
       INTEGER var_4
       INTEGER var_5
        call mpi_imrecv(var_0, var_1, var_2, var_3, var_4, var_5)
       call pmpi_imrecv(var_0, var_1, var_2, var_3, var_4, var_5)
        end program main
    