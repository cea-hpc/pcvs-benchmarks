
        
        program main
        use mpi
        
        INTEGER var_0
       INTEGER var_1
       INTEGER var_2(10)
       INTEGER var_3
        call mpi_dims_create(var_0, var_1, var_2, var_3)
       call pmpi_dims_create(var_0, var_1, var_2, var_3)
        end program main
    