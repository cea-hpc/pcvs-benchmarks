
        
        program main
        use mpi_f08
        
        INTEGER :: var_0
       INTEGER :: var_1
       INTEGER, DIMENSION(10) :: var_2
       INTEGER :: var_3
        call mpi_dims_create(var_0, var_1, var_2, var_3)
       call pmpi_dims_create(var_0, var_1, var_2, var_3)
        end program main
    