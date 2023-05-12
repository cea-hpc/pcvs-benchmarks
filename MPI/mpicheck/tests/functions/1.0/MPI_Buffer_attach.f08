
        
        program main
        use mpi_f08
        
        TYPE(INTEGER), DIMENSION(10) :: var_0
       INTEGER :: var_1
       INTEGER :: var_2
        call mpi_buffer_attach(var_0, var_1, var_2)
       call pmpi_buffer_attach(var_0, var_1, var_2)
        end program main
    