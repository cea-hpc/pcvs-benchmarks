
        
        program main
        use mpi_f08
        
        TYPE(MPI_Info) :: var_0
       INTEGER :: var_1
        call mpi_info_create(var_0, var_1)
       call pmpi_info_create(var_0, var_1)
        end program main
    