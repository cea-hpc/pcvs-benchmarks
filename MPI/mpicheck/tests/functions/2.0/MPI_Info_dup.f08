
        
        program main
        use mpi_f08
        
        TYPE(MPI_Info) :: var_0
       TYPE(MPI_Info) :: var_1
       INTEGER :: var_2
        call mpi_info_dup(var_0, var_1, var_2)
       call pmpi_info_dup(var_0, var_1, var_2)
        end program main
    