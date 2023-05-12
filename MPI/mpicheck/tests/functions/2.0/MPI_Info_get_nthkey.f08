
        
        program main
        use mpi_f08
        
        TYPE(MPI_Info) :: var_0
       INTEGER :: var_1
       CHARACTER(10) :: var_2
       INTEGER :: var_3
        call mpi_info_get_nthkey(var_0, var_1, var_2, var_3)
       call pmpi_info_get_nthkey(var_0, var_1, var_2, var_3)
        end program main
    