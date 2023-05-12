
        
        program main
        use mpi_f08
        
        CHARACTER(10) :: var_0
       TYPE(MPI_Info) :: var_1
       CHARACTER(10) :: var_2
       INTEGER :: var_3
        call mpi_publish_name(var_0, var_1, var_2, var_3)
       call pmpi_publish_name(var_0, var_1, var_2, var_3)
        end program main
    