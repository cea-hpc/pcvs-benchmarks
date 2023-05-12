
        
        program main
        use mpi_f08
        
        CHARACTER(10) :: var_0
       TYPE(MPI_Info) :: var_1
       INTEGER :: var_2
        call mpi_file_delete(var_0, var_1, var_2)
       call pmpi_file_delete(var_0, var_1, var_2)
        end program main
    