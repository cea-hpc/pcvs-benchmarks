
        
        program main
        use mpi_f08
        
        TYPE(MPI_Status) :: var_0
       TYPE(MPI_Datatype) :: var_1
       INTEGER :: var_2
       INTEGER :: var_3
        call mpi_status_set_elements(var_0, var_1, var_2, var_3)
       call pmpi_status_set_elements(var_0, var_1, var_2, var_3)
        end program main
    