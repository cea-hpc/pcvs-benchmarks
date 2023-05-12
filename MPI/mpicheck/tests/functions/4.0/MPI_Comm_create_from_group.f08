
        
        program main
        use mpi_f08
        
        TYPE(MPI_Group) :: var_0
       CHARACTER(10) :: var_1
       TYPE(MPI_Info) :: var_2
       TYPE(MPI_Errhandler) :: var_3
       TYPE(MPI_Comm) :: var_4
       INTEGER :: var_5
        call mpi_comm_create_from_group(var_0, var_1, var_2, var_3, var_4, var_5)
       call pmpi_comm_create_from_group(var_0, var_1, var_2, var_3, var_4, var_5)
        end program main
    