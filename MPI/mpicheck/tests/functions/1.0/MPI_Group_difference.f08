
        
        program main
        use mpi_f08
        
        TYPE(MPI_Group) :: var_0
       TYPE(MPI_Group) :: var_1
       TYPE(MPI_Group) :: var_2
       INTEGER :: var_3
        call mpi_group_difference(var_0, var_1, var_2, var_3)
       call pmpi_group_difference(var_0, var_1, var_2, var_3)
        end program main
    