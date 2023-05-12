
        
        program main
        use mpi_f08
        
        TYPE(MPI_Group) :: var_0
       INTEGER :: var_1
       INTEGER, DIMENSION(10,3) :: var_2
       TYPE(MPI_Group) :: var_3
       INTEGER :: var_4
        call mpi_group_range_excl(var_0, var_1, var_2, var_3, var_4)
       call pmpi_group_range_excl(var_0, var_1, var_2, var_3, var_4)
        end program main
    