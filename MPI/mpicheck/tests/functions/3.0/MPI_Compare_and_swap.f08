
        
        program main
        use mpi_f08
        
        TYPE(INTEGER), DIMENSION(10) :: var_0
       TYPE(INTEGER), DIMENSION(10) :: var_1
       TYPE(INTEGER), DIMENSION(10) :: var_2
       TYPE(MPI_Datatype) :: var_3
       INTEGER :: var_4
       INTEGER(KIND=MPI_ADDRESS_KIND) :: var_5
       TYPE(MPI_Win) :: var_6
       INTEGER :: var_7
        call mpi_compare_and_swap(var_0, var_1, var_2, var_3, var_4, var_5, var_6, var_7)
       call pmpi_compare_and_swap(var_0, var_1, var_2, var_3, var_4, var_5, var_6, var_7)
        end program main
    