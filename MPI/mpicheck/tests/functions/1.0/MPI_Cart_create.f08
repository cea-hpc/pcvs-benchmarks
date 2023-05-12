
        
        program main
        use mpi_f08
        
        TYPE(MPI_Comm) :: var_0
       INTEGER :: var_1
       INTEGER, DIMENSION(10) :: var_2
       LOGICAL, DIMENSION(10) :: var_3
       LOGICAL :: var_4
       TYPE(MPI_Comm) :: var_5
       INTEGER :: var_6
        call mpi_cart_create(var_0, var_1, var_2, var_3, var_4, var_5, var_6)
       call pmpi_cart_create(var_0, var_1, var_2, var_3, var_4, var_5, var_6)
        end program main
    