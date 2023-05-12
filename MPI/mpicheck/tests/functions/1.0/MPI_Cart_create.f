
        
        program main
        include 'mpif.h'
        
        INTEGER var_0
       INTEGER var_1
       INTEGER var_2(10)
       LOGICAL var_3(10)
       LOGICAL var_4
       INTEGER var_5
       INTEGER var_6
        call mpi_cart_create(var_0, var_1, var_2, var_3, var_4, var_5, var_6)
       call pmpi_cart_create(var_0, var_1, var_2, var_3, var_4, var_5, var_6)
        end program main
    