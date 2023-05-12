
        
        program main
        use mpi
        
        INTEGER var_0
       INTEGER var_1
       INTEGER var_2
       INTEGER var_3
       CHARACTER*(10) var_4
       INTEGER var_5
       INTEGER var_6
       INTEGER var_7
       INTEGER var_8
        call mpi_intercomm_create_from_groups(var_0, var_1, var_2, var_3, var_4, var_5, var_6, &
       var_7, var_8)
       call pmpi_intercomm_create_from_groups(var_0, var_1, var_2, var_3, var_4, var_5, var_6, &
       var_7, var_8)
        end program main
    