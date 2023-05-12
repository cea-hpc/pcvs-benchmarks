
        
        program main
        use mpi
        
        CHARACTER*(10) var_0
       INTEGER var_1
       INTEGER var_2
        call mpi_get_library_version(var_0, var_1, var_2)
       call pmpi_get_library_version(var_0, var_1, var_2)
        end program main
    