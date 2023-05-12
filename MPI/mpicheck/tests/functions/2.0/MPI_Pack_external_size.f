
        
        program main
        include 'mpif.h'
        
        CHARACTER*(10) var_0
       INTEGER var_1
       INTEGER var_2
       INTEGER(KIND=MPI_ADDRESS_KIND) var_3
       INTEGER var_4
        call mpi_pack_external_size(var_0, var_1, var_2, var_3, var_4)
       call pmpi_pack_external_size(var_0, var_1, var_2, var_3, var_4)
        end program main
    