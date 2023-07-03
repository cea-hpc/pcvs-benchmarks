
        
        program main
        use mpi_f08
        
            INTERFACE
            SUBROUTINE MPI_User_function_def98(invec, inoutvec, len, datatype)
                    import MPI_Datatype, C_PTR
                    TYPE(C_PTR), VALUE :: invec, inoutvec
                    INTEGER :: len
                    TYPE(MPI_Datatype) :: datatype
            END SUBROUTINE
            END INTERFACE
        PROCEDURE(MPI_User_function_def98), POINTER :: var_0
       LOGICAL :: var_1
       TYPE(MPI_Op) :: var_2
       INTEGER :: var_3
        call mpi_op_create(var_0, var_1, var_2, var_3)
       call pmpi_op_create(var_0, var_1, var_2, var_3)
        end program main
    