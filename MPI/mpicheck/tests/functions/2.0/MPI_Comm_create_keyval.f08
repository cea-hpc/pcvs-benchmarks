
        
        program main
        use mpi_f08
        
            INTERFACE
            SUBROUTINE MPI_Comm_copy_attr_function_def53(oldcomm, comm_keyval, extra_state, attribute_val_in, &
                    & attribute_val_out, flag, ierror)
                    import MPI_Comm, MPI_ADDRESS_KIND
					TYPE(MPI_Comm) :: oldcomm
					INTEGER :: comm_keyval, ierror
					INTEGER(KIND=MPI_ADDRESS_KIND) :: extra_state, attribute_val_in, attribute_val_out
					LOGICAL :: flag
            END SUBROUTINE
            END INTERFACE

            INTERFACE
            SUBROUTINE MPI_Comm_delete_attr_function_def18(comm, comm_keyval, attribute_val, extra_state, ierror)
                    import MPI_Comm, MPI_ADDRESS_KIND
                    TYPE(MPI_Comm) :: comm
                    INTEGER :: comm_keyval, ierror
                    INTEGER(KIND=MPI_ADDRESS_KIND) :: attribute_val, extra_state
            END SUBROUTINE
            END INTERFACE
        PROCEDURE(MPI_Comm_copy_attr_function_def53), POINTER :: var_0
       PROCEDURE(MPI_Comm_delete_attr_function_def18), POINTER :: var_1
       INTEGER :: var_2
       INTEGER(KIND=MPI_ADDRESS_KIND) :: var_3
       INTEGER :: var_4
        call mpi_comm_create_keyval(var_0, var_1, var_2, var_3, var_4)
       call pmpi_comm_create_keyval(var_0, var_1, var_2, var_3, var_4)
        end program main
    