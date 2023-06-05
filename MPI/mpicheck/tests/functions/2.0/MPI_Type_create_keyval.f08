
        
        program main
        use mpi_f08
        
            INTERFACE
            SUBROUTINE MPI_Type_copy_attr_function_def95(oldtype, type_keyval, extra_state, attribute_val_in, &
                    & attribute_val_out, flag, ierror)
                    import MPI_Datatype, MPI_ADDRESS_KIND
                     TYPE(MPI_Datatype) :: oldtype
                     INTEGER :: type_keyval, ierror
                     INTEGER(KIND=MPI_ADDRESS_KIND) :: extra_state, attribute_val_in, attribute_val_out
                     LOGICAL :: flag
            END SUBROUTINE
            END INTERFACE

            INTERFACE
            SUBROUTINE MPI_Type_delete_attr_function_def87(type, type_keyval, attribute_val, extra_state, ierror)
                    import MPI_Datatype, MPI_ADDRESS_KIND
                    TYPE(MPI_Datatype) :: type
                    INTEGER :: type_keyval, ierror
                    INTEGER(KIND=MPI_ADDRESS_KIND) :: attribute_val, extra_state
            END SUBROUTINE
            END INTERFACE
        PROCEDURE(MPI_Type_copy_attr_function_def95), POINTER :: var_0
       PROCEDURE(MPI_Type_delete_attr_function_def87), POINTER :: var_1
       INTEGER :: var_2
       INTEGER(KIND=MPI_ADDRESS_KIND) :: var_3
       INTEGER :: var_4
        call mpi_type_create_keyval(var_0, var_1, var_2, var_3, var_4)
       call pmpi_type_create_keyval(var_0, var_1, var_2, var_3, var_4)
        end program main
    