
        
        program main
        use mpi_f08
        
            INTERFACE
            SUBROUTINE MPI_Grequest_query_function_def86(extra_state, status, ierror)
                    import MPI_ADDRESS_KIND, MPI_Status
                    INTEGER(KIND=MPI_ADDRESS_KIND) :: extra_state
                    TYPE(MPI_Status) :: status
                    INTEGER :: ierror
            END SUBROUTINE
            END INTERFACE

            INTERFACE
            SUBROUTINE MPI_Grequest_free_function_def28(extra_state, ierror)
                    IMPORT MPI_ADDRESS_KIND
                    INTEGER(KIND=MPI_ADDRESS_KIND) :: extra_state
                    INTEGER :: ierror
            END SUBROUTINE
            END INTERFACE

            INTERFACE
            SUBROUTINE MPI_Grequest_cancel_function_def84(extra_state, complete, ierror)
                    IMPORT MPI_ADDRESS_KIND
                   INTEGER(KIND=MPI_ADDRESS_KIND) :: extra_state
                    LOGICAL :: complete
                    INTEGER :: ierror
            END SUBROUTINE
            END INTERFACE
        PROCEDURE(MPI_Grequest_query_function_def86), POINTER :: var_0
       PROCEDURE(MPI_Grequest_free_function_def28), POINTER :: var_1
       PROCEDURE(MPI_Grequest_cancel_function_def84), POINTER :: var_2
       INTEGER(KIND=MPI_ADDRESS_KIND) :: var_3
       TYPE(MPI_Request) :: var_4
       INTEGER :: var_5
        call mpi_grequest_start(var_0, var_1, var_2, var_3, var_4, var_5)
       call pmpi_grequest_start(var_0, var_1, var_2, var_3, var_4, var_5)
        end program main
    