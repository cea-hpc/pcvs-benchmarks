
        
        program main
        use mpi_f08
        
            INTERFACE
            SUBROUTINE MPI_Session_errhandler_function_def34(comm, error_code)
                    import MPI_Session
                    TYPE(MPI_Session) :: comm
                    INTEGER :: error_code
            END SUBROUTINE
            END INTERFACE
        PROCEDURE(MPI_Session_errhandler_function_def34), POINTER :: var_0
       TYPE(MPI_Errhandler) :: var_1
       INTEGER :: var_2
        call mpi_session_create_errhandler(var_0, var_1, var_2)
       call pmpi_session_create_errhandler(var_0, var_1, var_2)
        end program main
    