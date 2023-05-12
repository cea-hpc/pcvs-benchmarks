
        
        program main
        use mpi_f08
        
            INTERFACE
            SUBROUTINE MPI_Comm_errhandler_function_def9(comm, error_code)
                    import MPI_Comm
                    TYPE(MPI_Comm) :: comm
                    INTEGER :: error_code
            END SUBROUTINE
            END INTERFACE
        PROCEDURE(MPI_Comm_errhandler_function_def9), POINTER :: var_0
       TYPE(MPI_Errhandler) :: var_1
       INTEGER :: var_2
        call mpi_comm_create_errhandler(var_0, var_1, var_2)
       call pmpi_comm_create_errhandler(var_0, var_1, var_2)
        end program main
    