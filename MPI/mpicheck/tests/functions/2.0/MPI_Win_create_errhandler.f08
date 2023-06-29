
        
        program main
        use mpi_f08
        
            INTERFACE
            SUBROUTINE MPI_Win_errhandler_function_def67(comm, error_code)
                    import MPI_Win
                    TYPE(MPI_Win) :: comm
                    INTEGER :: error_code
            END SUBROUTINE
            END INTERFACE
        PROCEDURE(MPI_Win_errhandler_function_def67), POINTER :: var_0
       TYPE(MPI_Errhandler) :: var_1
       INTEGER :: var_2
        call mpi_win_create_errhandler(var_0, var_1, var_2)
       call pmpi_win_create_errhandler(var_0, var_1, var_2)
        end program main
    