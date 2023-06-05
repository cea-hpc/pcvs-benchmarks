
        
        program main
        use mpi_f08
        
            INTERFACE
            SUBROUTINE MPI_File_errhandler_function_def80(comm, error_code)
                    import MPI_File
                    TYPE(MPI_File) :: comm
                    INTEGER :: error_code
            END SUBROUTINE
            END INTERFACE
        PROCEDURE(MPI_File_errhandler_function_def80), POINTER :: var_0
       TYPE(MPI_Errhandler) :: var_1
       INTEGER :: var_2
        call mpi_file_create_errhandler(var_0, var_1, var_2)
       call pmpi_file_create_errhandler(var_0, var_1, var_2)
        end program main
    