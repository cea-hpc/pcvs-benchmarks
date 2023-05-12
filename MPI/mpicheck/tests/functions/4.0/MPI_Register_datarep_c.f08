
        
        program main
        use mpi_f08
        
            INTERFACE
            SUBROUTINE MPI_Datarep_conversion_function_def82(userbuf, datatype, count, &
                & filebuf, position, extra_state, ierror)
                import MPI_Datatype, MPI_OFFSET_KIND, MPI_ADDRESS_KIND, C_PTR
                TYPE(C_PTR), VALUE :: userbuf, filebuf
                TYPE(MPI_Datatype) :: datatype
                INTEGER :: count, ierror
                INTEGER(KIND=MPI_OFFSET_KIND) :: position
                INTEGER(KIND=MPI_ADDRESS_KIND) :: extra_state
            END SUBROUTINE
            END INTERFACE

            INTERFACE
            SUBROUTINE MPI_Datarep_conversion_function_def38(userbuf, datatype, count, &
                & filebuf, position, extra_state, ierror)
                import MPI_Datatype, MPI_OFFSET_KIND, MPI_ADDRESS_KIND, C_PTR
                TYPE(C_PTR), VALUE :: userbuf, filebuf
                TYPE(MPI_Datatype) :: datatype
                INTEGER :: count, ierror
                INTEGER(KIND=MPI_OFFSET_KIND) :: position
                INTEGER(KIND=MPI_ADDRESS_KIND) :: extra_state
            END SUBROUTINE
            END INTERFACE

            INTERFACE
            SUBROUTINE MPI_Datarep_extent_function_def18(datatype, extent, extra_state, ierror)
                    import MPI_Datatype, MPI_ADDRESS_KIND
                    TYPE(MPI_Datatype) :: datatype
                    INTEGER(KIND=MPI_ADDRESS_KIND) :: extent, extra_state
                    INTEGER :: ierror
            END SUBROUTINE
            END INTERFACE
        CHARACTER(10) :: var_0
       PROCEDURE(MPI_Datarep_conversion_function_def82), POINTER :: var_1
       PROCEDURE(MPI_Datarep_conversion_function_def38), POINTER :: var_2
       PROCEDURE(MPI_Datarep_extent_function_def18), POINTER :: var_3
       INTEGER(KIND=MPI_ADDRESS_KIND) :: var_4
       INTEGER :: var_5
        call mpi_register_datarep_c(var_0, var_1, var_2, var_3, var_4, var_5)
       call pmpi_register_datarep_c(var_0, var_1, var_2, var_3, var_4, var_5)
        end program main
    