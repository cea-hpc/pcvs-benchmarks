      integer mpi_comm_world
      parameter (mpi_comm_world = 0)

      integer mpi_max, mpi_min, mpi_sum
      parameter (mpi_max = 1, mpi_sum = 2, mpi_min = 3)

      integer mpi_byte, mpi_integer, mpi_real, mpi_logical,  &
     &                  mpi_double_precision,  mpi_complex,  &
     &                  mpi_double_complex, mpi_char, mpi_real8
      parameter (mpi_double_precision = 1,  &
     &           mpi_integer = 2,  &
     &           mpi_byte = 3,  &
     &           mpi_real= 4,  &
     &           mpi_logical = 5,  &
     &           mpi_complex = 6,  &
     &           mpi_double_complex = 7,  &
     &           mpi_char = 3,  &
     &           mpi_real8 = 1)

      integer mpi_any_source
      parameter (mpi_any_source = -1)

      integer mpi_err_other
      parameter (mpi_err_other = -1)

      double precision mpi_wtime
      external mpi_wtime

      integer mpi_status_size
      parameter (mpi_status_size=3)

      integer mpi_thread_single, mpi_thread_serialized,  &
     &        mpi_thread_funneled, mpi_thread_multiple
      parameter ( mpi_thread_single = 1,  &
     &            mpi_thread_serialized = 2,  &
     &            mpi_thread_funneled = 3,  &
     &            mpi_thread_multiple = 4)

      integer mpi_bottom, mpi_in_place, mpi_status_ignore
      common /mpipriv/ mpi_bottom, mpi_in_place, mpi_status_ignore
