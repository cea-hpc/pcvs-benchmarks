
        
        program main
        use mpi_f08
        
      integer i, intsize, ans, size, rank, color
      integer maxSize
      parameter (maxSize=32)
      integer scounts(maxSize), sdispls(maxSize)
      integer rcounts(maxSize), rdispls(maxSize)
      integer sbuf(maxSize), rbuf(maxSize)

      TYPE(MPI_Datatype) :: stypes, rtypes
      TYPE(MPI_Comm) :: comm
      INTEGER :: ierr







      call mpi_alltoallw( sbuf, scounts, sdispls, stypes, rbuf, rcounts, rdispls, rtypes, comm, ierr )
        end program main
    
