module mpif90_mod

#include "assert.h"
use kind_mod
use mpi_param_mod

!=======================================================================
!                       Version 1.1: 02/99, MRZ
!                       Version 1.0: 05/92, PFN
!-----------------------------------------------------------------------
! MPI
!   This class wraps MPI functions.  This facilitates turning off MPI.
!
!-----------------------------------------------------------------------
! v1.0: Original implementation
! v1.1: MPI functions wrapped in a Fortran90 class
!=======================================================================

private

#include "mpif90.if"

contains

!=======================================================================
! MPIAllReduceT interface
!=======================================================================

  subroutine mpi_MPIAllReduceT_r(recvBuf,mpiOp,comm)

!    This subroutine performs an MPI All Reduce operation with a
!    barrier.  The data to be broadcast, a floating point scalar, is
!    passed in as recvBuf; the reduced data is returned in recvBuf.
!
!      recvBuf  send and receive buffer
!      mpiOp    MPI operation
!      comm     MPI communicator

!    variable declarations
     implicit none

!    passed variables
     real(long),   intent(inout) :: recvBuf
     character(*), intent(in)    :: mpiOp
     integer,      intent(in)    :: comm

!    local variables
     integer    :: length, ierror
     real(long) :: sendBuf

     character(4), dimension(4) :: mpiOps = &
                                   (/"min ","max ","prod","sum "/)

!    assertions
     require(any(mpiOp==mpiOps(:)), "Invalid MPI Reduction Operation")

#ifdef MPI
     
!      copy the send buffer into temporary storage
       sendBuf = recvBuf

!      MPI Barrier is implicit for MPI_Allreduce

!      MPI Reduction
       length = 1
       select case (mpiOp)
       case ("min")
          call MPI_Allreduce(sendBuf, recvBuf, length, MPI_REAL8, &
            MPI_MIN, comm, ierror)
       case ("max")
          call MPI_Allreduce(sendBuf, recvBuf, length, MPI_REAL8, &
            MPI_MAX, comm, ierror)
       case ("prod")
          call MPI_Allreduce(sendBuf, recvBuf, length, MPI_REAL8, &
            MPI_PROD, comm, ierror)
       case ("sum")
          call MPI_Allreduce(sendBuf, recvBuf, length, MPI_REAL8, &
            MPI_SUM, comm, ierror)
       end select

       if (ierror /= MPI_SUCCESS) then
          call f90fatal("MPI Reduction Failed")
       endif

#endif

     return
  end subroutine mpi_MPIAllReduceT_r

!-----------------------------------------------------------------------
  subroutine mpi_MPIAllReduceT_r_(recvBuf,mpiOp,comm)

!    This subroutine performs an MPI All Reduce operation with a
!    barrier.  The data to be broadcast, a floating point array, is
!    passed in as recvBuf; the reduced data is returned in recvBuf.
!
!      recvBuf  send and receive buffer
!      mpiOp    MPI operation
!      comm     MPI communicator

!    variable declarations
     implicit none

!    passed variables
     real(long),   intent(inout) :: recvBuf(:)
     character(*), intent(in)    :: mpiOp
     integer,      intent(in)    :: comm

!    local variables
     integer                 :: length, ierror, alloc_stat
     real(long), allocatable :: sendBuf(:)

     character(4), dimension(4) :: mpiOps = &
                                   (/"min ","max ","prod","sum "/)

!    assertions
     require(any(mpiOp==mpiOps(:)), "Invalid MPI Reduction Operation")

#ifdef MPI

!      allocate memory for the send buffer
       allocate(sendBuf(size(recvBuf)))

!      copy the send buffer into temporary storage
       sendBuf(:) = recvBuf(:)

!      MPI Barrier is implicit for MPI_Allreduce

!      MPI Reduction
       length = size(recvBuf(:))
       select case (mpiOp)
       case ("min")
          call MPI_Allreduce(sendBuf, recvBuf, length, MPI_REAL8, &
            MPI_MIN, comm, ierror)
       case ("max")
          call MPI_Allreduce(sendBuf, recvBuf, length, MPI_REAL8, &
            MPI_MAX, comm, ierror)
       case ("prod")
          call MPI_Allreduce(sendBuf, recvBuf, length, MPI_REAL8, &
            MPI_PROD, comm, ierror)
       case ("sum")
          call MPI_Allreduce(sendBuf, recvBuf, length, MPI_REAL8, &
            MPI_SUM, comm, ierror)
       end select

       if (ierror /= MPI_SUCCESS) then
          call f90fatal("MPI Reduction Failed")
       endif

!      free memory
       deallocate(sendBuf, stat=alloc_stat)

#endif


     return
  end subroutine mpi_MPIAllReduceT_r_

!-----------------------------------------------------------------------
  subroutine mpi_MPIAllReduceT_i(recvBuf,mpiOp,comm)

!    This subroutine performs an MPI All Reduce operation with a
!    barrier.  The data to be broadcast, an integer scalar, is
!    passed in as recvBuf; the reduced data is returned in recvBuf.
!
!      recvBuf  send and receive buffer
!      mpiOp    MPI operation
!      comm     MPI communicator

!    variable declarations
     implicit none

!    passed variables
     integer,      intent(inout) :: recvBuf
     character(*), intent(in)    :: mpiOp
     integer,      intent(in)    :: comm

!    local variables
     integer    :: length, ierror
     integer    :: sendBuf

     character(4), dimension(4) :: mpiOps = &
                                   (/"min ","max ","prod","sum "/)

!    assertions
     require(any(mpiOp==mpiOps(:)), "Invalid MPI Reduction Operation")

#ifdef MPI

!      copy the send buffer into temporary storage
       sendBuf = recvBuf

!      MPI Barrier is implicit for MPI_Allreduce

!      MPI Reduction
       length = 1
       select case (mpiOp)
       case ("min")
          call MPI_Allreduce(sendBuf, recvBuf, length, MPI_INTEGER, &
            MPI_MIN, comm, ierror)
       case ("max")
          call MPI_Allreduce(sendBuf, recvBuf, length, MPI_INTEGER, &
            MPI_MAX, comm, ierror)
       case ("prod")
          call MPI_Allreduce(sendBuf, recvBuf, length, MPI_INTEGER, &
            MPI_PROD, comm, ierror)
       case ("sum")
          call MPI_Allreduce(sendBuf, recvBuf, length, MPI_INTEGER, &
            MPI_SUM, comm, ierror)
       end select

       if (ierror /= MPI_SUCCESS) then
          call f90fatal("MPI Reduction Failed")
       endif

#endif

     return
  end subroutine mpi_MPIAllReduceT_i

!-----------------------------------------------------------------------
  subroutine mpi_MPIAllReduceT_i_(recvBuf,mpiOp,comm)

!    This subroutine performs an MPI All Reduce operation with a
!    barrier.  The data to be broadcast, an integer array, is
!    passed in as recvBuf; the reduced data is returned in recvBuf.
!
!      recvBuf  send and receive buffer
!      mpiOp    MPI operation
!      comm     MPI communicator

!    variable declarations
     implicit none

!    passed variables
     integer,      intent(inout) :: recvBuf(:)
     character(*), intent(in)    :: mpiOp
     integer,      intent(in)    :: comm

!    local variables
     integer              :: length, ierror, alloc_stat
     integer, allocatable :: sendBuf(:)

     character(4), dimension(4) :: mpiOps = &
                                   (/"min ","max ","prod","sum "/)

!    assertions
     require(any(mpiOp==mpiOps(:)), "Invalid MPI Reduction Operation")

#ifdef MPI

!      allocate memory for the send buffer
       allocate(sendBuf(size(recvBuf)))

!      copy the send buffer into temporary storage
       sendBuf(:) = recvBuf(:)

!      MPI Barrier is implicit for MPI_Allreduce

!      MPI Reduction
       length = size(recvBuf(:))
       select case (mpiOp)
       case ("min")
          call MPI_Allreduce(sendBuf, recvBuf, length, MPI_INTEGER, &
            MPI_MIN, comm, ierror)
       case ("max")
          call MPI_Allreduce(sendBuf, recvBuf, length, MPI_INTEGER, &
            MPI_MAX, comm, ierror)
       case ("prod")
          call MPI_Allreduce(sendBuf, recvBuf, length, MPI_INTEGER, &
            MPI_PROD, comm, ierror)
       case ("sum")
          call MPI_Allreduce(sendBuf, recvBuf, length, MPI_INTEGER, &
            MPI_SUM, comm, ierror)
       end select

       if (ierror /= MPI_SUCCESS) then
          call f90fatal("MPI Reduction Failed")
       endif

!      free memory
       deallocate(sendBuf, stat=alloc_stat)

#endif


     return
  end subroutine mpi_MPIAllReduceT_i_

!=======================================================================
! MPIBarrier interface
!=======================================================================

  subroutine mpi_MPIBarrierT(comm)

!    This subroutine performs an MPI Barrier operation.
!      comm   MPI communicator

!    variable declarations
     implicit none

!    passed variables
     integer, intent(in) :: comm

!    local variables
     integer :: ierror

#ifdef MPI
!    MPI Barrier
     call MPI_Barrier(comm, ierror)
     if (ierror /= MPI_SUCCESS) then
        call f90fatal("MPI Barrier Failed")
     endif
#endif

     return
  end subroutine mpi_MPIBarrierT

!=======================================================================
! MPICommRank interface
!=======================================================================

  subroutine mpi_MPICommRank(comm,rank)

!    This subroutine determines the rank of the calling process in the
!    communicator.
!      rank   processor rank
!      comm   MPI communicator

!    variable declarations
     implicit none

!    passed variables
     integer, intent(in)  :: comm
     integer, intent(out) :: rank

!    local variables
     integer :: ierror

#ifdef MPI
!    MPI Communicator Rank
     call MPI_Comm_rank(comm, rank, ierror)
     if (ierror /= MPI_SUCCESS) then
        call f90fatal("MPI Barrier Failed")
     endif
#else
     rank = 0
#endif

     return
  end subroutine mpi_MPICommRank

!=======================================================================
! MPICommSize interface
!=======================================================================

  subroutine mpi_MPICommSize(comm,commSize)

!    This subroutine determines the size of the group associated with
!    the communicator.
!      comm       MPI communicator
!      commSize   communicator size

!    variable declarations
     implicit none

!    passed variables
     integer, intent(in)  :: comm
     integer, intent(out) :: commSize

!    local variables
     integer :: ierror

#ifdef MPI
!    MPI Communicator Size
     call MPI_Comm_size(comm, commSize, ierror)
     if (ierror /= MPI_SUCCESS) then
        call f90fatal("MPI Barrier Failed")
     endif
#else
     commSize = 1
#endif

     return
  end subroutine mpi_MPICommSize

!=======================================================================
! MPIFinalize interface
!=======================================================================

  subroutine mpi_MPIFinalize()

!    This subroutine performs an MPI Finalize operation.

!    variable declarations
     implicit none

!    local variables
     integer :: ierror

#ifdef MPI
!    MPI Finalize
     call MPI_Finalize(ierror)
     if (ierror /= MPI_SUCCESS) then
        call f90fatal("MPI Finalize Failed")
     endif
#endif

     return
  end subroutine mpi_MPIFinalize

!=======================================================================
! MPIGather interface
!=======================================================================

  subroutine mpi_MPIGather_r_(sendBuf,recvBuf,gatherNode,comm)

!    This subroutine performs an MPI Gather operation.  The data to
!    be broadcast, a floating point array, is passed in as send Buf;
!    the gathered data is returned in recvBuf.
!
!      sendBuf      send buffer
!      recvBuf      receive buffer
!      gatherNode   node to which all data is gathered
!      comm         MPI communicator

!    variable declarations
     implicit none

!    passed variables
     real(long), intent(in)    :: sendBuf(:)
     real(long), intent(inout) :: recvBuf(:,:)
     integer,    intent(in)    :: gatherNode
     integer,    intent(in)    :: comm

!    local variables
     integer    :: commSize, myNode, sendCount, recvCount, ierror
     real(long) :: recvBufDum(1,1)

#ifdef MPI
!    determine size and rank
     commSize = getMPISizeT(comm)
     myNode = getMPIRankT(comm)

!    assertions
     if (myNode == gatherNode) then
        require(size(recvBuf,1)==size(sendBuf,1), "Invalid MPI Gather")
        require(size(recvBuf,2)==commSize, "Invalid MPI Gather")
     endif

!    MPI Barrier before performing the gather
     call MPIBarrierT(comm)

!    MPI Gather
     sendCount = size(sendBuf,1)
     recvCount = sendCount

     if (myNode == gatherNode) then
!       on the gather node, perform the gather operation into the
!       allocated receive buffer

        call MPI_Gather(sendBuf, sendCount, MPI_REAL8, &
                        recvBuf, recvCount, MPI_REAL8, &
                        gatherNode, comm, ierror)
     else
!       on non-gather nodes, the receiver buffer is dereferenced due
!       to a Fortran90 copy-in/copy-out operation.  To avoid
!       dereferencing a null pointer, pass a dummy (allocated) receive
!       buffer, which MPI ignores.

        call MPI_Gather(sendBuf, sendCount, MPI_REAL8, &
                        recvBufDum, recvCount, MPI_REAL8, &
                        gatherNode, comm, ierror)
     endif

     if (ierror /= MPI_SUCCESS) then
        call f90fatal("MPI Reduction Failed")
     endif
#endif

     return
  end subroutine mpi_MPIGather_r_

!-----------------------------------------------------------------------
  subroutine mpi_MPIGather_r__(sendBuf,recvBuf,gatherNode,comm)

!    This subroutine performs an MPI Gather operation.  The data to
!    be broadcast, a floating point array, is passed in as send Buf;
!    the gathered data is returned in recvBuf.
!
!      sendBuf      send buffer
!      recvBuf      receive buffer
!      gatherNode   node to which all data is gathered
!      comm         MPI communicator

!    variable declarations
     implicit none

!    passed variables
     real(long), intent(in)    :: sendBuf(:,:)
     real(long), intent(inout) :: recvBuf(:,:,:)
     integer,    intent(in)    :: gatherNode
     integer,    intent(in)    :: comm

!    local variables
     integer    :: commSize, myNode, sendCount, recvCount, ierror
     real(long) :: recvBufDum(1,1,1)

#ifdef MPI
!    determine size and rank
     commSize = getMPISizeT(comm)
     myNode = getMPIRankT(comm)

!    assertions
     if (myNode == gatherNode) then
        require(size(recvBuf,1)==size(sendBuf,1), "Invalid MPI Gather")
        require(size(recvBuf,2)==size(sendBuf,2), "Invalid MPI Gather")
        require(size(recvBuf,3)==commSize, "Invalid MPI Gather")
     endif

!    MPI Barrier before performing the gather
     call MPIBarrierT(comm)

!    MPI Gather
     sendCount = size(sendBuf,1)*size(sendBuf,2)
     recvCount = sendCount

     if (myNode == gatherNode) then
!       on the gather node, perform the gather operation into the
!       allocated receive buffer

        call MPI_Gather(sendBuf, sendCount, MPI_REAL8, &
                        recvBuf, recvCount, MPI_REAL8, &
                        gatherNode, comm, ierror)
     else
!       on non-gather nodes, the receiver buffer is dereferenced due
!       to a Fortran90 copy-in/copy-out operation.  To avoid
!       dereferencing a null pointer, pass a dummy (allocated) receive
!       buffer, which MPI ignores.

        call MPI_Gather(sendBuf, sendCount, MPI_REAL8, &
                        recvBufDum, recvCount, MPI_REAL8, &
                        gatherNode, comm, ierror)
     endif

     if (ierror /= MPI_SUCCESS) then
        call f90fatal("MPI Reduction Failed")
     endif
#endif

     return
  end subroutine mpi_MPIGather_r__

!=======================================================================
! MPIInit interface
!=======================================================================

  subroutine mpi_MPIInit()

!    This subroutine performs an MPI Initialization operation.

!    variable declarations
     implicit none

!    local variables
     integer :: ierror

#ifdef MPI
!    MPI Init
     call MPI_Init(ierror)
     if (ierror /= MPI_SUCCESS) then
        call f90fatal("MPI Init Failed")
     endif
#endif

     return
  end subroutine mpi_MPIInit

!=======================================================================
! getMPIRank interface
!=======================================================================

  function mpi_getMPIRankT(comm) result(MPIRank)

!    This subroutine determines the rank of the calling process in the
!    communicator.
!
!      comm      MPI communicator
!      MPIRank   processor rank

!    variable declarations
     implicit none

!    passed variables
     integer, intent(in) :: comm
     integer             :: MPIRank

!    local variables
     integer :: ierror

#ifdef MPI
!    MPI Communicator Rank
     call MPI_Comm_rank(comm, MPIrank, ierror)
     if (ierror /= MPI_SUCCESS) then
        call f90fatal("MPI Barrier Failed")
     endif
#else
     MPIRank = 0
#endif

     return
  end function mpi_getMPIRankT

!=======================================================================
! getMPISize interface
!=======================================================================

  function mpi_getMPISizeT(comm) result(MPISize)

!    This subroutine determines the size of the group associated with
!    the communicator.
!      comm      MPI communicator
!      MPISize   communicator size

!    variable declarations
     implicit none

!    passed variables
     integer, intent(in) :: comm
     integer             :: MPISize

!    local variables
     integer :: ierror

#ifdef MPI
!    MPI Communicator Size
     call MPI_Comm_size(comm, MPISize, ierror)
     if (ierror /= MPI_SUCCESS) then
        call f90fatal("MPI Barrier Failed")
     endif
#else
     MPISize = 1
#endif

     return
  end function mpi_getMPISizeT

end module mpif90_mod
