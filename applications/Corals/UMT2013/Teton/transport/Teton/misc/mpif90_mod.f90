# 1 "misc/mpif90_mod.F90"
# 1 "<interne>"
# 1 "<ligne-de-commande>"
# 1 "misc/mpif90_mod.F90"
module mpif90_mod


# 1 "./include/assert.h" 1
!  Assertion checking include file for TETON

# 4 "misc/mpif90_mod.F90" 2
use kind_mod
use mpi_param_mod

!=======================================================================
!                       Version 1.1: 02/99, MRZ
!                       Version 1.0: 05/92, PFN
!-----------------------------------------------------------------------
! 1
!   This class wraps 1 functions.  This facilitates turning off 1.
!
!-----------------------------------------------------------------------
! v1.0: Original implementation
! v1.1: 1 functions wrapped in a Fortran90 class
!=======================================================================

private


# 1 "misc/mpif90.if" 1
!=======================================================================
!                       Version 1.0: 03/99, MRZ
!-----------------------------------------------------------------------
! 1 Interface File
!   This defines the interface to the 1 class.
!
!-----------------------------------------------------------------------
! v1.0: Original implementation
!=======================================================================


! public interfaces
  public MPIAllReduceT, MPIBarrierT, MPICommRank, MPICommSize, &
         MPIFinalize, MPIGather, MPIInit, &
         getMPIRankT, getMPISizeT

!=======================================================================
! MPIAllReduceT(buffer, op, comm)
!
!   Performs an 1 reduction on all nodes in the communicator.
!-----------------------------------------------------------------------
!   buffer   data buffer (integer or double precision)
!               input:  data to be reduced
!              output:  reduced data
!   op       reduction operation
!              "prod"...product reduction
!               "sum"...sum reduction
!               "min"...minimization reduction
!               "max"...maximization reduction
!   comm     1 communicator
!=======================================================================
  interface MPIAllReduceT
    module procedure mpi_MPIAllReduceT_r, &
                     mpi_MPIAllReduceT_r_, &
                     mpi_MPIAllReduceT_i, &
                     mpi_MPIAllReduceT_i_
  end interface

!=======================================================================
! MPIBarrier(comm)
!
!   Performs an 1 barrier on all nodes in the communicator
!-----------------------------------------------------------------------
!   comm   1 communicator
!=======================================================================
  interface MPIBarrierT
    module procedure mpi_MPIBarrierT
  end interface

!=======================================================================
! MPICommRank(comm, rank)
!
!   Returns the rank of the calling process in the communicator
!-----------------------------------------------------------------------
!   comm   1 communicator
!   rank   processor rank
!=======================================================================
  interface MPICommRank
    module procedure mpi_MPICommRank
  end interface

!=======================================================================
! MPICommSize(comm, size)
!
!   Returns the size of the group associated with the communicator
!-----------------------------------------------------------------------
!   comm     1 communicator
!   size     group size
!=======================================================================
  interface MPICommSize
    module procedure mpi_MPICommSize
  end interface

!=======================================================================
! MPIFinalize()
!
!   Performs an 1 finalize operation
!=======================================================================
  interface MPIFinalize
    module procedure mpi_MPIFinalize
  end interface

!=======================================================================
! MPIGather(sendBuf, recBuf, root, comm)
!
!   Performs an 1 gather operation
!-----------------------------------------------------------------------
!   sendBuf  send buffer (double precision)
!   recvBuf  receive buffer (double precision)
!   root     node to which gather is performed
!   comm     1 communicator
!=======================================================================
  interface MPIGather
    module procedure mpi_MPIGather_r_, &
                     mpi_MPIGather_r__
  end interface

!=======================================================================
! MPIInit()
!
!   Performs an 1 initialization
!=======================================================================
  interface MPIInit
    module procedure mpi_MPIInit
  end interface

!=======================================================================
! getMPIRank(comm)
!
!   Returns the rank of the calling process in the communicator
!-----------------------------------------------------------------------
!   comm        1 communicator
!   getMPIRank  processor rank
!=======================================================================
  interface getMPIRankT
    module procedure mpi_getMPIRankT
  end interface

!=======================================================================
! getMPISize(comm)
!
!   Returns the size of the group associated with the communicator
!-----------------------------------------------------------------------
!   comm        1 communicator
!   getMPISize  group size
!=======================================================================
  interface getMPISizeT
    module procedure mpi_getMPISizeT
  end interface
# 22 "misc/mpif90_mod.F90" 2

contains

!=======================================================================
! MPIAllReduceT interface
!=======================================================================

  subroutine mpi_MPIAllReduceT_r(recvBuf,mpiOp,comm)

!    This subroutine performs an 1 All Reduce operation with a
!    barrier.  The data to be broadcast, a floating point scalar, is
!    passed in as recvBuf; the reduced data is returned in recvBuf.
!
!      recvBuf  send and receive buffer
!      mpiOp    1 operation
!      comm     1 communicator

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
     


     
!      copy the send buffer into temporary storage
       sendBuf = recvBuf

!      1 Barrier is implicit for MPI_Allreduce

!      1 Reduction
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



     return
  end subroutine mpi_MPIAllReduceT_r

!-----------------------------------------------------------------------
  subroutine mpi_MPIAllReduceT_r_(recvBuf,mpiOp,comm)

!    This subroutine performs an 1 All Reduce operation with a
!    barrier.  The data to be broadcast, a floating point array, is
!    passed in as recvBuf; the reduced data is returned in recvBuf.
!
!      recvBuf  send and receive buffer
!      mpiOp    1 operation
!      comm     1 communicator

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
     



!      allocate memory for the send buffer
       allocate(sendBuf(size(recvBuf)))

!      copy the send buffer into temporary storage
       sendBuf(:) = recvBuf(:)

!      1 Barrier is implicit for MPI_Allreduce

!      1 Reduction
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




     return
  end subroutine mpi_MPIAllReduceT_r_

!-----------------------------------------------------------------------
  subroutine mpi_MPIAllReduceT_i(recvBuf,mpiOp,comm)

!    This subroutine performs an 1 All Reduce operation with a
!    barrier.  The data to be broadcast, an integer scalar, is
!    passed in as recvBuf; the reduced data is returned in recvBuf.
!
!      recvBuf  send and receive buffer
!      mpiOp    1 operation
!      comm     1 communicator

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
     



!      copy the send buffer into temporary storage
       sendBuf = recvBuf

!      1 Barrier is implicit for MPI_Allreduce

!      1 Reduction
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



     return
  end subroutine mpi_MPIAllReduceT_i

!-----------------------------------------------------------------------
  subroutine mpi_MPIAllReduceT_i_(recvBuf,mpiOp,comm)

!    This subroutine performs an 1 All Reduce operation with a
!    barrier.  The data to be broadcast, an integer array, is
!    passed in as recvBuf; the reduced data is returned in recvBuf.
!
!      recvBuf  send and receive buffer
!      mpiOp    1 operation
!      comm     1 communicator

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
     



!      allocate memory for the send buffer
       allocate(sendBuf(size(recvBuf)))

!      copy the send buffer into temporary storage
       sendBuf(:) = recvBuf(:)

!      1 Barrier is implicit for MPI_Allreduce

!      1 Reduction
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




     return
  end subroutine mpi_MPIAllReduceT_i_

!=======================================================================
! MPIBarrier interface
!=======================================================================

  subroutine mpi_MPIBarrierT(comm)

!    This subroutine performs an 1 Barrier operation.
!      comm   1 communicator

!    variable declarations
     implicit none

!    passed variables
     integer, intent(in) :: comm

!    local variables
     integer :: ierror


!    1 Barrier
     call MPI_Barrier(comm, ierror)
     if (ierror /= MPI_SUCCESS) then
        call f90fatal("MPI Barrier Failed")
     endif


     return
  end subroutine mpi_MPIBarrierT

!=======================================================================
! MPICommRank interface
!=======================================================================

  subroutine mpi_MPICommRank(comm,rank)

!    This subroutine determines the rank of the calling process in the
!    communicator.
!      rank   processor rank
!      comm   1 communicator

!    variable declarations
     implicit none

!    passed variables
     integer, intent(in)  :: comm
     integer, intent(out) :: rank

!    local variables
     integer :: ierror


!    1 Communicator Rank
     call MPI_Comm_rank(comm, rank, ierror)
     if (ierror /= MPI_SUCCESS) then
        call f90fatal("MPI Barrier Failed")
     endif




     return
  end subroutine mpi_MPICommRank

!=======================================================================
! MPICommSize interface
!=======================================================================

  subroutine mpi_MPICommSize(comm,commSize)

!    This subroutine determines the size of the group associated with
!    the communicator.
!      comm       1 communicator
!      commSize   communicator size

!    variable declarations
     implicit none

!    passed variables
     integer, intent(in)  :: comm
     integer, intent(out) :: commSize

!    local variables
     integer :: ierror


!    1 Communicator Size
     call MPI_Comm_size(comm, commSize, ierror)
     if (ierror /= MPI_SUCCESS) then
        call f90fatal("MPI Barrier Failed")
     endif




     return
  end subroutine mpi_MPICommSize

!=======================================================================
! MPIFinalize interface
!=======================================================================

  subroutine mpi_MPIFinalize()

!    This subroutine performs an 1 Finalize operation.

!    variable declarations
     implicit none

!    local variables
     integer :: ierror


!    1 Finalize
     call MPI_Finalize(ierror)
     if (ierror /= MPI_SUCCESS) then
        call f90fatal("MPI Finalize Failed")
     endif


     return
  end subroutine mpi_MPIFinalize

!=======================================================================
! MPIGather interface
!=======================================================================

  subroutine mpi_MPIGather_r_(sendBuf,recvBuf,gatherNode,comm)

!    This subroutine performs an 1 Gather operation.  The data to
!    be broadcast, a floating point array, is passed in as send Buf;
!    the gathered data is returned in recvBuf.
!
!      sendBuf      send buffer
!      recvBuf      receive buffer
!      gatherNode   node to which all data is gathered
!      comm         1 communicator

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


!    determine size and rank
     commSize = getMPISizeT(comm)
     myNode = getMPIRankT(comm)

!    assertions
     if (myNode == gatherNode) then
        
        
     endif

!    1 Barrier before performing the gather
     call MPIBarrierT(comm)

!    1 Gather
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
!       buffer, which 1 ignores.

        call MPI_Gather(sendBuf, sendCount, MPI_REAL8, &
                        recvBufDum, recvCount, MPI_REAL8, &
                        gatherNode, comm, ierror)
     endif

     if (ierror /= MPI_SUCCESS) then
        call f90fatal("MPI Reduction Failed")
     endif


     return
  end subroutine mpi_MPIGather_r_

!-----------------------------------------------------------------------
  subroutine mpi_MPIGather_r__(sendBuf,recvBuf,gatherNode,comm)

!    This subroutine performs an 1 Gather operation.  The data to
!    be broadcast, a floating point array, is passed in as send Buf;
!    the gathered data is returned in recvBuf.
!
!      sendBuf      send buffer
!      recvBuf      receive buffer
!      gatherNode   node to which all data is gathered
!      comm         1 communicator

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


!    determine size and rank
     commSize = getMPISizeT(comm)
     myNode = getMPIRankT(comm)

!    assertions
     if (myNode == gatherNode) then
        
        
        
     endif

!    1 Barrier before performing the gather
     call MPIBarrierT(comm)

!    1 Gather
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
!       buffer, which 1 ignores.

        call MPI_Gather(sendBuf, sendCount, MPI_REAL8, &
                        recvBufDum, recvCount, MPI_REAL8, &
                        gatherNode, comm, ierror)
     endif

     if (ierror /= MPI_SUCCESS) then
        call f90fatal("MPI Reduction Failed")
     endif


     return
  end subroutine mpi_MPIGather_r__

!=======================================================================
! MPIInit interface
!=======================================================================

  subroutine mpi_MPIInit()

!    This subroutine performs an 1 Initialization operation.

!    variable declarations
     implicit none

!    local variables
     integer :: ierror


!    1 Init
     call MPI_Init(ierror)
     if (ierror /= MPI_SUCCESS) then
        call f90fatal("MPI Init Failed")
     endif


     return
  end subroutine mpi_MPIInit

!=======================================================================
! getMPIRank interface
!=======================================================================

  function mpi_getMPIRankT(comm) result(MPIRank)

!    This subroutine determines the rank of the calling process in the
!    communicator.
!
!      comm      1 communicator
!      MPIRank   processor rank

!    variable declarations
     implicit none

!    passed variables
     integer, intent(in) :: comm
     integer             :: MPIRank

!    local variables
     integer :: ierror


!    1 Communicator Rank
     call MPI_Comm_rank(comm, MPIrank, ierror)
     if (ierror /= MPI_SUCCESS) then
        call f90fatal("MPI Barrier Failed")
     endif




     return
  end function mpi_getMPIRankT

!=======================================================================
! getMPISize interface
!=======================================================================

  function mpi_getMPISizeT(comm) result(MPISize)

!    This subroutine determines the size of the group associated with
!    the communicator.
!      comm      1 communicator
!      MPISize   communicator size

!    variable declarations
     implicit none

!    passed variables
     integer, intent(in) :: comm
     integer             :: MPISize

!    local variables
     integer :: ierror


!    1 Communicator Size
     call MPI_Comm_size(comm, MPISize, ierror)
     if (ierror /= MPI_SUCCESS) then
        call f90fatal("MPI Barrier Failed")
     endif




     return
  end function mpi_getMPISizeT

end module mpif90_mod
