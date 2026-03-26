! Communication Module:  Contains data structures that tell processors
!                        how to communicate with each other 
                                                                                 
module Communicator_mod 

  use kind_mod
  use constant_mod

  private

! public interfaces

  public constructBuffer, destructBuffer
                                                                                 
  type, public :: Communicator 

     integer             :: lensend
     integer             :: lenrecv

     integer,    pointer :: nsend(:)
     integer,    pointer :: nrecv(:)
     integer,    pointer :: ListRecv(:)      ! ListRecv(lenrecv)
     integer,    pointer :: ListSend(:)      ! ListSend(lensend)
     integer,    pointer :: irequest(:)      ! irequest(2)

     real(adqt), pointer :: psibsend(:)      ! psibsend(lensend) - send buffer
     real(adqt), pointer :: psibrecv(:)      ! psibrecv(lenrecv) - receive buffer

  end type Communicator 

  type(Communicator), pointer, public :: Comm

  interface constructBuffer
    module procedure Communicator_ctor_buffer
  end interface

  interface destructBuffer
    module procedure Communicator_dtor
  end interface

contains

!=======================================================================
! constructBuffer interface
!=======================================================================
                                                                                    
  subroutine Communicator_ctor_buffer(self, lensend, lenrecv, Groups, NangBin, &
                                      nsendAngle, nrecvAngle)

    implicit none

!   Passed variables
                                                                                     
    type(Communicator),    intent(inout) :: self
    integer,               intent(in)    :: lensend, lenrecv, Groups, NangBin
    integer,               intent(in)    :: nsendAngle(NangBin)
    integer,               intent(in)    :: nrecvAngle(NangBin)

    allocate( self% nsend(NangBin) )
    allocate( self% nrecv(NangBin) )

    allocate( self% ListRecv(lenrecv) )
    allocate( self% ListSend(lensend) )
    allocate( self% irequest(2) )

    allocate( self% psibsend(lensend*Groups) )
    allocate( self% psibrecv(lenrecv*Groups) )

    self% lensend          = lensend
    self% lenrecv          = lenrecv

    self% nsend(1:NangBin) = nsendAngle(1:NangBin)
    self% nrecv(1:NangBin) = nrecvAngle(1:NangBin)

    return

  end subroutine Communicator_ctor_buffer

!=======================================================================
! destruct interface
!=======================================================================
                                                                                    
  subroutine Communicator_dtor(self)

    use mpi_param_mod
    use mpif90_mod

    implicit none

!   Passed variables
                                                                                     
    type(Communicator), intent(inout)    :: self

!   Local

    integer  :: ierr

!   Free Communication Requests

    if (self% lensend > 0) then
      call MPI_Request_Free(self%irequest(1), ierr)
    endif
                                                                                    
    if (self% lenrecv > 0) then
      call MPI_Request_Free(self%irequest(2), ierr)
    endif
                                                                                    

    deallocate( self% nsend    )
    deallocate( self% nrecv    )
    deallocate( self% ListSend )
    deallocate( self% ListRecv )
    deallocate( self% irequest )
    deallocate( self% psibsend )
    deallocate( self% psibrecv )


    return

  end subroutine Communicator_dtor

end module Communicator_mod

