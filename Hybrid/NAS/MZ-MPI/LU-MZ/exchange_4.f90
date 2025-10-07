
!---------------------------------------------------------------------
!---------------------------------------------------------------------

      subroutine exchange_4(g,h,nx,ny)

!---------------------------------------------------------------------
!---------------------------------------------------------------------

      use lu_data
      use mpinpb

      implicit none

!---------------------------------------------------------------------
!  input parameters
!---------------------------------------------------------------------
      double precision  g(problem_size+1,problem_size),  &
     &        h(problem_size+1,problem_size)
      integer nx, ny

!---------------------------------------------------------------------
!  local variables
!---------------------------------------------------------------------
      integer j
      integer ny1
      double precision  dum(2*problem_size)

      integer msgid1
      integer STATUS(MPI_STATUS_SIZE)



      ny1 = ny - 1

!---------------------------------------------------------------------
!   communicate in the south and north directions
!---------------------------------------------------------------------

!---------------------------------------------------------------------
!   receive from south
!---------------------------------------------------------------------
      if (south .ne. -1) then
        call MPI_IRECV( dum,  &
     &                  2*ny1,  &
     &                  dp_type,  &
     &                  south,  &
     &                  from_s,  &
     &                  comm_setup,  &
     &                  msgid1,  &
     &                  IERROR )

        call MPI_WAIT( msgid1, STATUS, IERROR )

        do j = 1,ny1
          g(nx+1,j) = dum(j)
          h(nx+1,j) = dum(j+ny1)
        end do

      end if

!---------------------------------------------------------------------
!   send north
!---------------------------------------------------------------------
      if (north .ne. -1) then
        do j = 1,ny1
          dum(j) = g(1,j)
          dum(j+ny1) = h(1,j)
        end do

        call MPI_SEND( dum,  &
     &                 2*ny1,  &
     &                 dp_type,  &
     &                 north,  &
     &                 from_s,  &
     &                 comm_setup,  &
     &                 IERROR )

      end if

      return
      end     
