
!---------------------------------------------------------------------
!---------------------------------------------------------------------

      subroutine exchange_1( g, nx, nxmax, ny, nz, k, jst, jend, iex )

!---------------------------------------------------------------------
!---------------------------------------------------------------------

      use lu_data
      use mpinpb

      implicit none

!---------------------------------------------------------------------
!  input parameters
!---------------------------------------------------------------------
      integer nx, nxmax, ny, nz
      double precision  g(5,-1:nxmax+2,ny,nz)
      integer k, jst, jend
      integer iex

!---------------------------------------------------------------------
!  local variables
!---------------------------------------------------------------------
      integer j
      double precision dum(5,problem_size)

      integer mid, STATUS(MPI_STATUS_SIZE)



!$OMP MASTER
      if (timeron) call timer_start(t_exch)
!$OMP END MASTER

      if( iex .eq. 0 ) then

          if( north .ne. -1 ) then
              call MPI_IRECV( dum(1,jst),  &
     &                       5*(jend-jst+1),  &
     &                       dp_type,  &
     &                       north,  &
     &                       from_n+jst,  &
     &                       comm_setup,  &
     &                       mid,  &
     &                       IERROR )
          endif

          call sync_left( nxmax, ny, nz, g )

          if( north .ne. -1 ) then
              call MPI_WAIT( mid, STATUS, IERROR )
              do j=jst,jend
                  g(1,0,j,k) = dum(1,j)
                  g(2,0,j,k) = dum(2,j)
                  g(3,0,j,k) = dum(3,j)
                  g(4,0,j,k) = dum(4,j)
                  g(5,0,j,k) = dum(5,j)
              enddo
          endif

      else if( iex .eq. 1 ) then

          if( south .ne. -1 ) then
              call MPI_IRECV( dum(1,jst),  &
     &                       5*(jend-jst+1),  &
     &                       dp_type,  &
     &                       south,  &
     &                       from_s+jst,  &
     &                       comm_setup,  &
     &                       mid,  &
     &                       IERROR )
          endif

          call sync_left( nxmax, ny, nz, g )

          if( south .ne. -1 ) then
              call MPI_WAIT( mid, STATUS, IERROR )
              do j=jst,jend
                  g(1,nx+1,j,k) = dum(1,j)
                  g(2,nx+1,j,k) = dum(2,j)
                  g(3,nx+1,j,k) = dum(3,j)
                  g(4,nx+1,j,k) = dum(4,j)
                  g(5,nx+1,j,k) = dum(5,j)
              enddo
          endif

      else if( iex .eq. 2 ) then

          if( south .ne. -1 ) then
              do j=jst,jend
                  dum(1,j) = g(1,nx,j,k) 
                  dum(2,j) = g(2,nx,j,k) 
                  dum(3,j) = g(3,nx,j,k) 
                  dum(4,j) = g(4,nx,j,k) 
                  dum(5,j) = g(5,nx,j,k) 
              enddo
              call MPI_ISEND( dum(1,jst),  &
     &                       5*(jend-jst+1),  &
     &                       dp_type,  &
     &                       south,  &
     &                       from_n+jst,  &
     &                       comm_setup,  &
     &                       mid,  &
     &                       IERROR )
          endif

          call sync_right( nxmax, ny, nz, g )

          if( south .ne. -1 ) then
              call MPI_WAIT( mid, STATUS, IERROR )
          endif

      else

          if( north .ne. -1 ) then
              do j=jst,jend
                  dum(1,j) = g(1,1,j,k)
                  dum(2,j) = g(2,1,j,k)
                  dum(3,j) = g(3,1,j,k)
                  dum(4,j) = g(4,1,j,k)
                  dum(5,j) = g(5,1,j,k)
              enddo
              call MPI_ISEND( dum(1,jst),  &
     &                       5*(jend-jst+1),  &
     &                       dp_type,  &
     &                       north,  &
     &                       from_s+jst,  &
     &                       comm_setup,  &
     &                       mid,  &
     &                       IERROR )
          endif

          call sync_right( nxmax, ny, nz, g )

          if( north .ne. -1 ) then
              call MPI_WAIT( mid, STATUS, IERROR )
          endif

      endif

!$OMP MASTER
      if (timeron) call timer_stop(t_exch)
!$OMP END MASTER

      end



