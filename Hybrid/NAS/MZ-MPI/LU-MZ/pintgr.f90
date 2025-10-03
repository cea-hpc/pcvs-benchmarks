
!---------------------------------------------------------------------
!---------------------------------------------------------------------

      subroutine pintgr(u, phi1, phi2, frc, nx, nxmax, ny, nz)

!---------------------------------------------------------------------
!---------------------------------------------------------------------

      use lu_data
      use mpinpb

      implicit none

      integer          nx, nxmax, ny, nz
      double precision u(5,-1:nxmax+2,ny,nz), frc,  &
     &                 phi1(problem_size+1,problem_size),  &
     &                 phi2(problem_size+1,problem_size)

!---------------------------------------------------------------------
!  local variables
!---------------------------------------------------------------------
      integer i, j, k
      double precision  frc1, frc2, frc3
      double precision  dummy
      integer iend1

!---------------------------------------------------------------------
!   initialize
!---------------------------------------------------------------------
      iend1 = nx
      if (south.eq.-1) iend1 = nx - 2

!$OMP PARALLEL DEFAULT(SHARED) PRIVATE(k,i,j)
!$OMP DO
      do j = 1,ny
        do i = 1,nx
          phi1(i,j) = 0.d0
          phi2(i,j) = 0.d0
        end do
      end do
!$OMP END DO

!$OMP DO
      do j = 2,ny-2
         do i = ist,iend

            k = 3

            phi1(i,j) = c2*(u(5,i,j,k) -  &
     &             0.5d0 * (u(2,i,j,k)**2 + u(3,i,j,k)**2 +  &
     &                      u(4,i,j,k)**2) / u(1,i,j,k) )

            k = nz-1

            phi2(i,j) = c2*(u(5,i,j,k) -  &
     &             0.5d0 * (u(2,i,j,k)**2 + u(3,i,j,k)**2 +  &
     &                      u(4,i,j,k)**2) / u(1,i,j,k) )
         end do
      end do
!$OMP END DO

!$OMP SINGLE
!---------------------------------------------------------------------
!  communicate in i direction
!---------------------------------------------------------------------
      call exchange_4(phi1,phi2,nx,ny)

      frc1 = 0.0d0
!$OMP END SINGLE

!$OMP DO REDUCTION(+:frc1)
      do j = 2,ny-3
         do i = ist, iend1
            frc1 = frc1 + (phi1(i,j)   + phi1(i+1,j)   +  &
     &                     phi1(i,j+1) + phi1(i+1,j+1) +  &
     &                     phi2(i,j)   + phi2(i+1,j)   +  &
     &                     phi2(i,j+1) + phi2(i+1,j+1))
         end do
      end do
!$OMP END DO nowait
!$OMP END PARALLEL

!---------------------------------------------------------------------
!  compute the global sum of individual contributions to frc1
!---------------------------------------------------------------------
      dummy = frc1
      call MPI_ALLREDUCE( dummy,  &
     &                    frc1,  &
     &                    1,  &
     &                    dp_type,  &
     &                    MPI_SUM,  &
     &                    comm_ipart,  &
     &                    IERROR )

      frc1 = dxi * deta * frc1

!---------------------------------------------------------------------
!   initialize
!---------------------------------------------------------------------
!$OMP PARALLEL DEFAULT(SHARED) PRIVATE(i,k)
!$OMP DO
      do k = 1,nz
        do i = 1,nx
          phi1(i,k) = 0.
          phi2(i,k) = 0.
        end do
      end do
!$OMP END DO

!$OMP DO
      do k = 3, nz-1
         do i = ist, iend
            phi1(i,k) = c2*(u(5,i,2,k) -  &
     &             0.5d0 * (u(2,i,2,k)**2 + u(3,i,2,k)**2 +  &
     &                      u(4,i,2,k)**2 ) / u(1,i,2,k) )
         end do
      end do
!$OMP END DO nowait

!$OMP DO
      do k = 3, nz-1
         do i = ist, iend
            phi2(i,k) = c2*(u(5,i,ny-2,k) -  &
     &             0.5d0 * (u(2,i,ny-2,k)**2 + u(3,i,ny-2,k)**2 +  &
     &                      u(4,i,ny-2,k)**2 ) / u(1,i,ny-2,k) )
         end do
      end do
!$OMP END DO


!$OMP SINGLE
!---------------------------------------------------------------------
!  communicate in i direction
!---------------------------------------------------------------------
      call exchange_4(phi1,phi2,nx,nz)

      frc2 = 0.0d0
!$OMP END SINGLE

!$OMP DO REDUCTION(+:frc2)
      do k = 3, nz-2
         do i = ist, iend1
            frc2 = frc2 + (phi1(i,k)   + phi1(i+1,k)   +  &
     &                     phi1(i,k+1) + phi1(i+1,k+1) +  &
     &                     phi2(i,k)   + phi2(i+1,k)   +  &
     &                     phi2(i,k+1) + phi2(i+1,k+1))
         end do
      end do
!$OMP END DO nowait
!$OMP END PARALLEL


!---------------------------------------------------------------------
!  compute the global sum of individual contributions to frc1
!---------------------------------------------------------------------
      dummy = frc2
      call MPI_ALLREDUCE( dummy,  &
     &                    frc2,  &
     &                    1,  &
     &                    dp_type,  &
     &                    MPI_SUM,  &
     &                    comm_ipart,  &
     &                    IERROR )

      frc2 = dxi * dzeta * frc2

!---------------------------------------------------------------------
!   initialize
!---------------------------------------------------------------------
!$OMP PARALLEL DEFAULT(SHARED) PRIVATE(j,k)
!$OMP DO
      do k = 1,nz
        do j = 1,ny
          phi1(j,k) = 0.d0
          phi2(j,k) = 0.d0
        end do
      end do
!$OMP END DO

      if (north.eq.-1) then
!$OMP DO
      do k = 3, nz-1
         do j = 2, ny-2
            phi1(j,k) = c2*(u(5,2,j,k) -  &
     &             0.5d0 * (u(2,2,j,k)**2 + u(3,2,j,k)**2 +  &
     &                      u(4,2,j,k)**2)   / u(1,2,j,k) )
         end do
      end do
!$OMP END DO nowait
      endif

      if (south.eq.-1) then
!$OMP DO
      do k = 3, nz-1
         do j = 2, ny-2
            phi2(j,k) = c2*(u(5,nx-1,j,k) -  &
     &             0.5d0 * (u(2,nx-1,j,k)**2 + u(3,nx-1,j,k)**2 +  &
     &                      u(4,nx-1,j,k)**2)  / u(1,nx-1,j,k) )
         end do
      end do
!$OMP END DO nowait
      endif

!$OMP SINGLE
      frc3 = 0.0d0
!$OMP END SINGLE

!$OMP DO REDUCTION(+:frc3)
      do k = 3, nz-2
         do j = 2, ny-3
            frc3 = frc3 + (phi1(j,k)   + phi1(j+1,k)   +  &
     &                     phi1(j,k+1) + phi1(j+1,k+1) +  &
     &                     phi2(j,k)   + phi2(j+1,k)   +  &
     &                     phi2(j,k+1) + phi2(j+1,k+1))
         end do
      end do
!$OMP END DO nowait
!$OMP END PARALLEL


!---------------------------------------------------------------------
!  compute the global sum of individual contributions to frc3
!---------------------------------------------------------------------
      dummy = frc3
      call MPI_ALLREDUCE( dummy,  &
     &                    frc3,  &
     &                    1,  &
     &                    dp_type,  &
     &                    MPI_SUM,  &
     &                    comm_ipart,  &
     &                    IERROR )

      frc3 = deta * dzeta * frc3
      frc = 0.25d0 * ( frc1 + frc2 + frc3 )

      return
      end
