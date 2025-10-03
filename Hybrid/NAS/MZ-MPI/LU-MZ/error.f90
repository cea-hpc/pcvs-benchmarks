!---------------------------------------------------------------------
!---------------------------------------------------------------------

      subroutine error(u, errnm, nx1, nx, nxmax, ny, nz)

!---------------------------------------------------------------------
!---------------------------------------------------------------------

!---------------------------------------------------------------------
!
!   compute the solution error
!
!---------------------------------------------------------------------

      use lu_data
      use mpinpb

      implicit none

      integer nx, nxmax, ny, nz, nx1
      double precision u(5,-1:nxmax+2,ny,nz), errnm(5)

!---------------------------------------------------------------------
!  local variables
!---------------------------------------------------------------------
      integer i, j, k, m
      double precision  tmp
      double precision  u000ijk(5)
      double precision  errnm_loc(5)

      do m = 1, 5
         errnm(m) = 0.0d0
      end do

!$OMP PARALLEL DEFAULT(SHARED) PRIVATE(errnm_loc,tmp,m,u000ijk,i,j,k)
      do m=1,5
         errnm_loc(m)=0.0d0
      enddo
      do k = 2, nz-1
!$OMP DO SCHEDULE(STATIC)
         do j = 2, ny-1
            do i = ist, iend
               call exact( i+ipt, j, k, u000ijk, nx1, ny, nz )
               do m = 1, 5
                  tmp = ( u000ijk(m) - u(m,i,j,k) )
                  errnm_loc(m) = errnm_loc(m) + tmp * tmp
               end do
            end do
         end do
!$OMP END DO nowait
      end do
      do m=1,5
!$OMP ATOMIC
         errnm(m)=errnm(m)+errnm_loc(m)
      enddo
!$OMP END PARALLEL

      call mpi_allreduce(MPI_IN_PLACE, errnm, 5, dp_type, MPI_SUM,  &
     &                   comm_ipart, ierror)

      do m = 1, 5
         errnm(m) = dsqrt(errnm(m) / (dble(nz-2)*(ny-2)*(nx1-2)))
      end do

      return
      end
