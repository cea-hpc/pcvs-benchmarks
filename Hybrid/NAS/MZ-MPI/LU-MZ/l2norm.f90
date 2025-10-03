
!---------------------------------------------------------------------
!---------------------------------------------------------------------
      subroutine l2norm (v, sum, nx1, nx, nxmax, ny, nz)
!---------------------------------------------------------------------
!---------------------------------------------------------------------

!---------------------------------------------------------------------
!   to compute the l2-norm of vector v.
!---------------------------------------------------------------------

      use lu_data
      use mpinpb

      implicit none

!---------------------------------------------------------------------
!  input parameters
!---------------------------------------------------------------------
      integer nx, nxmax, ny, nz, nx1
      double precision  v(5,-1:nxmax+2,ny,nz), sum(5)

!---------------------------------------------------------------------
!  local variables
!---------------------------------------------------------------------
      integer i, j, k, m
      double precision  sum_loc(5)


      do m = 1, 5
         sum(m) = 0.0d+00
      end do

!$OMP PARALLEL DEFAULT(SHARED) PRIVATE(sum_loc,m,i,j,k)
      do m=1,5
         sum_loc(m)=0.0d0
      enddo
      do k = 2, nz-1
!$OMP DO SCHEDULE(STATIC)
         do j = 2, ny-1
            do i = ist, iend
               do m = 1, 5
                  sum_loc(m) = sum_loc(m) + v(m,i,j,k) * v(m,i,j,k)
               end do
            end do
         end do
!$OMP END DO nowait
      end do
      do m=1,5
!$OMP ATOMIC
         sum(m)=sum(m)+sum_loc(m)
      enddo
!$OMP END PARALLEL

      call mpi_allreduce(MPI_IN_PLACE, sum, 5, dp_type, MPI_SUM,  &
     &                   comm_ipart, ierror)

      do m = 1, 5
         sum(m) = dsqrt(sum(m) / (dble(nz-2)*dble(ny-2)*dble(nx1-2)))
      end do

      return
      end
