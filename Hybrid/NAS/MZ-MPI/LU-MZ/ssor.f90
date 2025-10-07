!---------------------------------------------------------------------
!---------------------------------------------------------------------

      subroutine ssor(u, rsd, frct, qs, rho_i, a, b, c, d,  &
     &                au, bu, cu, du, nx, nxmax, ny, nz)

!---------------------------------------------------------------------
!---------------------------------------------------------------------

!---------------------------------------------------------------------
!   to perform pseudo-time stepping SSOR iterations
!   for five nonlinear pde's.
!---------------------------------------------------------------------

      use lu_data
      use mpinpb

      implicit none

      integer          nx, nxmax, ny, nz
      double precision u(5,-1:nxmax+2,ny,nz), rsd(5,-1:nxmax+2,ny,nz),  &
     &                 frct(5,nxmax,ny,nz), qs(0:nxmax+1,ny,nz),  &
     &                 rho_i(0:nxmax+1,ny,nz),  &
     &                 a (5,5,nxmax,ny), b (5,5,nxmax,ny),  &
     &                 c (5,5,nxmax,ny), d (5,5,nxmax,ny),  &
     &                 au(5,5,nxmax,ny), bu(5,5,nxmax,ny),  &
     &                 cu(5,5,nxmax,ny), du(5,5,nxmax,ny)

!---------------------------------------------------------------------
!  local variables
!---------------------------------------------------------------------
      integer i, j, k, m
      integer jst, jend, iex
      double precision  tmp, tv(5,problem_size,problem_size)
      external timer_read
      double precision timer_read

 
!$OMP PARALLEL DEFAULT(SHARED) PRIVATE(m,i,j,k,tmp,jst,jend,iex)
!$OMP MASTER
      if (timeron) call timer_start(t_rhs)
!$OMP END MASTER
      do k = 2, nz-1
!$OMP DO SCHEDULE(STATIC)
         do j = 2, ny-1
            do i = ist, iend
               do m = 1, 5
                  rsd(m,i,j,k) = dt * rsd(m,i,j,k)
               end do
            end do
         end do
!$OMP END DO nowait
      end do
!$OMP MASTER
      if (timeron) call timer_stop(t_rhs)
!$OMP END MASTER

      call sync_init
!$OMP BARRIER


      jst = ny
      jend = 1
!$OMP DO SCHEDULE(STATIC)
      do j = 2, ny-1
         jst = min(j, jst)
         jend = max(j, jend)
      end do
!$OMP END DO nowait

      do k = 2, nz-1 
!---------------------------------------------------------------------
!   form the lower triangular part of the jacobian matrix
!---------------------------------------------------------------------
!$OMP MASTER
         if (timeron) call timer_start(t_jacld)
!$OMP END MASTER
         call jacld(k, u, rho_i, qs, a, b, c, d,  &
     &              nx, nxmax, ny, nz)
!$OMP MASTER
         if (timeron) call timer_stop(t_jacld)
!$OMP END MASTER
 
!---------------------------------------------------------------------
!   perform the lower triangular solution
!---------------------------------------------------------------------
         iex = 0
         call exchange_1( rsd, nx, nxmax, ny, nz, k, jst, jend, iex )

!$OMP MASTER
         if (timeron) call timer_start(t_blts)
!$OMP END MASTER
         call blts( nx, nxmax, ny, nz, k, ist, iend, omega, rsd,  &
     &              a, b, c, d)
!$OMP MASTER
         if (timeron) call timer_stop(t_blts)
!$OMP END MASTER

         iex = 2
         call exchange_1( rsd, nx, nxmax, ny, nz, k, jst, jend, iex )
      end do
 
!$OMP BARRIER


      jst = ny
      jend = 1
!$OMP DO SCHEDULE(STATIC)
      do j = ny-1, 2, -1
         jst = min(j, jst)
         jend = max(j, jend)
      end do
!$OMP END DO nowait

      do k = nz-1, 2, -1
!---------------------------------------------------------------------
!   form the strictly upper triangular part of the jacobian matrix
!---------------------------------------------------------------------
!$OMP MASTER
         if (timeron) call timer_start(t_jacu)
!$OMP END MASTER
         call jacu(k, u, rho_i, qs, au, bu, cu, du,  &
     &             nx, nxmax, ny, nz)
!$OMP MASTER
         if (timeron) call timer_stop(t_jacu)
!$OMP END MASTER

!---------------------------------------------------------------------
!   perform the upper triangular solution
!---------------------------------------------------------------------
         iex = 1
         call exchange_1( rsd, nx, nxmax, ny, nz, k, jst, jend, iex )

!$OMP MASTER
         if (timeron) call timer_start(t_buts)
!$OMP END MASTER
         call buts( nx, nxmax, ny, nz, k, ist, iend, omega, rsd, tv,  &
     &              du, au, bu, cu)
!$OMP MASTER
         if (timeron) call timer_stop(t_buts)
!$OMP END MASTER

         iex = 3
         call exchange_1( rsd, nx, nxmax, ny, nz, k, jst, jend, iex )
      end do

!$OMP BARRIER


!---------------------------------------------------------------------
!   update the variables
!---------------------------------------------------------------------

      tmp = 1.0d0 / ( omega * ( 2.0d0 - omega ) ) 
!$OMP MASTER
      if (timeron) call timer_start(t_add)
!$OMP END MASTER
      do k = 2, nz-1
!$OMP DO SCHEDULE(STATIC)
         do j = 2, ny-1
            do i = ist, iend
               do m = 1, 5
                  u(m,i,j,k) = u(m,i,j,k) + tmp * rsd(m,i,j,k)
               end do
            end do
         end do
!$OMP END DO nowait
      end do
!$OMP MASTER
      if (timeron) call timer_stop(t_add)
!$OMP END MASTER
!$OMP END PARALLEL

!---------------------------------------------------------------------
!   compute the steady-state residuals
!---------------------------------------------------------------------
      if (timeron) call timer_start(t_rhs)
      call rhs(u, rsd, frct, qs, rho_i, nx, nxmax, ny, nz)
      if (timeron) call timer_stop(t_rhs)

      return
      end
