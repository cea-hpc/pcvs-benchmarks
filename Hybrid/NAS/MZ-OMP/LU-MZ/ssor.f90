!---------------------------------------------------------------------
!---------------------------------------------------------------------

      subroutine ssor(u, rsd, frct, qs, rho_i, tv, a, b, c, d,  &
     &                au, bu, cu, du, nx, nxmax, ny, nz, isync)

!---------------------------------------------------------------------
!---------------------------------------------------------------------

!---------------------------------------------------------------------
!   to perform pseudo-time stepping SSOR iterations
!   for five nonlinear pde's.
!---------------------------------------------------------------------

      use lu_data
      implicit none

      integer          nx, nxmax, ny, nz, isync(0:problem_size)
      double precision u(5,nxmax,ny,nz), rsd(5,nxmax,ny,nz),  &
     &                 frct(5,nxmax,ny,nz), qs(nxmax,ny,nz),  &
     &                 rho_i(nxmax,ny,nz), tv(5,2:nxmax-1,ny),  &
     &                 a (5,5,2:nxmax-1,ny), b (5,5,2:nxmax-1,ny),  &
     &                 c (5,5,2:nxmax-1,ny), d (5,5,2:nxmax-1,ny),  &
     &                 au(5,5,2:nxmax-1,ny), bu(5,5,2:nxmax-1,ny),  &
     &                 cu(5,5,2:nxmax-1,ny), du(5,5,2:nxmax-1,ny)

!---------------------------------------------------------------------
!  local variables
!---------------------------------------------------------------------
      integer mthreadnum, iam
      integer i, j, k, m
      double precision  tmp
      external timer_read
      double precision timer_read

 
!$OMP PARALLEL DEFAULT(SHARED) PRIVATE(m,i,j,k,tmp,mthreadnum,iam)
!$OMP MASTER
      if (timeron) call timer_start(t_rhs)
!$OMP END MASTER
      do k = 2, nz-1
!$OMP DO SCHEDULE(STATIC)
         do j = 2, ny-1
            do i = 2, nx-1
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


      call sync_init( problem_size, iam, mthreadnum, isync )
!$OMP BARRIER


      do k = 2, nz-1 
!---------------------------------------------------------------------
!   form the lower triangular part of the jacobian matrix
!---------------------------------------------------------------------
!$OMP MASTER
         if (timeron) call timer_start(t_jacld)
!$OMP END MASTER

         call jacld(k, u, rho_i, qs, a, b, c, d, nx, nxmax, ny, nz)

!$OMP MASTER
         if (timeron) call timer_stop(t_jacld)
 
!---------------------------------------------------------------------
!   perform the lower triangular solution
!---------------------------------------------------------------------
         if (timeron) call timer_start(t_blts)
!$OMP END MASTER

         call sync_left( nxmax, ny, nz, rsd, iam, mthreadnum, isync )

         call blts( nx, nxmax, ny, nz, k, omega, rsd, a, b, c, d)

         call sync_right( nxmax, ny, nz, rsd, iam, mthreadnum, isync )

!$OMP MASTER
         if (timeron) call timer_stop(t_blts)
!$OMP END MASTER
      end do
 
!$OMP BARRIER


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

!---------------------------------------------------------------------
!   perform the upper triangular solution
!---------------------------------------------------------------------
         if (timeron) call timer_start(t_buts)
!$OMP END MASTER

         call sync_left( nxmax, ny, nz, rsd, iam, mthreadnum, isync )

         call buts( nx, nxmax, ny, nz, k, omega, rsd, tv,  &
     &              du, au, bu, cu)

         call sync_right( nxmax, ny, nz, rsd, iam, mthreadnum, isync )

!$OMP MASTER
         if (timeron) call timer_stop(t_buts)
!$OMP END MASTER
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
            do i = 2, nx-1
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
