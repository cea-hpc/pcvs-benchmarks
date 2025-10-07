
!---------------------------------------------------------------------
!---------------------------------------------------------------------
      subroutine setiv(u, nx, nxmax, ny, nz)

!---------------------------------------------------------------------
!---------------------------------------------------------------------

!---------------------------------------------------------------------
!
!   set the initial values of independent variables based on tri-linear
!   interpolation of boundary values in the computational space.
!
!---------------------------------------------------------------------

      use lu_data
      implicit none

      integer nx, nxmax, ny, nz
      double precision u(5,-1:nxmax+2,ny,nz)

!---------------------------------------------------------------------
!  local variables
!---------------------------------------------------------------------
      integer i, j, k, m
      integer ig
      double precision  xi, eta, zeta
      double precision  pxi, peta, pzeta
      double precision  ue_1jk(5),ue_nx0jk(5),ue_i1k(5),  &
     &        ue_iny0k(5),ue_ij1(5),ue_ijnz(5)


!$OMP PARALLEL DEFAULT(SHARED) PRIVATE(pzeta,peta,pxi,m,ue_ijnz,  &
!$OMP& ue_ij1,ue_iny0k,ue_i1k,ue_nx0jk,ue_1jk,xi,i,eta,j,zeta,k,ig)
      do k = 2, nz - 1
         zeta = ( dble (k-1) ) / (nz-1)
!$OMP DO SCHEDULE(STATIC)
         do j = 2, ny - 1
            eta = ( dble (j-1) ) / (ny-1)
            do i = 1, nx
               ig = i + ipt
               if (ig.ne.1 .and. ig.ne.nx0) then
               xi = ( dble (ig-1) ) / (nx0-1)
               call exact(1,j,k,ue_1jk,nx0,ny,nz)
               call exact(nx0,j,k,ue_nx0jk,nx0,ny,nz)
               call exact(ig,1,k,ue_i1k,nx0,ny,nz)
               call exact(ig,ny,k,ue_iny0k,nx0,ny,nz)
               call exact(ig,j,1,ue_ij1,nx0,ny,nz)
               call exact(ig,j,nz,ue_ijnz,nx0,ny,nz)
               do m = 1, 5
                  pxi =   ( 1.0d+00 - xi ) * ue_1jk(m)  &
     &                              + xi   * ue_nx0jk(m)
                  peta =  ( 1.0d+00 - eta ) * ue_i1k(m)  &
     &                              + eta   * ue_iny0k(m)
                  pzeta = ( 1.0d+00 - zeta ) * ue_ij1(m)  &
     &                              + zeta   * ue_ijnz(m)

                  u( m, i, j, k ) = pxi + peta + pzeta  &
     &                 - pxi * peta - peta * pzeta - pzeta * pxi  &
     &                 + pxi * peta * pzeta

               end do
               end if
            end do
         end do
!$OMP END DO nowait
      end do
!$OMP END PARALLEL

      return
      end
