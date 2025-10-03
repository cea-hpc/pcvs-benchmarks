!---------------------------------------------------------------------
!---------------------------------------------------------------------

      subroutine sync_init( ny, iam, mthreadnum, isync )

!---------------------------------------------------------------------
!   Initialize synchronization variables
!---------------------------------------------------------------------

      implicit none

      integer ny
      integer isync(0:ny), mthreadnum, iam

!---------------------------------------------------------------------
!---------------------------------------------------------------------

!$    integer, external :: omp_get_thread_num
!$    integer, external :: omp_get_num_threads

      mthreadnum = 0
!$    mthreadnum = omp_get_num_threads() - 1
      if (mthreadnum .gt. ny) mthreadnum = ny
      iam = 0
!$    iam = omp_get_thread_num()
      if (iam.le.mthreadnum) isync(iam) = 0

      return
      end

!---------------------------------------------------------------------
!---------------------------------------------------------------------

      subroutine sync_left( nxmax, ny, nz, v,  &
     &                      iam, mthreadnum, isync )

!---------------------------------------------------------------------
!   Thread synchronization for pipeline operation
!---------------------------------------------------------------------

      implicit none

      integer nxmax, ny, nz
      double precision  v(5, nxmax, ny, nz)

      integer isync(0:ny), mthreadnum, iam

!---------------------------------------------------------------------
!---------------------------------------------------------------------

      integer neigh, iv


      if (iam .gt. 0 .and. iam .le. mthreadnum) then
         neigh = iam - 1
!$omp atomic read
         iv = isync(neigh)
         do while (iv .eq. 0)
!$omp atomic read
            iv = isync(neigh)
         end do
!$omp atomic write
         isync(neigh) = 0
      endif
!$omp flush(isync,v)


      return
      end

!---------------------------------------------------------------------
!---------------------------------------------------------------------

      subroutine sync_right( nxmax, ny, nz, v,  &
     &                       iam, mthreadnum, isync )

!---------------------------------------------------------------------
!   Thread synchronization for pipeline operation
!---------------------------------------------------------------------

      implicit none

      integer nxmax, ny, nz
      double precision  v(5, nxmax, ny, nz)

      integer isync(0:ny), mthreadnum, iam

!---------------------------------------------------------------------
!---------------------------------------------------------------------

      integer iv


!$omp flush(isync,v)
      if (iam .lt. mthreadnum) then
!$omp atomic read
         iv = isync(iam)
         do while (iv .eq. 1)
!$omp atomic read
            iv = isync(iam)
         end do
!$omp atomic write
         isync(iam) = 1
      endif


      return
      end
