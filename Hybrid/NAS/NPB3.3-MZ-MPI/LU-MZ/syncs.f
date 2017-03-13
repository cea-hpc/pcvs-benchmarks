c---------------------------------------------------------------------
c---------------------------------------------------------------------

      subroutine sync_left( nxmax, ny, nz, v,
     &                      iam, mthreadnum, isync )

c---------------------------------------------------------------------
c   Thread synchronization for pipeline operation
c---------------------------------------------------------------------

      implicit none

      integer nxmax, ny, nz
      double precision  v(5, nxmax, ny, nz)

      integer isync(0:ny), mthreadnum, iam

c---------------------------------------------------------------------
c---------------------------------------------------------------------

      integer neigh


      if (iam .gt. 0 .and. iam .le. mthreadnum) then
         neigh = iam - 1
         do while (isync(neigh) .eq. 0)
!$OMP FLUSH(isync)
         end do
         isync(neigh) = 0
!$OMP FLUSH(isync,v)
      endif


      return
      end

c---------------------------------------------------------------------
c---------------------------------------------------------------------

      subroutine sync_right( nxmax, ny, nz, v,
     &                       iam, mthreadnum, isync )

c---------------------------------------------------------------------
c   Thread synchronization for pipeline operation
c---------------------------------------------------------------------

      implicit none

      integer nxmax, ny, nz
      double precision  v(5, nxmax, ny, nz)

      integer isync(0:ny), mthreadnum, iam

c---------------------------------------------------------------------
c---------------------------------------------------------------------


      if (iam .lt. mthreadnum) then
!$omp flush(isync,v)
         do while (isync(iam) .eq. 1)
!$omp flush(isync)
         end do
         isync(iam) = 1
!$omp flush(isync)
      endif


      return
      end
