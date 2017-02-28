!     $Id: iot_types.f,v 1.1 1997/11/18 13:23:49 djl Exp $
!
!     IOT: MPI IO Test Suite
!
!     Copyright Fujitsu Ltd. 1997
!
!     Function:
!     Create MPI datatypes
!     Release 1.0
!
!     Description:
!     Hide error checking etc when creating 
!       datatypes in main program. Keep the MPI names.
!
!     Interface:
!     use iot_types
!
!     External components:
!     iot_defs, iot_err
!-------------------------------------------------------------------

      module iot_types

      contains

!-----------------------------------------------------------------------    
!     Contains the following subroutines:
!
!     create_contig_type
!     create_vector_type
!     create_hvector_type
!     create_submat2D_type
!     create_submat3D_type
!-----------------------------------------------------------------------    

!-----------------------------------------------------------------------    
!     create_contig_type
!       contiguous data type
!-----------------------------------------------------------------------    

      subroutine create_contig_type(node, count, etype, contigtype)
      use iot_defs
      use iot_err
      implicit none

      integer :: node
      integer :: count
      integer :: etype ! basic element type
      integer :: contigtype
      integer :: ierr
      call mpi_type_contiguous(count, etype, contigtype, ierr)
      if (ierr /= MPI_SUCCESS) then
         call iotMPIerr(node, ierr, "Error in create_contig_type")
         return
      end if
      call mpi_type_commit(contigtype, ierr)
      if (ierr /= MPI_SUCCESS) then
         call iotMPIerr(node, ierr, "Error in create_contig_type commit")
         return
      end if

      end subroutine create_contig_type


!-----------------------------------------------------------------------    
!     create_vector_type
!       vector data type
!-----------------------------------------------------------------------    

      subroutine create_vector_type(node, count, blocklength, &
        stride, etype, vectortype)
      use iot_defs
      use iot_err
      implicit none

      integer :: node
      integer :: count, blocklength, stride
      integer :: etype ! basic element type
      integer :: vectortype
      integer :: ierr
      call mpi_type_vector(count, blocklength, stride ,etype, &
        vectortype, ierr)
      if (ierr /= MPI_SUCCESS) then
         call iotMPIerr(node, ierr, "Error in create_vector_type")
         return
      end if
      call mpi_type_commit(vectortype, ierr)
      if (ierr /= MPI_SUCCESS) then
         call iotMPIerr(node, ierr, "Error in create_vector_type commit")
         return
      end if

      end subroutine create_vector_type


!-----------------------------------------------------------------------    
!     create_hvector_type
!       hvector data type
!-----------------------------------------------------------------------    

      subroutine create_hvector_type(node, count, blocklength, &
        stride, etype, hvectortype)
      use iot_defs
      use iot_err
      implicit none

      integer :: node
      integer :: count, blocklength, stride
      integer :: etype ! basic element type
      integer :: hvectortype
      integer :: ierr

!    Should use new MPI 2 routine
      call mpi_type_hvector(count, blocklength, stride ,etype, &
        hvectortype, ierr)
      if (ierr /= MPI_SUCCESS) then
         call iotMPIerr(node, ierr, "Error in create_hvector_type")
         return
      end if
      call mpi_type_commit(hvectortype, ierr)
      if (ierr /= MPI_SUCCESS) then
         call iotMPIerr(node, ierr, "Error in create_hvector_type commit")
         return
      end if

      end subroutine create_hvector_type


!-----------------------------------------------------------------------    
!     create_submat2D_type
!       subarray data type in 2 dimensions
!-----------------------------------------------------------------------    

      subroutine create_submat2D_type(pNx, pNy, xproc, yproc, &
         node, etype, submattype)
      use iot_defs
      use iot_err
      implicit none

      integer :: pNx, pNy, node
      integer :: xproc, yproc
      integer :: etype ! basic element type
      integer :: submattype
      integer :: size(2), subsize(2), start(2)
      integer :: ierr

      size(1) = pNy * yproc
      size(2) = pNx * xproc
      subsize(1) = pNy
      subsize(2) = pNx
      start(1) = (node/xproc) * pNy
      start(2) = modulo(node,xproc) * pNx
      call mpi_type_create_subarray(2, size, subsize, start, &
        MPI_ORDER_FORTRAN, etype, submattype, ierr)
      if (ierr /= MPI_SUCCESS) then
         call iotMPIerr(node, ierr, "Error in create_submat2D_type")
         return
      end if
      call mpi_type_commit(submattype, ierr)
      if (ierr /= MPI_SUCCESS) then
         call iotMPIerr(node, ierr, "Error in create_submat2D_type commit")
      end if

      end subroutine create_submat2D_type




!-----------------------------------------------------------------------    
!     create_submat3D_type
!       subarray data type in 3 dimensions
!-----------------------------------------------------------------------    

      subroutine create_submat3D_type(pNx, pNy, pNz,  &
         xproc, yproc, zproc, &
         node, etype, submattype)
      use iot_defs
      use iot_err
      implicit none

      integer :: pNx, pNy, pNz, node
      integer :: xproc, yproc, zproc
      integer :: etype ! basic element type
      integer :: submattype
      integer :: size(3), subsize(3), start(3)
      integer :: ierr

      size(1) = pNz * zproc
      size(2) = pNy * yproc
      size(3) = pNx * xproc
      subsize(1) = pNz
      subsize(2) = pNy
      subsize(3) = pNx
      start(1) = modulo(node,zproc) * pNz
      start(2) = modulo((node/zproc),yproc) * pNy
      start(3) = (node/(yproc*zproc)) * pNx
      call mpi_type_create_subarray(3, size, subsize, start, &
        MPI_ORDER_FORTRAN, etype, submattype, ierr)
      if (ierr /= MPI_SUCCESS) then
         call iotMPIerr(node, ierr, "Error in create_submat3D_type")
         return
      end if
      call mpi_type_commit(submattype, ierr)
      if (ierr /= MPI_SUCCESS) then
         call iotMPIerr(node, ierr, "Error in create_submat3D_type commit")
      end if

      end subroutine create_submat3D_type



!-----------------------------------------------------------------------

      end module iot_types






