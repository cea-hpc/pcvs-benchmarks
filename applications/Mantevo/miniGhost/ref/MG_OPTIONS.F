! ************************************************************************
!
!               miniGhost: stencil computations with boundary exchange.
!                 Copyright (2012) Sandia Corporation
!
! Under terms of Contract DE-AC04-94AL85000, there is a non-exclusive
! license for use of this work by or on behalf of the U.S. Government.
!
! This library is free software; you can redistribute it and/or modify
! it under the terms of the GNU Lesser General Public License as
! published by the Free Software Foundation; either version 2.1 of the
! License, or (at your option) any later version.
!
! This library is distributed in the hope that it will be useful, but
! WITHOUT ANY WARRANTY; without even the implied warranty of
! MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
! Lesser General Public License for more details.
!
! You should have received a copy of the GNU Lesser General Public
! License along with this library; if not, write to the Free Software
! Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307
! USA
! Questions? Contact Richard F. Barrett (rfbarre@sandia.gov) or
!                    Michael A. Heroux (maherou@sandia.gov)
!
! ************************************************************************

MODULE MG_OPTIONS_MOD

   ! Purpose
   ! =======
   ! Declaration of miniGhost options.

   USE MG_CONSTANTS_MOD

   IMPLICIT NONE

   INTEGER :: SCALING

   INTEGER, PARAMETER ::      &
      SCALING_STRONG   = 1,   &
      SCALING_WEAK     = 2           ! Default

   INTEGER :: COMM_METHOD

   INTEGER, PARAMETER ::       &

      COMM_METHOD_BSPMA = 10,  &      ! Default
      COMM_METHOD_SVAF  = 11

   INTEGER :: STENCIL

   INTEGER, PARAMETER ::          &

      STENCIL_NONE         = 20,  &
      STENCIL_2D5PT        = 21,  &  ! Default
      STENCIL_2D9PT        = 22,  &
      STENCIL_3D7PT        = 23,  &
      STENCIL_3D27PT       = 24

   INTEGER :: BC

   INTEGER, PARAMETER ::          &

      BC_DIRICHLET = 31

   INTEGER, PARAMETER ::       &

      CP_METHOD_MPIIO  = 1,  &      ! Default
      CP_METHOD_H5PART = 2


   ! ---------
   ! Variables
   ! ---------

   INTEGER ::                      &

      NX, NY, NZ,                  &  ! Local domain dimensions.

      NPX, NPY, NPZ,               &  ! Number of processes in direction X, Y, Z

      NVARS,                       &  ! Number of variables defined.
      NTSTEPS,                     &  ! Number of time steps to be executed.
      NSPIKES,                     &  ! Number of spikes to be inserted, one each NTSTEPS


      PERCENT_SUM,                 &  ! Requested percentage of variables to be globally summed.
      DEBUG_GRID,                  &  ! 1: zero domain, insert heat sourse (spike) into center.
      REPORT_DIFFUSION,            &  ! Every param.report_diffusion time steps, report error.
                                      ! Note that if in debug_grid mode, once the number of time
                                      ! steps exceeds half of the minimum global dimension, an error
                                      ! is expected to occur as the heat disapates off of the domain.
      REPORT_PERF,                 &  ! Amount of performance data reported. 0, 1, 2

      CP_METHOD,                   &
      CP_INTERVAL,                 &
      RESTART_CP_NUM

   CHARACTER*(1024) ::             &
      CP_FILE,                     &
      RESTART_FILE



END MODULE MG_OPTIONS_MOD
