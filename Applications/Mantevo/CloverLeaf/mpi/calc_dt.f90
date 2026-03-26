!Crown Copyright 2012 AWE.
!
! This file is part of CloverLeaf.
!
! CloverLeaf is free software: you can redistribute it and/or modify it under 
! the terms of the GNU General Public License as published by the 
! Free Software Foundation, either version 3 of the License, or (at your option) 
! any later version.
!
! CloverLeaf is distributed in the hope that it will be useful, but 
! WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or 
! FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public License for more 
! details.
!
! You should have received a copy of the GNU General Public License along with 
! CloverLeaf. If not, see http://www.gnu.org/licenses/.

!>  @brief Driver for the timestep kernels
!>  @author Wayne Gaudin
!>  @details Invokes the user specified timestep kernel.

MODULE calc_dt_module

CONTAINS

  SUBROUTINE calc_dt(tile,local_dt,local_control,xl_pos,yl_pos,jldt,kldt)

    USE clover_module
    USE calc_dt_kernel_module

    IMPLICIT NONE

    INTEGER          :: tile
    REAL(KIND=8)     :: local_dt
    CHARACTER(LEN=8) :: local_control
    REAL(KIND=8)     :: xl_pos,yl_pos
    INTEGER          :: jldt,kldt

    INTEGER          :: l_control
    INTEGER          :: small

    local_dt=g_big


    small = 0


    IF(use_fortran_kernels)THEN
      CALL calc_dt_kernel(chunk%tiles(tile)%t_xmin,     &
        chunk%tiles(tile)%t_xmax,     &
        chunk%tiles(tile)%t_ymin,     &
        chunk%tiles(tile)%t_ymax,     &
        g_small,                       &
        g_big,                         &
        dtmin,                         &
        dtc_safe,                      &
        dtu_safe,                      &
        dtv_safe,                      &
        dtdiv_safe,                    &
        chunk%tiles(tile)%field%xarea,     &
        chunk%tiles(tile)%field%yarea,     &
        chunk%tiles(tile)%field%cellx,     &
        chunk%tiles(tile)%field%celly,     &
        chunk%tiles(tile)%field%celldx,    &
        chunk%tiles(tile)%field%celldy,    &
        chunk%tiles(tile)%field%volume,    &
        chunk%tiles(tile)%field%density0,  &
        chunk%tiles(tile)%field%energy0,   &
        chunk%tiles(tile)%field%pressure,  &
        chunk%tiles(tile)%field%viscosity, &
        chunk%tiles(tile)%field%soundspeed,&
        chunk%tiles(tile)%field%xvel0,     &
        chunk%tiles(tile)%field%yvel0,     &
        chunk%tiles(tile)%field%work_array1,&
        local_dt,                      &
        l_control,                     &
        xl_pos,                        &
        yl_pos,                        &
        jldt,                          &
        kldt,                          &
        small                          )
    ELSEIF(use_C_kernels)THEN
      CALL calc_dt_kernel_c(chunk%tiles(tile)%t_xmin,     &
        chunk%tiles(tile)%t_xmax,     &
        chunk%tiles(tile)%t_ymin,     &
        chunk%tiles(tile)%t_ymax,     &
        g_small,                       &
        g_big,                         &
        dtmin,                         &
        dtc_safe,                      &
        dtu_safe,                      &
        dtv_safe,                      &
        dtdiv_safe,                    &
        chunk%tiles(tile)%field%xarea,     &
        chunk%tiles(tile)%field%yarea,     &
        chunk%tiles(tile)%field%cellx,     &
        chunk%tiles(tile)%field%celly,     &
        chunk%tiles(tile)%field%celldx,    &
        chunk%tiles(tile)%field%celldy,    &
        chunk%tiles(tile)%field%volume,    &
        chunk%tiles(tile)%field%density0,  &
        chunk%tiles(tile)%field%energy0,   &
        chunk%tiles(tile)%field%pressure,  &
        chunk%tiles(tile)%field%viscosity, &
        chunk%tiles(tile)%field%soundspeed,&
        chunk%tiles(tile)%field%xvel0,     &
        chunk%tiles(tile)%field%yvel0,     &
        chunk%tiles(tile)%field%work_array1,&
        local_dt,                      &
        l_control,                     &
        xl_pos,                        &
        yl_pos,                        &
        jldt,                          &
        kldt,                          &
        small                          )
    ENDIF


    IF(l_control.EQ.1) local_control='sound'
    IF(l_control.EQ.2) local_control='xvel'
    IF(l_control.EQ.3) local_control='yvel'
    IF(l_control.EQ.4) local_control='div'

  END SUBROUTINE calc_dt

END MODULE calc_dt_module
