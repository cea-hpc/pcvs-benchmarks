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

!>  @brief Cell centred advection driver.
!>  @author Wayne Gaudin
!>  @details Invokes the user selected advection kernel.

MODULE  advec_cell_driver_module

CONTAINS

  SUBROUTINE advec_cell_driver(tile,sweep_number,dir)

    USE clover_module
    USE advec_cell_kernel_module

    IMPLICIT NONE

    INTEGER :: tile,sweep_number,dir

    IF(use_fortran_kernels)THEN
      CALL advec_cell_kernel(chunk%tiles(tile)%t_xmin, &
        chunk%tiles(tile)%t_xmax,                 &
        chunk%tiles(tile)%t_ymin,                 &
        chunk%tiles(tile)%t_ymax,                 &
        dir,                                       &
        sweep_number,                              &
        chunk%tiles(tile)%field%vertexdx,              &
        chunk%tiles(tile)%field%vertexdy,              &
        chunk%tiles(tile)%field%volume,                &
        chunk%tiles(tile)%field%density1,              &
        chunk%tiles(tile)%field%energy1,               &
        chunk%tiles(tile)%field%mass_flux_x,           &
        chunk%tiles(tile)%field%vol_flux_x,            &
        chunk%tiles(tile)%field%mass_flux_y,           &
        chunk%tiles(tile)%field%vol_flux_y,            &
        chunk%tiles(tile)%field%work_array1,           &
        chunk%tiles(tile)%field%work_array2,           &
        chunk%tiles(tile)%field%work_array3,           &
        chunk%tiles(tile)%field%work_array4,           &
        chunk%tiles(tile)%field%work_array5,           &
        chunk%tiles(tile)%field%work_array6,           &
        chunk%tiles(tile)%field%work_array7            )
    ELSEIF(use_C_kernels)THEN
      CALL advec_cell_kernel_c(chunk%tiles(tile)%t_xmin,    &
        chunk%tiles(tile)%t_xmax,                 &
        chunk%tiles(tile)%t_ymin,                 &
        chunk%tiles(tile)%t_ymax,                 &
        dir,                                       &
        sweep_number,                              &
        chunk%tiles(tile)%field%vertexdx,              &
        chunk%tiles(tile)%field%vertexdy,              &
        chunk%tiles(tile)%field%volume,                &
        chunk%tiles(tile)%field%density1,              &
        chunk%tiles(tile)%field%energy1,               &
        chunk%tiles(tile)%field%mass_flux_x,           &
        chunk%tiles(tile)%field%vol_flux_x,            &
        chunk%tiles(tile)%field%mass_flux_y,           &
        chunk%tiles(tile)%field%vol_flux_y,            &
        chunk%tiles(tile)%field%work_array1,           &
        chunk%tiles(tile)%field%work_array2,           &
        chunk%tiles(tile)%field%work_array3,           &
        chunk%tiles(tile)%field%work_array4,           &
        chunk%tiles(tile)%field%work_array5,           &
        chunk%tiles(tile)%field%work_array6,           &
        chunk%tiles(tile)%field%work_array7            )
    ENDIF


  END SUBROUTINE advec_cell_driver

END MODULE  advec_cell_driver_module

