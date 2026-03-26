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

!>  @brief Driver for the viscosity kernels
!>  @author Wayne Gaudin
!>  @details Selects the user specified kernel to caluclate the artificial 
!>  viscosity.

MODULE viscosity_module

CONTAINS

  SUBROUTINE viscosity()

    USE clover_module
    USE viscosity_kernel_module
  
    IMPLICIT NONE

    INTEGER :: tile


    IF(use_fortran_kernels) THEN
      DO tile=1,tiles_per_chunk

        CALL viscosity_kernel(chunk%tiles(tile)%t_xmin,                   &
          chunk%tiles(tile)%t_xmax,                     &
          chunk%tiles(tile)%t_ymin,                     &
          chunk%tiles(tile)%t_ymax,                     &
          chunk%tiles(tile)%field%celldx,                    &
          chunk%tiles(tile)%field%celldy,                    &
          chunk%tiles(tile)%field%density0,                  &
          chunk%tiles(tile)%field%pressure,                  &
          chunk%tiles(tile)%field%viscosity,                 &
          chunk%tiles(tile)%field%xvel0,                     &
          chunk%tiles(tile)%field%yvel0                      )


      ENDDO
    ELSEIF(use_C_kernels) THEN
      DO tile=1,tiles_per_chunk

        CALL viscosity_kernel_c(chunk%tiles(tile)%t_xmin,                   &
          chunk%tiles(tile)%t_xmax,                     &
          chunk%tiles(tile)%t_ymin,                     &
          chunk%tiles(tile)%t_ymax,                     &
          chunk%tiles(tile)%field%celldx,                    &
          chunk%tiles(tile)%field%celldy,                    &
          chunk%tiles(tile)%field%density0,                  &
          chunk%tiles(tile)%field%pressure,                  &
          chunk%tiles(tile)%field%viscosity,                 &
          chunk%tiles(tile)%field%xvel0,                     &
          chunk%tiles(tile)%field%yvel0                      )


      ENDDO
    ENDIF

  END SUBROUTINE viscosity

END MODULE viscosity_module
