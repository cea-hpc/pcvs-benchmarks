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

!>  @brief Ideal gas kernel driver
!>  @author Wayne Gaudin
!>  @details Invokes the user specified kernel for the ideal gas equation of
!>  state using the specified time level data.

MODULE ideal_gas_module

CONTAINS

  SUBROUTINE ideal_gas(tile,predict)

    USE clover_module
    USE ideal_gas_kernel_module

    IMPLICIT NONE

    INTEGER :: tile

    LOGICAl :: predict


    IF(use_fortran_kernels) THEN
      IF(.NOT.predict) THEN
        CALL ideal_gas_kernel(chunk%tiles(tile)%t_xmin,    &
          chunk%tiles(tile)%t_xmax,      &
          chunk%tiles(tile)%t_ymin,      &
          chunk%tiles(tile)%t_ymax,      &
          chunk%tiles(tile)%field%density0,   &
          chunk%tiles(tile)%field%energy0,    &
          chunk%tiles(tile)%field%pressure,   &
          chunk%tiles(tile)%field%soundspeed  )

      ELSE
        CALL ideal_gas_kernel(chunk%tiles(tile)%t_xmin,    &
          chunk%tiles(tile)%t_xmax,      &
          chunk%tiles(tile)%t_ymin,      &
          chunk%tiles(tile)%t_ymax,      &
          chunk%tiles(tile)%field%density1,   &
          chunk%tiles(tile)%field%energy1,    &
          chunk%tiles(tile)%field%pressure,   &
          chunk%tiles(tile)%field%soundspeed  )

      ENDIF

    ELSEIF(use_C_kernels) THEN
      IF(.NOT.predict) THEN
        CALL ideal_gas_kernel_c(chunk%tiles(tile)%t_xmin,    &
          chunk%tiles(tile)%t_xmax,      &
          chunk%tiles(tile)%t_ymin,      &
          chunk%tiles(tile)%t_ymax,      &
          chunk%tiles(tile)%field%density0,   &
          chunk%tiles(tile)%field%energy0,    &
          chunk%tiles(tile)%field%pressure,   &
          chunk%tiles(tile)%field%soundspeed  )

      ELSE
        CALL ideal_gas_kernel_c(chunk%tiles(tile)%t_xmin,    &
          chunk%tiles(tile)%t_xmax,      &
          chunk%tiles(tile)%t_ymin,      &
          chunk%tiles(tile)%t_ymax,      &
          chunk%tiles(tile)%field%density1,   &
          chunk%tiles(tile)%field%energy1,    &
          chunk%tiles(tile)%field%pressure,   &
          chunk%tiles(tile)%field%soundspeed  )

      ENDIF
    ENDIF

  END SUBROUTINE ideal_gas

END MODULE ideal_gas_module
