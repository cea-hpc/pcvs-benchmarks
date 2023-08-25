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

!>  @brief Fortran kernel to update the external halo cells in a chunk.
!>  @author Wayne Gaudin
!>  @details Updates halo cells for the required fields at the required depth
!>  for any halo cells that lie on an external boundary. The location and type
!>  of data governs how this is carried out. External boundaries are always
!>  reflective.

MODULE update_tile_halo_kernel_module

  USE data_module

CONTAINS

  SUBROUTINE update_tile_halo_l_kernel(x_min,x_max,y_min,y_max,                            &
                                       density0,                                                   &
                                       energy0,                                                    &
                                       pressure,                                                   &
                                       viscosity,                                                  &
                                       soundspeed,                                                 &
                                       density1,                                                   &
                                       energy1,                                                    &
                                       xvel0,                                                      &
                                       yvel0,                                                      &
                                       xvel1,                                                      &
                                       yvel1,                                                      &
                                       vol_flux_x,                                                 &
                                       vol_flux_y,                                                 &
                                       mass_flux_x,                                                &
                                       mass_flux_y,                                                &
                                       left_xmin, left_xmax, left_ymin, left_ymax,                 &
                                       left_density0,                                                   &
                                       left_energy0,                                                    &
                                       left_pressure,                                                   &
                                       left_viscosity,                                                  &
                                       left_soundspeed,                                                 &
                                       left_density1,                                                   &
                                       left_energy1,                                                    &
                                       left_xvel0,                                                      &
                                       left_yvel0,                                                      &
                                       left_xvel1,                                                      &
                                       left_yvel1,                                                      &
                                       left_vol_flux_x,                                                 &
                                       left_vol_flux_y,                                                 &
                                       left_mass_flux_x,                                                &
                                       left_mass_flux_y,                                                &
                                       fields,                                                     &
                                       depth)

    IMPLICIT NONE

    INTEGER :: x_min,x_max,y_min,y_max
    REAL(KIND=8), DIMENSION(x_min-2:x_max+2,y_min-2:y_max+2) :: density0,energy0
    REAL(KIND=8), DIMENSION(x_min-2:x_max+2,y_min-2:y_max+2) :: pressure,viscosity,soundspeed
    REAL(KIND=8), DIMENSION(x_min-2:x_max+2,y_min-2:y_max+2) :: density1,energy1
    REAL(KIND=8), DIMENSION(x_min-2:x_max+3,y_min-2:y_max+3) :: xvel0,yvel0
    REAL(KIND=8), DIMENSION(x_min-2:x_max+3,y_min-2:y_max+3) :: xvel1,yvel1
    REAL(KIND=8), DIMENSION(x_min-2:x_max+3,y_min-2:y_max+2) :: vol_flux_x,mass_flux_x
    REAL(KIND=8), DIMENSION(x_min-2:x_max+2,y_min-2:y_max+3) :: vol_flux_y,mass_flux_y
    INTEGER :: left_xmin, left_xmax, left_ymin, left_ymax
    REAL(KIND=8), DIMENSION(left_xmin-2:left_xmax+2,left_ymin-2:left_ymax+2) :: left_density0,left_energy0
    REAL(KIND=8), DIMENSION(left_xmin-2:left_xmax+2,left_ymin-2:left_ymax+2) :: left_pressure,left_viscosity,left_soundspeed
    REAL(KIND=8), DIMENSION(left_xmin-2:left_xmax+2,left_ymin-2:left_ymax+2) :: left_density1,left_energy1
    REAL(KIND=8), DIMENSION(left_xmin-2:left_xmax+3,left_ymin-2:left_ymax+3) :: left_xvel0,left_yvel0
    REAL(KIND=8), DIMENSION(left_xmin-2:left_xmax+3,left_ymin-2:left_ymax+3) :: left_xvel1,left_yvel1
    REAL(KIND=8), DIMENSION(left_xmin-2:left_xmax+3,left_ymin-2:left_ymax+2) :: left_vol_flux_x,left_mass_flux_x
    REAL(KIND=8), DIMENSION(left_xmin-2:left_xmax+2,left_ymin-2:left_ymax+3) :: left_vol_flux_y,left_mass_flux_y

    INTEGER :: fields(:),depth

    INTEGER :: j,k




    ! Density 0
   

    IF(fields(FIELD_DENSITY0).EQ.1) THEN

      DO k=y_min-depth,y_max+depth
        DO j=1,depth
          density0(x_min-j,k)=left_density0(left_xmax+1-j,k)
        ENDDO
      ENDDO

    ENDIF

    ! Density 1
    IF(fields(FIELD_DENSITY1).EQ.1) THEN

      DO k=y_min-depth,y_max+depth
        DO j=1,depth
          density1(x_min-j,k)=left_density1(left_xmax+1-j,k)
        ENDDO
      ENDDO

    ENDIF

   
    ! Energy 0
    IF(fields(FIELD_ENERGY0).EQ.1) THEN

      DO k=y_min-depth,y_max+depth
        DO j=1,depth
          energy0(x_min-j,k)=left_energy0(left_xmax+1-j,k)
        ENDDO
      ENDDO

    ENDIF

    ! Energy 1
    IF(fields(FIELD_DENSITY1).EQ.1) THEN

      DO k=y_min-depth,y_max+depth
        DO j=1,depth
          energy1(x_min-j,k)=left_energy1(left_xmax+1-j,k)
        ENDDO
      ENDDO

    ENDIF

  
    ! Pressure
    IF(fields(FIELD_PRESSURE).EQ.1) THEN

      DO k=y_min-depth,y_max+depth
        DO j=1,depth
          pressure(x_min-j,k)=left_pressure(left_xmax+1-j,k)
        ENDDO
      ENDDO

    ENDIF

    ! Viscocity
    IF(fields(FIELD_VISCOSITY).EQ.1) THEN

      DO k=y_min-depth,y_max+depth
        DO j=1,depth
          viscosity(x_min-j,k)=left_viscosity(left_xmax+1-j,k)
        ENDDO
      ENDDO

    ENDIF

    ! Soundspeed
    IF(fields(FIELD_SOUNDSPEED).EQ.1) THEN

      DO k=y_min-depth,y_max+depth
        DO j=1,depth
          soundspeed(x_min-j,k)=left_soundspeed(left_xmax+1-j,k)
        ENDDO
      ENDDO

    ENDIF


    ! XVEL 0
    IF(fields(FIELD_XVEL0).EQ.1) THEN

      DO k=y_min-depth,y_max+1+depth
        DO j=1,depth
          xvel0(x_min-j,k)=left_xvel0(left_xmax+1-j,k)
        ENDDO
      ENDDO

    ENDIF

    ! XVEL 1
    IF(fields(FIELD_XVEL1).EQ.1) THEN

      DO k=y_min-depth,y_max+1+depth
        DO j=1,depth
          xvel1(x_min-j,k)=left_xvel1(left_xmax+1-j,k)
        ENDDO
      ENDDO

    ENDIF

    ! YVEL 0
    IF(fields(FIELD_YVEL0).EQ.1) THEN

      DO k=y_min-depth,y_max+1+depth
        DO j=1,depth
          yvel0(x_min-j,k)=left_yvel0(left_xmax+1-j,k)
        ENDDO
      ENDDO

    ENDIF

    ! YVEL 1
    IF(fields(FIELD_YVEL1).EQ.1) THEN

      DO k=y_min-depth,y_max+1+depth
        DO j=1,depth
          yvel1(x_min-j,k)=left_yvel1(left_xmax+1-j,k)
        ENDDO
      ENDDO

    ENDIF

    ! VOL_FLUX_X
    IF(fields(FIELD_VOL_FLUX_X).EQ.1) THEN

      DO k=y_min-depth,y_max+depth
        DO j=1,depth
          vol_flux_x(x_min-j,k)=left_vol_flux_x(left_xmax+1-j,k)
        ENDDO
      ENDDO

    ENDIF

    ! MASS_FLUX_X
    IF(fields(FIELD_MASS_FLUX_X).EQ.1) THEN

      DO k=y_min-depth,y_max+depth
        DO j=1,depth
          mass_flux_x(x_min-j,k)=left_mass_flux_x(left_xmax+1-j,k)
        ENDDO
      ENDDO

    ENDIF

    ! VOL_FLUX_Y
    IF(fields(FIELD_VOL_FLUX_Y).EQ.1) THEN

      DO k=y_min-depth,y_max+1+depth
        DO j=1,depth
          vol_flux_y(x_min-j,k)=left_vol_flux_y(left_xmax+1-j,k)
        ENDDO
      ENDDO

    ENDIF

    ! MASS_FLUX_Y
    IF(fields(FIELD_MASS_FLUX_Y).EQ.1) THEN

      DO k=y_min-depth,y_max+1+depth
        DO j=1,depth
          mass_flux_y(x_min-j,k)=left_mass_flux_y(left_xmax+1-j,k)
        ENDDO
      ENDDO

    ENDIF



  END SUBROUTINE update_tile_halo_l_kernel


  SUBROUTINE update_tile_halo_r_kernel(x_min,x_max,y_min,y_max,                            &
                                       density0,                                                   &
                                       energy0,                                                    &
                                       pressure,                                                   &
                                       viscosity,                                                  &
                                       soundspeed,                                                 &
                                       density1,                                                   &
                                       energy1,                                                    &
                                       xvel0,                                                      &
                                       yvel0,                                                      &
                                       xvel1,                                                      &
                                       yvel1,                                                      &
                                       vol_flux_x,                                                 &
                                       vol_flux_y,                                                 &
                                       mass_flux_x,                                                &
                                       mass_flux_y,                                                &
                                       right_xmin, right_xmax, right_ymin, right_ymax,                 &
                                       right_density0,                                                   &
                                       right_energy0,                                                    &
                                       right_pressure,                                                   &
                                       right_viscosity,                                                  &
                                       right_soundspeed,                                                 &
                                       right_density1,                                                   &
                                       right_energy1,                                                    &
                                       right_xvel0,                                                      &
                                       right_yvel0,                                                      &
                                       right_xvel1,                                                      &
                                       right_yvel1,                                                      &
                                       right_vol_flux_x,                                                 &
                                       right_vol_flux_y,                                                 &
                                       right_mass_flux_x,                                                &
                                       right_mass_flux_y,                                                &
                                       fields,                                                     &
                                       depth)

    IMPLICIT NONE

    INTEGER :: x_min,x_max,y_min,y_max
    REAL(KIND=8), DIMENSION(x_min-2:x_max+2,y_min-2:y_max+2) :: density0,energy0
    REAL(KIND=8), DIMENSION(x_min-2:x_max+2,y_min-2:y_max+2) :: pressure,viscosity,soundspeed
    REAL(KIND=8), DIMENSION(x_min-2:x_max+2,y_min-2:y_max+2) :: density1,energy1
    REAL(KIND=8), DIMENSION(x_min-2:x_max+3,y_min-2:y_max+3) :: xvel0,yvel0
    REAL(KIND=8), DIMENSION(x_min-2:x_max+3,y_min-2:y_max+3) :: xvel1,yvel1
    REAL(KIND=8), DIMENSION(x_min-2:x_max+3,y_min-2:y_max+2) :: vol_flux_x,mass_flux_x
    REAL(KIND=8), DIMENSION(x_min-2:x_max+2,y_min-2:y_max+3) :: vol_flux_y,mass_flux_y
    INTEGER :: right_xmin, right_xmax, right_ymin, right_ymax
    REAL(KIND=8), DIMENSION(right_xmin-2:right_xmax+2,right_ymin-2:right_ymax+2) :: right_density0,right_energy0
    REAL(KIND=8), DIMENSION(right_xmin-2:right_xmax+2,right_ymin-2:right_ymax+2) :: right_pressure,right_viscosity,right_soundspeed
    REAL(KIND=8), DIMENSION(right_xmin-2:right_xmax+2,right_ymin-2:right_ymax+2) :: right_density1,right_energy1
    REAL(KIND=8), DIMENSION(right_xmin-2:right_xmax+3,right_ymin-2:right_ymax+3) :: right_xvel0,right_yvel0
    REAL(KIND=8), DIMENSION(right_xmin-2:right_xmax+3,right_ymin-2:right_ymax+3) :: right_xvel1,right_yvel1
    REAL(KIND=8), DIMENSION(right_xmin-2:right_xmax+3,right_ymin-2:right_ymax+2) :: right_vol_flux_x,right_mass_flux_x
    REAL(KIND=8), DIMENSION(right_xmin-2:right_xmax+2,right_ymin-2:right_ymax+3) :: right_vol_flux_y,right_mass_flux_y

    INTEGER :: fields(:),depth

    INTEGER :: j,k




    ! Density 0
    IF(fields(FIELD_DENSITY0).EQ.1) THEN

      DO k=y_min-depth,y_max+depth
        DO j=1,depth
          density0(x_max+j,k)=right_density0(right_xmin-1+j,k)
        ENDDO
      ENDDO

    ENDIF

    ! Density 1
    IF(fields(FIELD_DENSITY1).EQ.1) THEN

      DO k=y_min-depth,y_max+depth
        DO j=1,depth
          density1(x_max+j,k)=right_density1(right_xmin-1+j,k)
        ENDDO
      ENDDO

    ENDIF

   
    ! Energy 0
    IF(fields(FIELD_ENERGY0).EQ.1) THEN

      DO k=y_min-depth,y_max+depth
        DO j=1,depth
          energy0(x_max+j,k)=right_energy0(right_xmin-1+j,k)
        ENDDO
      ENDDO

    ENDIF

    ! Energy 1
    IF(fields(FIELD_DENSITY1).EQ.1) THEN

      DO k=y_min-depth,y_max+depth
        DO j=1,depth
          energy1(x_max+j,k)=right_energy1(right_xmin-1+j,k)
        ENDDO
      ENDDO

    ENDIF

  
    ! Pressure
    IF(fields(FIELD_PRESSURE).EQ.1) THEN

      DO k=y_min-depth,y_max+depth
        DO j=1,depth
          pressure(x_max+j,k)=right_pressure(right_xmin-1+j,k)
        ENDDO
      ENDDO

    ENDIF

    ! Viscocity
    IF(fields(FIELD_VISCOSITY).EQ.1) THEN

      DO k=y_min-depth,y_max+depth
        DO j=1,depth
          viscosity(x_max+j,k)=right_viscosity(right_xmin-1+j,k)
        ENDDO
      ENDDO

    ENDIF

    ! Soundspeed
    IF(fields(FIELD_SOUNDSPEED).EQ.1) THEN

      DO k=y_min-depth,y_max+depth
        DO j=1,depth
          soundspeed(x_max+j,k)=right_soundspeed(right_xmin-1+j,k)
        ENDDO
      ENDDO

    ENDIF


    ! XVEL 0
    IF(fields(FIELD_XVEL0).EQ.1) THEN

      DO k=y_min-depth,y_max+1+depth
        DO j=1,depth
          xvel0(x_max+1+j,k)=right_xvel0(right_xmin+1-1+j,k)
        ENDDO
      ENDDO

    ENDIF

    ! XVEL 1
    IF(fields(FIELD_XVEL1).EQ.1) THEN

      DO k=y_min-depth,y_max+1+depth
        DO j=1,depth
          xvel1(x_max+1+j,k)=right_xvel1(right_xmin+1-1+j,k)
        ENDDO
      ENDDO

    ENDIF

    ! YVEL 0
    IF(fields(FIELD_YVEL0).EQ.1) THEN

      DO k=y_min-depth,y_max+1+depth
        DO j=1,depth
          yvel0(x_max+1+j,k)=right_yvel0(right_xmin+1-1+j,k)
        ENDDO
      ENDDO

    ENDIF

    ! YVEL 1
    IF(fields(FIELD_YVEL1).EQ.1) THEN

      DO k=y_min-depth,y_max+1+depth
        DO j=1,depth
          yvel1(x_max+1+j,k)=right_yvel1(right_xmin+1-1+j,k)
        ENDDO
      ENDDO

    ENDIF

    ! VOL_FLUX_X
    IF(fields(FIELD_VOL_FLUX_X).EQ.1) THEN

      DO k=y_min-depth,y_max+depth
        DO j=1,depth
          vol_flux_x(x_max+1+j,k)=right_vol_flux_x(right_xmin+1-1+j,k)
        ENDDO
      ENDDO

    ENDIF

    ! MASS_FLUX_X
    IF(fields(FIELD_MASS_FLUX_X).EQ.1) THEN

      DO k=y_min-depth,y_max+depth
        DO j=1,depth
          mass_flux_x(x_max+1+j,k)=right_mass_flux_x(right_xmin+1-1+j,k)
        ENDDO
      ENDDO

    ENDIF

    ! VOL_FLUX_Y
    IF(fields(FIELD_VOL_FLUX_Y).EQ.1) THEN

      DO k=y_min-depth,y_max+1+depth
        DO j=1,depth
          vol_flux_y(x_max+j,k)=right_vol_flux_y(right_xmin-1+j,k)
        ENDDO
      ENDDO

    ENDIF

    ! MASS_FLUX_Y
    IF(fields(FIELD_MASS_FLUX_Y).EQ.1) THEN

      DO k=y_min-depth,y_max+1+depth
        DO j=1,depth
          mass_flux_y(x_max+j,k)=right_mass_flux_y(right_xmin-1+j,k)
        ENDDO
      ENDDO

    ENDIF



  END SUBROUTINE update_tile_halo_r_kernel


  ! Top and bottom only do xmin -> xmax
  ! This is because the corner ghosts will get communicated in the left right communication

  SUBROUTINE update_tile_halo_t_kernel(x_min,x_max,y_min,y_max,                            &
                                       density0,                                                   &
                                       energy0,                                                    &
                                       pressure,                                                   &
                                       viscosity,                                                  &
                                       soundspeed,                                                 &
                                       density1,                                                   &
                                       energy1,                                                    &
                                       xvel0,                                                      &
                                       yvel0,                                                      &
                                       xvel1,                                                      &
                                       yvel1,                                                      &
                                       vol_flux_x,                                                 &
                                       vol_flux_y,                                                 &
                                       mass_flux_x,                                                &
                                       mass_flux_y,                                                &
                                       top_xmin, top_xmax, top_ymin, top_ymax,                 &
                                       top_density0,                                                   &
                                       top_energy0,                                                    &
                                       top_pressure,                                                   &
                                       top_viscosity,                                                  &
                                       top_soundspeed,                                                 &
                                       top_density1,                                                   &
                                       top_energy1,                                                    &
                                       top_xvel0,                                                      &
                                       top_yvel0,                                                      &
                                       top_xvel1,                                                      &
                                       top_yvel1,                                                      &
                                       top_vol_flux_x,                                                 &
                                       top_vol_flux_y,                                                 &
                                       top_mass_flux_x,                                                &
                                       top_mass_flux_y,                                                &
                                       fields,                                                     &
                                       depth)

    IMPLICIT NONE

    INTEGER :: x_min,x_max,y_min,y_max
    REAL(KIND=8), DIMENSION(x_min-2:x_max+2,y_min-2:y_max+2) :: density0,energy0
    REAL(KIND=8), DIMENSION(x_min-2:x_max+2,y_min-2:y_max+2) :: pressure,viscosity,soundspeed
    REAL(KIND=8), DIMENSION(x_min-2:x_max+2,y_min-2:y_max+2) :: density1,energy1
    REAL(KIND=8), DIMENSION(x_min-2:x_max+3,y_min-2:y_max+3) :: xvel0,yvel0
    REAL(KIND=8), DIMENSION(x_min-2:x_max+3,y_min-2:y_max+3) :: xvel1,yvel1
    REAL(KIND=8), DIMENSION(x_min-2:x_max+3,y_min-2:y_max+2) :: vol_flux_x,mass_flux_x
    REAL(KIND=8), DIMENSION(x_min-2:x_max+2,y_min-2:y_max+3) :: vol_flux_y,mass_flux_y
    INTEGER :: top_xmin, top_xmax, top_ymin, top_ymax
    REAL(KIND=8), DIMENSION(top_xmin-2:top_xmax+2,top_ymin-2:top_ymax+2) :: top_density0,top_energy0
    REAL(KIND=8), DIMENSION(top_xmin-2:top_xmax+2,top_ymin-2:top_ymax+2) :: top_pressure,top_viscosity,top_soundspeed
    REAL(KIND=8), DIMENSION(top_xmin-2:top_xmax+2,top_ymin-2:top_ymax+2) :: top_density1,top_energy1
    REAL(KIND=8), DIMENSION(top_xmin-2:top_xmax+3,top_ymin-2:top_ymax+3) :: top_xvel0,top_yvel0
    REAL(KIND=8), DIMENSION(top_xmin-2:top_xmax+3,top_ymin-2:top_ymax+3) :: top_xvel1,top_yvel1
    REAL(KIND=8), DIMENSION(top_xmin-2:top_xmax+3,top_ymin-2:top_ymax+2) :: top_vol_flux_x,top_mass_flux_x
    REAL(KIND=8), DIMENSION(top_xmin-2:top_xmax+2,top_ymin-2:top_ymax+3) :: top_vol_flux_y,top_mass_flux_y

    INTEGER :: fields(:),depth

    INTEGER :: j,k




    ! Density 0
    IF(fields(FIELD_DENSITY0).EQ.1) THEN

      DO k=1,depth
        DO j=x_min-depth, x_max+depth
          density0(j,y_max+k)=top_density0(j,top_ymin-1+k)
        ENDDO
      ENDDO

    ENDIF

    ! Density 1
    IF(fields(FIELD_DENSITY1).EQ.1) THEN

      DO k=1,depth
        DO j=x_min-depth, x_max+depth
          density1(j,y_max+k)=top_density1(j,top_ymin-1+k)
        ENDDO
      ENDDO

    ENDIF

   
    ! Energy 0
    IF(fields(FIELD_ENERGY0).EQ.1) THEN

      DO k=1,depth
        DO j=x_min-depth, x_max+depth
          energy0(j,y_max+k)=top_energy0(j,top_ymin-1+k)
        ENDDO
      ENDDO

    ENDIF

    ! Energy 1
    IF(fields(FIELD_DENSITY1).EQ.1) THEN

      DO k=1,depth
        DO j=x_min-depth, x_max+depth
          energy1(j,y_max+k)=top_energy1(j,top_ymin-1+k)
        ENDDO
      ENDDO

    ENDIF

  
    ! Pressure
    IF(fields(FIELD_PRESSURE).EQ.1) THEN

      DO k=1,depth
        DO j=x_min-depth, x_max+depth
          pressure(j,y_max+k)=top_pressure(j,top_ymin-1+k)
        ENDDO
      ENDDO

    ENDIF

    ! Viscocity
    IF(fields(FIELD_VISCOSITY).EQ.1) THEN

      DO k=1,depth
        DO j=x_min-depth, x_max+depth
          viscosity(j,y_max+k)=top_viscosity(j,top_ymin-1+k)
        ENDDO
      ENDDO

    ENDIF

    ! Soundspeed
    IF(fields(FIELD_SOUNDSPEED).EQ.1) THEN

      DO k=1,depth
        DO j=x_min-depth, x_max+depth
          soundspeed(j,y_max+k)=top_soundspeed(j,top_ymin-1+k)
        ENDDO
      ENDDO

    ENDIF


    ! XVEL 0
    IF(fields(FIELD_XVEL0).EQ.1) THEN

      DO k=1,depth
        DO j=x_min-depth, x_max+1+depth
          xvel0(j,y_max+1+k)=top_xvel0(j,top_ymin+1-1+k)
        ENDDO
      ENDDO

    ENDIF

    ! XVEL 1
    IF(fields(FIELD_XVEL1).EQ.1) THEN

      DO k=1,depth
        DO j=x_min-depth, x_max+1+depth
          xvel1(j,y_max+1+k)=top_xvel1(j,top_ymin+1-1+k)
        ENDDO
      ENDDO

    ENDIF

    ! YVEL 0
    IF(fields(FIELD_YVEL0).EQ.1) THEN

      DO k=1,depth
        DO j=x_min-depth, x_max+1+depth
          yvel0(j,y_max+1+k)=top_yvel0(j,top_ymin+1-1+k)
        ENDDO
      ENDDO

    ENDIF

    ! YVEL 1
    IF(fields(FIELD_YVEL1).EQ.1) THEN

      DO k=1,depth
        DO j=x_min-depth, x_max+1+depth
          yvel1(j,y_max+1+k)=top_yvel1(j,top_ymin+1-1+k)
        ENDDO
      ENDDO

    ENDIF

    ! VOL_FLUX_X
    IF(fields(FIELD_VOL_FLUX_X).EQ.1) THEN

      DO k=1,depth
        DO j=x_min-depth, x_max+1+depth
          vol_flux_x(j,y_max+k)=top_vol_flux_x(j,top_ymin-1+k)
        ENDDO
      ENDDO

    ENDIF

    ! MASS_FLUX_X
    IF(fields(FIELD_MASS_FLUX_X).EQ.1) THEN

      DO k=1,depth
        DO j=x_min-depth, x_max+1+depth
          mass_flux_x(j,y_max+k)=top_mass_flux_x(j,top_ymin-1+k)
        ENDDO
      ENDDO

    ENDIF

    ! VOL_FLUX_Y
    IF(fields(FIELD_VOL_FLUX_Y).EQ.1) THEN

      DO k=1,depth
        DO j=x_min-depth, x_max+depth
          vol_flux_y(j,y_max+1+k)=top_vol_flux_y(j,top_ymin+1-1+k)
        ENDDO
      ENDDO

    ENDIF

    ! MASS_FLUX_Y
    IF(fields(FIELD_MASS_FLUX_Y).EQ.1) THEN

      DO k=1,depth
        DO j=x_min-depth, x_max+depth
          mass_flux_y(j,y_max+1+k)=top_mass_flux_y(j,top_ymin+1-1+k)
        ENDDO
      ENDDO

    ENDIF



  END SUBROUTINE update_tile_halo_t_kernel


  SUBROUTINE update_tile_halo_b_kernel(x_min,x_max,y_min,y_max,                     &
                                       density0,                                           &
                                       energy0,                                            &
                                       pressure,                                           &
                                       viscosity,                                          &
                                       soundspeed,                                         &
                                       density1,                                           &
                                       energy1,                                            &
                                       xvel0,                                              &
                                       yvel0,                                              &
                                       xvel1,                                              &
                                       yvel1,                                              &
                                       vol_flux_x,                                         &
                                       vol_flux_y,                                         &
                                       mass_flux_x,                                        &
                                       mass_flux_y,                                        &
                                       bottom_xmin, bottom_xmax, bottom_ymin, bottom_ymax, &
                                       bottom_density0,                                    &
                                       bottom_energy0,                                     &
                                       bottom_pressure,                                    &
                                       bottom_viscosity,                                   &
                                       bottom_soundspeed,                                  &
                                       bottom_density1,                                    &
                                       bottom_energy1,                                     &
                                       bottom_xvel0,                                       &
                                       bottom_yvel0,                                       &
                                       bottom_xvel1,                                       &
                                       bottom_yvel1,                                       &
                                       bottom_vol_flux_x,                                  &
                                       bottom_vol_flux_y,                                  &
                                       bottom_mass_flux_x,                                 &
                                       bottom_mass_flux_y,                                 &
                                       fields,                                             &
                                       depth)

    IMPLICIT NONE

    INTEGER :: x_min,x_max,y_min,y_max
    REAL(KIND=8), DIMENSION(x_min-2:x_max+2,y_min-2:y_max+2) :: density0,energy0
    REAL(KIND=8), DIMENSION(x_min-2:x_max+2,y_min-2:y_max+2) :: pressure,viscosity,soundspeed
    REAL(KIND=8), DIMENSION(x_min-2:x_max+2,y_min-2:y_max+2) :: density1,energy1
    REAL(KIND=8), DIMENSION(x_min-2:x_max+3,y_min-2:y_max+3) :: xvel0,yvel0
    REAL(KIND=8), DIMENSION(x_min-2:x_max+3,y_min-2:y_max+3) :: xvel1,yvel1
    REAL(KIND=8), DIMENSION(x_min-2:x_max+3,y_min-2:y_max+2) :: vol_flux_x,mass_flux_x
    REAL(KIND=8), DIMENSION(x_min-2:x_max+2,y_min-2:y_max+3) :: vol_flux_y,mass_flux_y
    INTEGER :: bottom_xmin, bottom_xmax, bottom_ymin, bottom_ymax
    REAL(KIND=8), DIMENSION(bottom_xmin-2:bottom_xmax+2,bottom_ymin-2:bottom_ymax+2) :: &
      bottom_density0,bottom_energy0
    REAL(KIND=8), DIMENSION(bottom_xmin-2:bottom_xmax+2,bottom_ymin-2:bottom_ymax+2) :: &
      bottom_pressure,bottom_viscosity,bottom_soundspeed
    REAL(KIND=8), DIMENSION(bottom_xmin-2:bottom_xmax+2,bottom_ymin-2:bottom_ymax+2) :: &
      bottom_density1,bottom_energy1
    REAL(KIND=8), DIMENSION(bottom_xmin-2:bottom_xmax+3,bottom_ymin-2:bottom_ymax+3) :: &
      bottom_xvel0,bottom_yvel0
    REAL(KIND=8), DIMENSION(bottom_xmin-2:bottom_xmax+3,bottom_ymin-2:bottom_ymax+3) :: &
      bottom_xvel1,bottom_yvel1
    REAL(KIND=8), DIMENSION(bottom_xmin-2:bottom_xmax+3,bottom_ymin-2:bottom_ymax+2) :: &
      bottom_vol_flux_x,bottom_mass_flux_x
    REAL(KIND=8), DIMENSION(bottom_xmin-2:bottom_xmax+2,bottom_ymin-2:bottom_ymax+3) :: &
      bottom_vol_flux_y,bottom_mass_flux_y

    INTEGER :: fields(:),depth


    INTEGER :: j,k




    ! Density 0
    IF(fields(FIELD_DENSITY0).EQ.1) THEN

      DO k=1,depth
        DO j=x_min-depth, x_max+depth
          density0(j,y_min-k)=bottom_density0(j,bottom_ymax+1-k)
        ENDDO
      ENDDO

    ENDIF

    ! Density 1
    IF(fields(FIELD_DENSITY1).EQ.1) THEN

      DO k=1,depth
        DO j=x_min-depth, x_max+depth
          density1(j,y_min-k)=bottom_density1(j,bottom_ymax+1-k)
        ENDDO
      ENDDO

    ENDIF

   
    ! Energy 0
    IF(fields(FIELD_ENERGY0).EQ.1) THEN

      DO k=1,depth
        DO j=x_min-depth, x_max+depth
          energy0(j,y_min-k)=bottom_energy0(j,bottom_ymax+1-k)
        ENDDO
      ENDDO

    ENDIF

    ! Energy 1
    IF(fields(FIELD_DENSITY1).EQ.1) THEN

      DO k=1,depth
        DO j=x_min-depth, x_max+depth
          energy1(j,y_min-k)=bottom_energy1(j,bottom_ymax+1-k)
        ENDDO
      ENDDO

    ENDIF

  
    ! Pressure
    IF(fields(FIELD_PRESSURE).EQ.1) THEN

      DO k=1,depth
        DO j=x_min-depth, x_max+depth
          pressure(j,y_min-k)=bottom_pressure(j,bottom_ymax+1-k)
        ENDDO
      ENDDO

    ENDIF

    ! Viscocity
    IF(fields(FIELD_VISCOSITY).EQ.1) THEN

      DO k=1,depth
        DO j=x_min-depth, x_max+depth
          viscosity(j,y_min-k)=bottom_viscosity(j,bottom_ymax+1-k)
        ENDDO
      ENDDO

    ENDIF

    ! Soundspeed
    IF(fields(FIELD_SOUNDSPEED).EQ.1) THEN

      DO k=1,depth
        DO j=x_min-depth, x_max+depth
          soundspeed(j,y_min-k)=bottom_soundspeed(j,bottom_ymax+1-k)
        ENDDO
      ENDDO

    ENDIF


    ! XVEL 0
    IF(fields(FIELD_XVEL0).EQ.1) THEN

      DO k=1,depth
        DO j=x_min-depth, x_max+1+depth
          xvel0(j,y_min-k)=bottom_xvel0(j,bottom_ymax+1-k)
        ENDDO
      ENDDO

    ENDIF

    ! XVEL 1
    IF(fields(FIELD_XVEL1).EQ.1) THEN

      DO k=1,depth
        DO j=x_min-depth, x_max+1+depth
          xvel1(j,y_min-k)=bottom_xvel1(j,bottom_ymax+1-k)
        ENDDO
      ENDDO

    ENDIF

    ! YVEL 0
    IF(fields(FIELD_YVEL0).EQ.1) THEN

      DO k=1,depth
        DO j=x_min-depth, x_max+1+depth
          yvel0(j,y_min-k)=bottom_yvel0(j,bottom_ymax+1-k)
        ENDDO
      ENDDO

    ENDIF

    ! YVEL 1
    IF(fields(FIELD_YVEL1).EQ.1) THEN

      DO k=1,depth
        DO j=x_min-depth, x_max+1+depth
          yvel1(j,y_min-k)=bottom_yvel1(j,bottom_ymax+1-k)
        ENDDO
      ENDDO

    ENDIF

    ! VOL_FLUX_X
    IF(fields(FIELD_VOL_FLUX_X).EQ.1) THEN

      DO k=1,depth
        DO j=x_min-depth, x_max+1+depth
          vol_flux_x(j,y_min-k)=bottom_vol_flux_x(j,bottom_ymax+1-k)
        ENDDO
      ENDDO

    ENDIF

    ! MASS_FLUX_X
    IF(fields(FIELD_MASS_FLUX_X).EQ.1) THEN

      DO k=1,depth
        DO j=x_min-depth, x_max+1+depth
          mass_flux_x(j,y_min-k)=bottom_mass_flux_x(j,bottom_ymax+1-k)
        ENDDO
      ENDDO

    ENDIF

    ! VOL_FLUX_Y
    IF(fields(FIELD_VOL_FLUX_Y).EQ.1) THEN

      DO k=1,depth
        DO j=x_min-depth, x_max+depth
          vol_flux_y(j,y_min-k)=bottom_vol_flux_y(j,bottom_ymax+1-k)
        ENDDO
      ENDDO

    ENDIF

    ! MASS_FLUX_Y
    IF(fields(FIELD_MASS_FLUX_Y).EQ.1) THEN

      DO k=1,depth
        DO j=x_min-depth, x_max+depth
          mass_flux_y(j,y_min-k)=bottom_mass_flux_y(j,bottom_ymax+1-k)
        ENDDO
      ENDDO

    ENDIF



  END SUBROUTINE update_tile_halo_b_kernel



END  MODULE update_tile_halo_kernel_module
