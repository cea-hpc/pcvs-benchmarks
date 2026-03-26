/*Crown Copyright 2012 AWE.
*
* This file is part of CloverLeaf.
*
* CloverLeaf is free software: you can redistribute it and/or modify it under 
* the terms of the GNU General Public License as published by the 
* Free Software Foundation, either version 3 of the License, or (at your option) 
* any later version.
*
* CloverLeaf is distributed in the hope that it will be useful, but 
* WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or 
* FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public License for more 
* details.
*
* You should have received a copy of the GNU General Public License along with 
* CloverLeaf. If not, see http://www.gnu.org/licenses/. */

/**
 *  @brief C mpi buffer packing kernel
 *  @author Wayne Gaudin
 *  @details Packs/unpacks mpi send and receive buffers
 */

#include <stdio.h>
#include <stdlib.h>
#include "ftocmacros.h"
#include <math.h>

void clover_pack_message_left_c_(int *xmin,int *xmax,int *ymin,int *ymax, double *field,
                                 double *left_snd_buffer,
                                 int *CLL_DT,int *VRTX_DT,int *X_FC_DT,int *Y_FC_DT,
                                 int *dpth, int *fld_typ,
                                 int *bffr_ffst)

{
  int x_min=*xmin;
  int x_max=*xmax;
  int y_min=*ymin;
  int y_max=*ymax;
  int CELL_DATA=*CLL_DT;
  int VERTEX_DATA=*VRTX_DT;
  int X_FACE_DATA=*X_FC_DT;
  int Y_FACE_DATA=*Y_FC_DT;
  int field_type=*fld_typ;
  int depth=*dpth;
  int buffer_offset=*bffr_ffst;

  int j,k,index,x_inc,y_inc;

//Pack 

// These array modifications still need to be added on, plus the donor data location changes as in update_halo
  if(field_type==CELL_DATA) {
    x_inc=0;
    y_inc=0;
  }
  if(field_type==VERTEX_DATA) {
    x_inc=1;
    y_inc=1;
  }
  if(field_type==X_FACE_DATA) {
    x_inc=1;
    y_inc=0;
  }
  if(field_type==Y_FACE_DATA) {
    x_inc=0;
    y_inc=1;
  }

#pragma omp parallel for private(j,k,index)
  for (k=y_min-depth;k<=y_max+y_inc+depth;k++) {
#pragma ivdep
    for (j=1;j<=depth;j++) {
      index=buffer_offset + j+(k+depth-1)*depth;
      left_snd_buffer[FTNREF1D(index,1)]=field[FTNREF2D(x_min+x_inc-1+j,k,x_max+4+x_inc,x_min-2,y_min-2)];
    }
  }

}

void clover_unpack_message_left_c_(int *xmin,int *xmax,int *ymin,int *ymax, double *field,
                                   double *left_rcv_buffer,
                                   int *CLL_DT,int *VRTX_DT,int *X_FC_DT,int *Y_FC_DT,
                                   int *dpth, int *fld_typ,
                                   int *bffr_ffst)

{
  int x_min=*xmin;
  int x_max=*xmax;
  int y_min=*ymin;
  int y_max=*ymax;
  int CELL_DATA=*CLL_DT;
  int VERTEX_DATA=*VRTX_DT;
  int X_FACE_DATA=*X_FC_DT;
  int Y_FACE_DATA=*Y_FC_DT;
  int field_type=*fld_typ;
  int depth=*dpth;
  int buffer_offset=*bffr_ffst;

  int j,k,index,x_inc,y_inc;

//Unpack 

// These array modifications still need to be added on, plus the donor data location changes as in update_halo
  if(field_type==CELL_DATA) {
    x_inc=0;
    y_inc=0;
  }
  if(field_type==VERTEX_DATA) {
    x_inc=1;
    y_inc=1;
  }
  if(field_type==X_FACE_DATA) {
    x_inc=1;
    y_inc=0;
  }
  if(field_type==Y_FACE_DATA) {
    x_inc=0;
    y_inc=1;
  }

#pragma omp parallel for private(j,k,index)
  for (k=y_min-depth;k<=y_max+y_inc+depth;k++) {
#pragma ivdep
    for (j=1;j<=depth;j++) {
      index=buffer_offset + j+(k+depth-1)*depth;
      field[FTNREF2D(x_min-j,k,x_max+4+x_inc,x_min-2,y_min-2)]=left_rcv_buffer[FTNREF1D(index,1)];
    }
  }

}

void clover_pack_message_right_c_(int *xmin,int *xmax,int *ymin,int *ymax, double *field,
                                  double *right_snd_buffer,
                                  int *CLL_DT,int *VRTX_DT,int *X_FC_DT,int *Y_FC_DT,
                                  int *dpth, int *fld_typ,
                                  int *bffr_ffst)

{
  int x_min=*xmin;
  int x_max=*xmax;
  int y_min=*ymin;
  int y_max=*ymax;
  int CELL_DATA=*CLL_DT;
  int VERTEX_DATA=*VRTX_DT;
  int X_FACE_DATA=*X_FC_DT;
  int Y_FACE_DATA=*Y_FC_DT;
  int field_type=*fld_typ;
  int depth=*dpth;
  int buffer_offset=*bffr_ffst;

  int j,k,index,x_inc,y_inc;

//Pack 

// These array modifications still need to be added on, plus the donor data location changes as in update_halo
  if(field_type==CELL_DATA) {
    x_inc=0;
    y_inc=0;
  }
  if(field_type==VERTEX_DATA) {
    x_inc=1;
    y_inc=1;
  }
  if(field_type==X_FACE_DATA) {
    x_inc=1;
    y_inc=0;
  }
  if(field_type==Y_FACE_DATA) {
    x_inc=0;
    y_inc=1;
  }

#pragma omp parallel for private(j,k,index)
  for (k=y_min-depth;k<=y_max+y_inc+depth;k++) {
#pragma ivdep
    for (j=1;j<=depth;j++) {
      index=buffer_offset + j+(k+depth-1)*depth;
      right_snd_buffer[FTNREF1D(index,1)]=field[FTNREF2D(x_max+1-j,k,x_max+4+x_inc,x_min-2,y_min-2)];
    }
  }

}

void clover_unpack_message_right_c_(int *xmin,int *xmax,int *ymin,int *ymax, double *field,
                                    double *right_rcv_buffer,
                                    int *CLL_DT,int *VRTX_DT,int *X_FC_DT,int *Y_FC_DT,
                                    int *dpth, int *fld_typ,
                                    int *bffr_ffst)

{
  int x_min=*xmin;
  int x_max=*xmax;
  int y_min=*ymin;
  int y_max=*ymax;
  int CELL_DATA=*CLL_DT;
  int VERTEX_DATA=*VRTX_DT;
  int X_FACE_DATA=*X_FC_DT;
  int Y_FACE_DATA=*Y_FC_DT;
  int field_type=*fld_typ;
  int depth=*dpth;
  int buffer_offset=*bffr_ffst;

  int j,k,index,x_inc,y_inc;

//Pack 

// These array modifications still need to be added on, plus the donor data location changes as in update_halo
  if(field_type==CELL_DATA) {
    x_inc=0;
    y_inc=0;
  }
  if(field_type==VERTEX_DATA) {
    x_inc=1;
    y_inc=1;
  }
  if(field_type==X_FACE_DATA) {
    x_inc=1;
    y_inc=0;
  }
  if(field_type==Y_FACE_DATA) {
    x_inc=0;
    y_inc=1;
  }

#pragma omp parallel for private(j,k,index)
  for (k=y_min-depth;k<=y_max+y_inc+depth;k++) {
#pragma ivdep
    for (j=1;j<=depth;j++) {
      index=buffer_offset + j+(k+depth-1)*depth;
      field[FTNREF2D(x_max+x_inc+j,k,x_max+4+x_inc,x_min-2,y_min-2)]=right_rcv_buffer[FTNREF1D(index,1)];
    }
  }

}

void clover_pack_message_top_c_(int *xmin,int *xmax,int *ymin,int *ymax, double *field,
                                double *top_snd_buffer,
                                int *CLL_DT,int *VRTX_DT,int *X_FC_DT,int *Y_FC_DT,
                                int *dpth, int *fld_typ,
                                int *bffr_ffst)

{
  int x_min=*xmin;
  int x_max=*xmax;
  int y_min=*ymin;
  int y_max=*ymax;
  int CELL_DATA=*CLL_DT;
  int VERTEX_DATA=*VRTX_DT;
  int X_FACE_DATA=*X_FC_DT;
  int Y_FACE_DATA=*Y_FC_DT;
  int field_type=*fld_typ;
  int depth=*dpth;
  int buffer_offset=*bffr_ffst;

  int j,k,index,x_inc,y_inc;

//Pack 

// These array modifications still need to be added on, plus the donor data location changes as in update_halo
  if(field_type==CELL_DATA) {
    x_inc=0;
    y_inc=0;
  }
  if(field_type==VERTEX_DATA) {
    x_inc=1;
    y_inc=1;
  }
  if(field_type==X_FACE_DATA) {
    x_inc=1;
    y_inc=0;
  }
  if(field_type==Y_FACE_DATA) {
    x_inc=0;
    y_inc=1;
  }

  for (k=1;k<=depth;k++) {
#pragma omp parallel for private(j,index)
    for (j=x_min-depth;j<=x_max+x_inc+depth;j++) {
      index= buffer_offset + k+(j+depth-1)*depth;
      top_snd_buffer[FTNREF1D(index,1)]=field[FTNREF2D(j,y_max+1-k,x_max+4+x_inc,x_min-2,y_min-2)];
    }
  }

}

void clover_pack_message_bottom_c_(int *xmin,int *xmax,int *ymin,int *ymax, double *field,
                                   double *bottom_snd_buffer,
                                   int *CLL_DT,int *VRTX_DT,int *X_FC_DT,int *Y_FC_DT,
                                   int *dpth, int *fld_typ,
                                   int *bffr_ffst)

{
  int x_min=*xmin;
  int x_max=*xmax;
  int y_min=*ymin;
  int y_max=*ymax;
  int CELL_DATA=*CLL_DT;
  int VERTEX_DATA=*VRTX_DT;
  int X_FACE_DATA=*X_FC_DT;
  int Y_FACE_DATA=*Y_FC_DT;
  int field_type=*fld_typ;
  int depth=*dpth;
  int buffer_offset=*bffr_ffst;

  int j,k,index,x_inc,y_inc;

//Pack 

// These array modifications still need to be added on, plus the donor data location changes as in update_halo
  if(field_type==CELL_DATA) {
    x_inc=0;
    y_inc=0;
  }
  if(field_type==VERTEX_DATA) {
    x_inc=1;
    y_inc=1;
  }
  if(field_type==X_FACE_DATA) {
    x_inc=1;
    y_inc=0;
  }
  if(field_type==Y_FACE_DATA) {
    x_inc=0;
    y_inc=1;
  }

  for (k=1;k<=depth;k++) {
#pragma omp parallel for private(j,index)
    for (j=x_min-depth;j<=x_max+x_inc+depth;j++) {
      index= buffer_offset + k+(j+depth-1)*depth;
      bottom_snd_buffer[FTNREF1D(index,1)]=field[FTNREF2D(j,y_min+y_inc-1+k,x_max+4+x_inc,x_min-2,y_min-2)];
    }
  }

}

void clover_unpack_message_bottom_c_(int *xmin,int *xmax,int *ymin,int *ymax, double *field,
                                     double *bottom_rcv_buffer,
                                     int *CLL_DT,int *VRTX_DT,int *X_FC_DT,int *Y_FC_DT,
                                     int *dpth, int *fld_typ,
                                     int *bffr_ffst)

{
  int x_min=*xmin;
  int x_max=*xmax;
  int y_min=*ymin;
  int y_max=*ymax;
  int CELL_DATA=*CLL_DT;
  int VERTEX_DATA=*VRTX_DT;
  int X_FACE_DATA=*X_FC_DT;
  int Y_FACE_DATA=*Y_FC_DT;
  int field_type=*fld_typ;
  int depth=*dpth;
  int buffer_offset=*bffr_ffst;

  int j,k,index,x_inc,y_inc;

//Unpack 

// These array modifications still need to be added on, plus the donor data location changes as in update_halo
  if(field_type==CELL_DATA) {
    x_inc=0;
    y_inc=0;
  }
  if(field_type==VERTEX_DATA) {
    x_inc=1;
    y_inc=1;
  }
  if(field_type==X_FACE_DATA) {
    x_inc=1;
    y_inc=0;
  }
  if(field_type==Y_FACE_DATA) {
    x_inc=0;
    y_inc=1;
  }

  for (k=1;k<=depth;k++) {
#pragma omp parallel for private(j,index)
    for (j=x_min-depth;j<=x_max+x_inc+depth;j++) {
      index= buffer_offset + k+(j+depth-1)*depth;
      field[FTNREF2D(j,y_min-k,x_max+4+x_inc,x_min-2,y_min-2)]=bottom_rcv_buffer[FTNREF1D(index,1)];
    }
  }

}

void clover_unpack_message_top_c_(int *xmin,int *xmax,int *ymin,int *ymax, double *field,
                                  double *top_rcv_buffer,
                                  int *CLL_DT,int *VRTX_DT,int *X_FC_DT,int *Y_FC_DT,
                                  int *dpth, int *fld_typ,
                                  int *bffr_ffst)

{
  int x_min=*xmin;
  int x_max=*xmax;
  int y_min=*ymin;
  int y_max=*ymax;
  int CELL_DATA=*CLL_DT;
  int VERTEX_DATA=*VRTX_DT;
  int X_FACE_DATA=*X_FC_DT;
  int Y_FACE_DATA=*Y_FC_DT;
  int field_type=*fld_typ;
  int depth=*dpth;
  int buffer_offset=*bffr_ffst;

  int j,k,index,x_inc,y_inc;

//Unpack 

// These array modifications still need to be added on, plus the donor data location changes as in update_halo
  if(field_type==CELL_DATA) {
    x_inc=0;
    y_inc=0;
  }
  if(field_type==VERTEX_DATA) {
    x_inc=1;
    y_inc=1;
  }
  if(field_type==X_FACE_DATA) {
    x_inc=1;
    y_inc=0;
  }
  if(field_type==Y_FACE_DATA) {
    x_inc=0;
    y_inc=1;
  }

  for (k=1;k<=depth;k++) {
#pragma omp parallel for private(j,index)
    for (j=x_min-depth;j<=x_max+x_inc+depth;j++) {
      index= buffer_offset + k+(j+depth-1)*depth;
      field[FTNREF2D(j,y_max+y_inc+k,x_max+4+x_inc,x_min-2,y_min-2)]=top_rcv_buffer[FTNREF1D(index,1)];
    }
  }

}
