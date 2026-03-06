// ************************************************************************
//
//              miniGhost: stencil computations with boundary exchange.
//                 Copyright (2012) Sandia Corporation
//
// Under terms of Contract DE-AC04-94AL85000, there is a non-exclusive
// license for use of this work by or on behalf of the U.S. Government.
//
// This library is free software; you can redistribute it and/or modify
// it under the terms of the GNU Lesser General Public License as
// published by the Free Software Foundation; either version 2.1 of the
// License, or (at your option) any later version.
//
// This library is distributed in the hope that it will be useful, but
// WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
// Lesser General Public License for more details.
//
// You should have received a copy of the GNU Lesser General Public
// License along with this library; if not, write to the Free Software
// Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307
// USA
// Questions? Contact Richard F. Barrett (rfbarre@sandia.gov) or
//                    Michael A. Heroux (maherou@sandia.gov)
//
// ************************************************************************

   memset(&param, 0, sizeof(param));

/* Ensure correct mapping to Fortran parameters defined in MG_OPTIONS.F. */

   param.scaling         = 2;     /* Weak scaling.                                                */
   param.nx              = 100;   /* Local x dimension (local for weak scaling,
                                    global when strong scaling selected.                          */
   param.ny              = 100;   /* Local y dimension (local for weak scaling,
                                    global when strong scaling selected.                          */
   param.nz              = 100;   /* Local z dimension (local for weak scaling,
                                     global when strong scaling selected.                         */
   param.stencil         = 21;    /* 2d 5-point stencil.                                          */
   param.num_vars        = 5;     /* Number of variables.                                         */
   param.percent_sum     = 100;   /* (Approximate) number of variables that to be summed.
                                     This is inteended to inject additional work (including a
                                     global reduction, but is not a correctness check.            */
   param.num_spikes      = 1;     /* Number of source terms to be applied,
                                     one per max number of time steps.                            */
   param.num_tsteps      = 100;   /* Number of time steps to be iterated.                         */
   param.stencil         = 21;    /* Stencil selected. See MG_OPTIONS.F for parameter settings.   */
   param.comm_method     = 10;    /* Boundary exchange option.                                    */
   param.bc              = 31;    /* Dirichlet boundary conditions                                */
                                  /* See MG_OPTIONS.F for parameter settings.                     */
#if defined _MG_REAL8
   param.error_tol       = 8;     /* 1.0 / (10.0**param.error_tol                                 */
#elif defined _MG_REAL4
   param.error_tol       = 4;
#endif

   param.report_diffusion= 20;    /* Every param.report_diffusion time steps, report error.
                                     Note that if in debug_grid mode, once the number of time
                                     steps exceeds half of the minimum global dimension, an error
                                     is expected to occur as the heat disapates off of the domain.*/

   /* Logical processor grid default (numpes, 1, 1) in main.c, but wanted to set something here.  */
   param.npx             = 1;     /* Logical processor grid dimension in x direction              */
   param.npy             = 1;     /* Logical processor grid dimension in y direction              */
   param.npz             = 1;     /* Logical processor grid dimension in z direction              */


   param.checkpoint_method   = 1; /* Checkpoint/restart implementation option.  See MG_OPTIONS.F  */
   param.checkpoint_interval = 0;
   param.checkpoint_file[0]  = '\0';
   param.restart_cp_num      = -2;
   param.restart_file[0]     = '\0';

   param.report_perf     = 0;     /* Amount of performance data reported.                         */

   param.debug_grid      = 0;     /* 1: zero domain, insert heat sourse (spike) into center.      */
