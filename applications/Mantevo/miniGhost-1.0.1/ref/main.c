//@header
// ************************************************************************
//
//      miniGhost: stencil computations with boundary exchange.
//              Copyright (2012) Sandia Corporation
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
//@header

#include <assert.h>
#include <stdlib.h>
#include <stdio.h>
#include <string.h>

#if defined _MG_MPI
#include "mpi.h"
#endif

#define SCALING_STRONG 1     // Ensure these values match the Fortran settings!
#define SCALING_WEAK   2

int
   mype,
   numpes;

   typedef struct {
      int
         scaling,
         nx,
         ny,
         nz,
         num_vars,
         percent_sum,
         num_spikes,
         num_tsteps,
         stencil,
         comm_method,
         bc,
         error_tol,
         report_diffusion,
         npx,
         npy,
         npz,
         report_perf,
         checkpoint_method,
         checkpoint_interval,
         restart_cp_num,
         debug_grid;

      char
         checkpoint_file[1024],
         restart_file[1024];
   } Input_params; // Problem parameters, passed to Fortran DRIVER.

// Prototypes

int check_input ( Input_params *param );
void print_help_message ();
void MG_Assert ( int ierr, char *error_msg );

#if defined _F2C_UPPER_CASE
#define MINI_GHOST MINI_GHOST
#elif defined _F2C___
#define MINI_GHOST mini_ghost__
#else
#define MINI_GHOST mini_ghost_
#endif

void MINI_GHOST ( int  *scaling,
                  int  *nx,
                  int  *ny,
                  int  *nz,
                  int  *num_vars,
                  int  *percent_sum,
                  int  *num_spikes,
                  int  *num_tsteps,
                  int  *stencil,
                  int  *comm_method,
                  int  *bc,
                  int  *error_tol,
                  int  *report_diffusion,
                  int  *npx,
                  int  *npy,
                  int  *npz,
                  int  *report_perf,
                  int  *checkpoint_method,
                  int  *checkpoint_interval,
                  char *checkpoint_file,
                  int  *restart_cp_num,
                  char *restart_file,
                  int  *debug_grid );

int main ( int argc, char** argv )
{

   // Purpose
   // =======
   // Parse command line input and call Fortran MINI_GHOST.

   //  =====================================================================

   // --------------
   //  Local Scalars
   // --------------

   int
      i,
      ierr = 0,
      num_args,
      remainder,
      root_pe = 0,
      tmp_nx,
      tmp_ny,
      tmp_nz;

   Input_params param;

   // ------------
   // Local Arrays
   // ------------

#define count_problem_params 21

   int problem_params[count_problem_params];

   // ---------------------
   // Executable Statements
   // ---------------------

   // Set defaults: Small problem run on single process.

#include "default-settings.h"

#if defined _MG_MPI
   ierr = MPI_Init ( &argc, &argv );

   ierr = MPI_Errhandler_set ( MPI_COMM_WORLD, MPI_ERRORS_ARE_FATAL );

   ierr = MPI_Comm_rank ( MPI_COMM_WORLD, &mype );

   ierr = MPI_Comm_size ( MPI_COMM_WORLD, &numpes );
#else
    mype = 0;
    numpes = 1;
#endif

   tmp_nx = param.nx;
   tmp_ny = param.ny;
   tmp_nz = param.nz;

   param.npx = numpes; // Default preparation.

   if ( mype == 0 ) {
      for ( i=1; i<argc; i++ ) {
            if ( !strcmp ( argv[i], "--scaling" ) )
               param.scaling = atoi ( argv[++i] );
            else if ( !strcmp ( argv[i], "--nx" ) )
               tmp_nx = atoi ( argv[++i] );
            else if ( !strcmp ( argv[i], "--ny" ) )
               tmp_ny = atoi ( argv[++i] );
            else if ( !strcmp ( argv[i], "--nz" ) )
               tmp_nz = atoi ( argv[++i] );
            else if ( !strcmp ( argv[i], "--ndim" ) ) {
                  tmp_nx = atoi ( argv[++i] );
                  tmp_ny = tmp_nx;
                  tmp_nz = tmp_nx;
               }
            else if ( !strcmp ( argv[i], "--num_vars" ) )
               param.num_vars = atoi ( argv[++i] );
            else if ( !strcmp ( argv[i], "--percent_sum" ) )
               param.percent_sum = atoi ( argv[++i] );
            else if ( !strcmp ( argv[i], "--num_spikes" ) )
               param.num_spikes = atoi ( argv[++i] );
            else if ( !strcmp ( argv[i], "--num_tsteps" ) )
               param.num_tsteps = atoi ( argv[++i] );
            else if ( !strcmp ( argv[i], "--stencil" ) )
               param.stencil = atoi ( argv[++i] );
            else if ( !strcmp ( argv[i], "--comm_method" ) )
               param.comm_method = atoi ( argv[++i] );
            else if ( !strcmp ( argv[i], "--bc" ) )
               param.bc = atoi ( argv[++i] );
            else if ( !strcmp ( argv[i], "--error_tol" ) )
               param.error_tol = atoi ( argv[++i] );
            else if ( !strcmp ( argv[i], "--report_diffusion" ) )
               param.report_diffusion = atoi ( argv[++i] );
            else if ( !strcmp ( argv[i], "--npx" ) )
               param.npx = atoi ( argv[++i] );
            else if ( !strcmp ( argv[i], "--npy" ) )
               param.npy = atoi ( argv[++i] );
            else if ( !strcmp ( argv[i], "--npz" ) )
               param.npz = atoi ( argv[++i] );
            else if ( !strcmp ( argv[i], "--npdim" ) ) {
                  param.npx = atoi ( argv[++i] );
                  param.npy = param.npx;
                  param.npz = param.npx;
               }
            else if ( !strcmp ( argv[i], "--report_perf" ) )
               param.report_perf = atoi ( argv[++i] );
            else if ( !strcmp ( argv[i], "--checkpoint_method" ) )
               param.checkpoint_method = atoi ( argv[++i] );
            else if ( !strcmp ( argv[i], "--checkpoint_interval" ) )
               param.checkpoint_interval = atoi ( argv[++i] );
            else if ( !strcmp ( argv[i], "--checkpoint_file" ) ) {
               if ((i+1 == argc) || (strlen(argv[i+1])==0)) {
                   fprintf(stderr, "The pathname of the checkpoint file is empty.  Aborting.\n");
                   print_help_message ();
               }
               if (strlen(argv[i+1])>1023) {
                   fprintf(stderr, "The pathname of the checkpoint file is too long.  It is limited to 1023 characters.  Aborting.\n");
                   print_help_message ();
               }
               strcpy( param.checkpoint_file, argv[++i] );
            }
            else if ( !strcmp ( argv[i], "--restart_cp_num" ) )
               param.restart_cp_num = atoi ( argv[++i] );
            else if ( !strcmp ( argv[i], "--restart_file" ) ) {
               if ((i+1 == argc) || (strlen(argv[i+1])==0)) {
                   fprintf(stderr, "The pathname of the restart file is empty.  Aborting.\n");
                   print_help_message ();
               }
               if (strlen(argv[i+1])>1023) {
                   fprintf(stderr, "The pathname of the restart file is too long.  It is limited to 1023 characters.  Aborting.\n");
                   print_help_message ();
               }
               strcpy( param.restart_file, argv[++i] );
            }
            else if ( !strcmp ( argv[i], "--debug_grid" ) )
               param.debug_grid = atoi ( argv[++i] );

            else if ( !strcmp ( argv[i], "--help" ) ) {
                  print_help_message ();
                  MG_Assert ( -1, "Listing of command line options requested to be printed to stdio" );
               }
            else {
                  fprintf ( stderr, "\n\n ** Error ** Unknown input parameter %s \n", argv[i] );
                  print_help_message ();
                  MG_Assert ( -1, "Unknown input parameter." );

               } // End parsing command line.
         }

         switch ( param.scaling )
         {
            case ( SCALING_WEAK ):

               param.nx = tmp_nx;
               param.ny = tmp_ny;
               param.nz = tmp_nz;

               break;

            case ( SCALING_STRONG ):

               param.nx = tmp_nx / param.npx;
               remainder = tmp_nx % param.npx;
               if ( mype < remainder )
                  param.nx++;

               param.ny = tmp_ny / param.npy;
               remainder = tmp_ny % param.npy;
               if ( mype < remainder )
                  param.ny++;

               param.nz = tmp_nz / param.npz;
               remainder = tmp_nz % param.npz;
               if ( mype < remainder )
                  param.nz++;

               break;

            default:

               fprintf ( stderr,
                         "\n\n ** Error ** Unknown scaling %d; options are weak (%d) and strong (%d). \n",
                         param.scaling, SCALING_WEAK, SCALING_STRONG );
               MG_Assert ( -1, "Unknown scaling option" );

         } // End switch  ( param.scaling )

         problem_params[0]  = param.scaling;
         problem_params[1]  = param.nx;
         problem_params[2]  = param.ny;
         problem_params[3]  = param.nz;
         problem_params[4]  = param.num_vars;
         problem_params[5]  = param.percent_sum;
         problem_params[6]  = param.num_spikes;
         problem_params[7]  = param.num_tsteps;
         problem_params[8]  = param.stencil;
         problem_params[9]  = param.comm_method;
         problem_params[10] = param.bc;
         problem_params[11] = param.error_tol;
         problem_params[12] = param.report_diffusion;
#if defined _MG_MPI
         problem_params[13] = param.npx;
         problem_params[14] = param.npy;
         problem_params[15] = param.npz;
#elif defined _MG_SERIAL
         problem_params[13] = 1;
         problem_params[14] = 1;
         problem_params[15] = 1;
#endif
         problem_params[16] = param.report_perf;
         problem_params[17] = param.checkpoint_method;
         problem_params[18] = param.checkpoint_interval;
         problem_params[19] = param.restart_cp_num;
         problem_params[20] = param.debug_grid;

         ierr = check_input ( &param );
         MG_Assert ( ierr, "invalid input parameter value" );
      }

#if defined _MG_MPI
   ierr = MPI_Bcast( problem_params, count_problem_params, MPI_INT, root_pe, MPI_COMM_WORLD );
   assert ( ierr == MPI_SUCCESS );
   ierr = MPI_Bcast( param.checkpoint_file, 1024, MPI_CHAR, root_pe, MPI_COMM_WORLD );
   assert ( ierr == MPI_SUCCESS );
   ierr = MPI_Bcast( param.restart_file, 1024, MPI_CHAR, root_pe, MPI_COMM_WORLD );
   assert ( ierr == MPI_SUCCESS );
#endif

   param.scaling       = problem_params[0];

   param.nx                  = problem_params[1];
   param.ny                  = problem_params[2];
   param.nz                  = problem_params[3];
   param.num_vars            = problem_params[4];
   param.percent_sum         = problem_params[5];

   param.num_spikes          = problem_params[6];
   param.num_tsteps          = problem_params[7];

   param.stencil             = problem_params[8];
   param.comm_method         = problem_params[9];

   param.bc                  = problem_params[10];

   param.error_tol           = problem_params[11];
   param.report_diffusion    = problem_params[12];

   param.npx                 = problem_params[13];
   param.npy                 = problem_params[14];
   param.npz                 = problem_params[15];

   param.report_perf         = problem_params[16];

   param.checkpoint_method   = problem_params[17];
   param.checkpoint_interval = problem_params[18];
   param.restart_cp_num      = problem_params[19];

   param.debug_grid          = problem_params[20];

   ierr = check_input ( &param );
   MG_Assert ( ierr, "illegal input parameter(s)" );

   MINI_GHOST (
                &param.scaling,
                &param.nx,
                &param.ny,
                &param.nz,
                &param.num_vars,
                &param.percent_sum,
                &param.num_spikes,
                &param.num_tsteps,
                &param.stencil,
                &param.comm_method,
                &param.bc,
                &param.error_tol,
                &param.report_diffusion,
                &param.npx,
                &param.npy,
                &param.npz,
                &param.report_perf,
                &param.checkpoint_method,
                &param.checkpoint_interval,
                &(param.checkpoint_file[0]),
                &param.restart_cp_num,
                &(param.restart_file[0]),
                &param.debug_grid );

#if defined _MG_MPI
   MPI_Finalize ( );
#endif

   exit(0);

} // End main.

// ======================================== Utilities ======================================

// =================================== print_help_message ==================================

void print_help_message ()
   {
      fprintf ( stderr, "\n\n (Optional) command line input is of the form: \n\n" );

      fprintf ( stderr, "--stencil \n" );

#if !defined _MG_SERIAL
      fprintf ( stderr, "--comm_method \n" );
      fprintf ( stderr, "--bc \n" );
      fprintf ( stderr, "--scaling\n" );
#endif

      fprintf ( stderr, "--nx  ( > 0 )\n" );
      fprintf ( stderr, "--ny  ( > 0 )\n" );
      fprintf ( stderr, "--nz  ( > 0 )\n" );
      fprintf ( stderr, "--ndim : for cubes, i.e. nx=ny=nz.  ( > 0 )\n\n" );

      fprintf ( stderr, "--num_vars (0 < num_vars <= 40)\n" );
      fprintf ( stderr, "--num_tsteps ( > 0 )\n" );

      fprintf ( stderr, "--error_tol ( e^{-error_tol} ; >= 0) \n" );
      fprintf ( stderr, "--report_diffusion (>= 0) \n" );

      fprintf ( stderr, "--percent_sum (0 through 100) \n" );

      fprintf ( stderr, "--report_perf : 0, 1, 2\n" );

      fprintf ( stderr, "--debug_grid ( 0, 1 )\n\n" );

#if !defined _MG_SERIAL
      fprintf ( stderr, "--npx ( 0 < npx <= numpes )\n" );
      fprintf ( stderr, "--npy ( 0 < npy <= numpes )\n" );
      fprintf ( stderr, "--npz ( 0 < npz <= numpes )\n" );
      fprintf ( stderr, "--npdim : for cubes, i.e. npx=npy=npz. ( npdim^3 = numpes )\n" );
#endif

      fprintf ( stderr, " \n\n The following checkpoint options require conditional compilation.\n\n" );
      fprintf ( stderr, "--checkpoint_method \n" );
      fprintf ( stderr, "--checkpoint_interval \n" );
      fprintf ( stderr, "--checkpoint_file \n\n" );
      fprintf ( stderr, "--restart_cp_num \n" );
      fprintf ( stderr, "--restart_file \n\n" );

      fprintf ( stderr, "All associated settings are integers except --checkpoint_file and --restart_file. \n" );
      fprintf ( stderr, "See MG_OPTIONS.F for listing of options.\n\n");

#if defined _MG_MPI
      MPI_Abort ( -1, MPI_COMM_WORLD );
      exit(0);
#elif defined _MG_SERIAL
      exit(0);
#else
      Communication protocol not defined in makefile. ( -D_MPI or -D_SERIAL).
#endif
   }

// ======================================= check_input =====================================

int check_input ( Input_params *param )
   {
      int num_input_err = 0;
      if ( param->nx <= 0 ) {
         num_input_err++;
         fprintf ( stderr, "[pe %d] ** Input error **: nx %d <= 0. \n", mype, param->nx );
      }
      if ( param->ny <= 0 ) {
         num_input_err++;
         fprintf ( stderr, "[pe %d] ** Input error **: ny %d <= 0. \n", mype, param->ny );
      }
      if ( param->nz <= 0 ) {
         num_input_err++;
         fprintf ( stderr, "[pe %d] ** Input error **: nz %d <= 0. \n", mype, param->nz );
         }
      if ( param->stencil != 20 &&
           param->stencil != 21 &&
           param->stencil != 22 &&
           param->stencil != 23 &&
           param->stencil != 24 ) {
         num_input_err++;
         fprintf ( stderr, "[pe %d] ** Input error **: Unknown stencil option (%d). \n", mype, param->stencil );
      }
      if ( param->num_vars <= 0 ) {
         num_input_err++;
         fprintf ( stderr, "[pe %d] ** Input error **: num_vars %d <= 0. \n", mype, param->num_vars );
      }
      if ( param->num_tsteps < 1 ) {
         num_input_err++;
         fprintf ( stderr, "[pe %d] ** Input error **: num_tsteps %d < 1. \n", mype, param->num_tsteps );
      }
      if ( param->num_spikes < 1 ) {
         num_input_err++;
         fprintf ( stderr, "[pe %d] ** Input error **: num_spikes %d < 1. \n", mype, param->num_spikes );
      }
      if ( param->error_tol < 0 ) {
         num_input_err++;
         fprintf ( stderr, "[pe %d] ** Input error **: error_tol %d < 1. \n", mype, param->error_tol );
      }
      if ( param->report_diffusion < 0 ) {
         num_input_err++;
         fprintf ( stderr, "[pe %d] ** Input error **: report_diffusion %d < 0. \n", mype, param->report_diffusion );
      }
      if ( param->percent_sum < 0 || param->percent_sum > 100 ) {
         num_input_err++;
         fprintf ( stderr, "[pe %d] ** Input error **: percent_sum out of range: %d. \n", mype, param->percent_sum );
      }
      if ( param->report_perf != 0 && param->report_perf != 1 && param->report_perf != 2 ) {
         num_input_err++;
         fprintf ( stderr, "[pe %d] ** Input error **: report_perf %d must be either (0,1,2). \n", mype, param->report_perf );
      }
      if ( param->debug_grid != 0 && param->debug_grid != 1 ) {
         num_input_err++;
         fprintf ( stderr, "[pe %d] ** Input error **: debug_grid %d must be either (0,1). \n", mype, param->debug_grid );
      }
      if ( param->npx < 1 ) {
         num_input_err++;
         fprintf ( stderr, "[pe %d] ** Input error **: npx %d < 1. \n", mype, param->npx );
      }
      if ( param->npy < 1 ) {
         num_input_err++;
         fprintf ( stderr, "[pe %d] ** Input error **: npy %d < 1. \n", mype, param->npy );
      }
      if ( param->npz < 1 ) {
         num_input_err++;
         fprintf ( stderr, "[pe %d] ** Input error **: npz %d < 1. \n", mype, param->npz );
      }
      if ( param->npx*param->npy*param->npz != numpes ) {
         num_input_err++;
         fprintf ( stderr,
                   "[pe %d] ** Input error **: npx*npy*npz not equal to numpes; (npx, npy, npz) = (%d, %d, %d).\n",
                    mype, param->npx, param->npy, param->npz );
      }
      if ( param->checkpoint_method != 1 &&
           param->checkpoint_method != 2 ) {
         num_input_err++;
         fprintf ( stderr, "[pe %d] ** Input error **: Unknown checkpoint method (%d). \n", mype, param->checkpoint_method );
      }
      if ( param->checkpoint_interval < 0) {
         num_input_err++;
         fprintf ( stderr, "[pe %d] ** Input error **: checkpoint_interval %d < 0.\n", mype, param->checkpoint_interval );
      }
      if ( param->debug_grid < 0 ) {
         num_input_err++;
         fprintf ( stderr, "[pe %d] ** Input error **: debug_grid %d < 0. \n", mype, param->debug_grid );
      }

      return ( num_input_err );

   } // End check_input.

// ======================================== MG_Assert ======================================

void MG_Assert ( int ierr, char *error_msg )
{

#if defined _MG_MPI
   if ( ierr != MPI_SUCCESS ) {
      fprintf (stderr, "[pe %d] ** Error ** %s. \n", mype, error_msg );
      MPI_Abort ( MPI_COMM_WORLD, -1 );
   }
#else
   if ( ierr != 0 ) {
      fprintf (stderr, " ** Error ** %s. \n", error_msg );
      exit(-1);
   }
#endif

}
// End main.c
