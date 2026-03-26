
PROGRAM DRIVER

   // PURPOSE
   // =======
   // PARSE COMMAND LINE INPUT AND CALL FORTRAN MINI_GHOST.

   //  =====================================================================

   // --------------
   //  LOCAL SCALARS
   // --------------

   INT
      I,               &
      CALL 0,        &
      NUM_ARGS,        &
      REMAINDER,       &
      ROOT_PE = 0,     &
      TMP_NX,          &
      TMP_NY,          &
      TMP_NZ


   // ------------
   // LOCAL ARRAYS
   // ------------

#DEFINE COUNT_PROBLEM_PARAMS 20

   INT PROBLEM_PARAMS ( COUNT_PROBLEM_PARAMS )

   // ---------------------
   // EXECUTABLE STATEMENTS
   // ---------------------

   // SET DEFAULTS: SMALL PROBLEM RUN ON SINGLE PROCESS.

#IF DEFINED _MG_MPI
   CALL MPI_INIT ( IERR )

   CALL MPI_ERRHANDLER_SET ( MPI_COMM_WORLD, MPI_ERRORS_ARE_FATAL, IERR )

   CALL MPI_COMM_RANK ( MPI_COMM_WORLD, &MYPE, IERR )

   CALL MPI_COMM_SIZE ( MPI_COMM_WORLD, &NUMPES, IERR )
#ELSE
    MYPE = 0
    NUMPES = 1
#ENDIF

   TMP_NX = PARAM.NX
   TMP_NY = PARAM.NY
   TMP_NZ = PARAM.NZ

   PARAM.NPX = NUMPES // DEFAULT PREPARATION.

   PARAM.CHECKPOINT_INTERVAL=0
   PARAM.CHECKPOINT_FILE[0)='\0'
   PARAM.RESTART_CP_NUM=-1
   PARAM.RESTART_FILE[0)='\0'

   DO I = 1, IARGC()
      CALL GETARG( I, ARG )
   END DO

   IF ( MYPE == 0 ) THEN
      DO I = 1, IARGC()
         CALL GETARG( I, ARG )
      END DO
      DO I = 1, IARGC()
         IF ( LSAME ( ARG ( I ), '--SCALING' ) THEN
            I = I + 1
            PARAM.SCALING = ATOI ( ARG ( I ) )
         ELSE IF ( LSAME ( ARG ( I ), '--NX' ) THEN
            I = I + 1
            TMP_NX = ATOI ( ARG ( I ) )
         ELSE IF ( LSAME ( ARG ( I ), '--NY' ) THEN
            I = I + 1
            TMP_NY = ATOI ( ARG ( I ) )
         ELSE IF ( LSAME ( ARG ( I ), '--NZ' ) THEN
            I = I + 1
            TMP_NZ = ATOI ( ARG ( I ) )
         ELSE IF ( LSAME ( ARG ( I ), '--NDIM' ) THENTHEN
            I = I + 1
               TMP_NX = ATOI ( ARG ( I ) )
               TMP_NY = TMP_NX
               TMP_NZ = TMP_NX
            }
         ELSE IF ( LSAME ( ARG ( I ), '--NUM_VARS' ) THEN
            I = I + 1
            PARAM.NUM_VARS = ATOI ( ARG ( I ) )
         ELSE IF ( LSAME ( ARG ( I ), '--PERCENT_SUM' ) THEN
            I = I + 1
            PARAM.PERCENT_SUM = ATOI ( ARG ( I ) )
         ELSE IF ( LSAME ( ARG ( I ), '--NUM_SPIKES' ) THEN
            I = I + 1
            PARAM.NUM_SPIKES = ATOI ( ARG ( I ) )
         ELSE IF ( LSAME ( ARG ( I ), '--NUM_TSTEPS' ) THEN
            I = I + 1
            PARAM.NUM_TSTEPS = ATOI ( ARG ( I ) )
         ELSE IF ( LSAME ( ARG ( I ), '--STENCIL' ) THEN
            I = I + 1
            PARAM.STENCIL = ATOI ( ARG ( I ) )
         ELSE IF ( LSAME ( ARG ( I ), '--COMM_METHOD' ) THEN
            I = I + 1
            PARAM.COMM_METHOD = ATOI ( ARG ( I ) )
         ELSE IF ( LSAME ( ARG ( I ), '--BC' ) THEN
            I = I + 1
            PARAM.BC = ATOI ( ARG ( I ) )
         ELSE IF ( LSAME ( ARG ( I ), '--ERROR_TOL' ) THEN
            I = I + 1
            PARAM.ERROR_TOL = ATOI ( ARG ( I ) )
         ELSE IF ( LSAME ( ARG ( I ), '--REPORT_DIFFUSION' ) THEN
            I = I + 1
            PARAM.REPORT_DIFFUSION = ATOI ( ARG ( I ) )
         ELSE IF ( LSAME ( ARG ( I ), '--NPX' ) THEN
            I = I + 1
            PARAM.NPX = ATOI ( ARG ( I ) )
         ELSE IF ( LSAME ( ARG ( I ), '--NPY' ) THEN
            I = I + 1
            PARAM.NPY = ATOI ( ARG ( I ) )
         ELSE IF ( LSAME ( ARG ( I ), '--NPZ' ) THEN
            I = I + 1
            PARAM.NPZ = ATOI ( ARG ( I ) )
         ELSE IF ( LSAME ( ARG ( I ), '--NPDIM' ) THEN
               PARAM.NPX = ATOI ( ARG ( I ) )
               PARAM.NPY = PARAM.NPX
               PARAM.NPZ = PARAM.NPX
            }
         ELSE IF ( LSAME ( ARG ( I ), '--REPORT_PERF' ) THEN
            I = I + 1
            PARAM.REPORT_PERF = ATOI ( ARG ( I ) )
         ELSE IF ( LSAME ( ARG ( I ), '--CHECKPOINT_INTERVAL' ) THEN
            I = I + 1
            PARAM.CHECKPOINT_INTERVAL = ATOI ( ARG ( I ) )
         ELSE IF ( LSAME ( ARG ( I ), '--CHECKPOINT_FILE' ) THEN
            I = I + 1
            IF ((I+1 == ARGC) || (STRLEN(ARG[I+1 )==0)) THEN
            I = I + 1
                WRITE(*,*)'THE PATHNAME OF THE CHECKPOINT FILE IS EMPTY.  ABORTING.\N')
                PRINT_HELP_MESSAGE ()
            }
            IF (STRLEN(ARG[I+1 )>1023) THEN
            I = I + 1
                WRITE(*,*)'THE PATHNAME OF THE CHECKPOINT FILE IS TOO LONG.  IT IS LIMITED TO 1023 CHARACTERS.  ABORTING.\N')
                PRINT_HELP_MESSAGE ()
                  }
                  STRCPY( PARAM.CHECKPOINT_FILE, ARG ( I ) )
               }
               ELSE IF ( LSAME ( ARG ( I ), '--RESTART_CP_NUM' ) THEN
                  I = I + 1
                  PARAM.RESTART_CP_NUM = ATOI ( ARG ( I ) )
               ELSE IF ( LSAME ( ARG ( I ), '--RESTART_FILE' ) THEN
                  I = I + 1
                  IF ((I+1 == ARGC) || (STRLEN(ARG[I+1 )==0)) THEN
                  I = I + 1
                      WRITE(*,*)'THE PATHNAME OF THE RESTART FILE IS EMPTY.  ABORTING.\N')
                      PRINT_HELP_MESSAGE ()
                  }
                  IF (STRLEN(ARG[I+1 )>1023) THEN
                  I = I + 1
                      WRITE(*,*)'THE PATHNAME OF THE RESTART FILE IS TOO LONG.  IT IS LIMITED TO 1023 CHARACTERS.  ABORTING.\N')
                      PRINT_HELP_MESSAGE ()
                  }
                  STRCPY( PARAM.RESTART_FILE, ARG ( I ) )
               }
               ELSE IF ( LSAME ( ARG ( I ), '--DEBUG_GRID' ) )
                  PARAM.DEBUG_GRID = ATOI ( ARG ( I ) )

               ELSE IF ( LSAME ( ARG ( I ), '--HELP' ) THEN
                  I = I + 1
                     CALL PRINT_HELP_MESSAGE ()
                     CALL MG_ASSERT ( -1, 'LISTING OF COMMAND LINE OPTIONS REQUESTED TO BE PRINTED TO STDIO' )
                  }
               ELSE
                  THEN
                     WRITE(*,*) '\N\N ** ERROR ** UNKNOWN INPUT PARAMETER %S \N', ARG ( I ) )
                     PRINT_HELP_MESSAGE ()
                     CALL MG_ASSERT ( -1, 'UNKNOWN INPUT PARAMETER.' )

                  } // END PARSING COMMAND LINE.
            }

         CASE SELECT ( PARAM.SCALING )

               CASE ( SCALING_WEAK ):

                  PARAM.NX = TMP_NX
                  PARAM.NY = TMP_NY
                  PARAM.NZ = TMP_NZ

                  BREAK

               CASE ( SCALING_STRONG ):

                  PARAM.NX = TMP_NX / PARAM.NPX
                  REMAINDER = TMP_NX % PARAM.NPX
                  IF ( MYPE < REMAINDER )
                     PARAM.NX++

                  PARAM.NY = TMP_NY / PARAM.NPY
                  REMAINDER = TMP_NY % PARAM.NPY
                  IF ( MYPE < REMAINDER )
                     PARAM.NY++

                  PARAM.NZ = TMP_NZ / PARAM.NPZ
                  REMAINDER = TMP_NZ % PARAM.NPZ
                  IF ( MYPE < REMAINDER )
                     PARAM.NZ++

                  BREAK

               DEFAULT:

                  WRITE(*,*)
                            '\N\N ** ERROR ** UNKNOWN SCALING %D OPTIONS ARE WEAK (%D) AND STRONG (%D). \N',
                            PARAM.SCALING, SCALING_WEAK, SCALING_STRONG )
                  CALL MG_ASSERT ( -1, 'UNKNOWN SCALING OPTION' )

            } // END SWITCH  ( PARAM.SCALING )

         PROBLEM_PARAMS (  0 ) = PARAM.SCALING
         PROBLEM_PARAMS (  1 ) = PARAM.NX
         PROBLEM_PARAMS (  2 ) = PARAM.NY
         PROBLEM_PARAMS (  3 ) = PARAM.NZ
         PROBLEM_PARAMS (  4 ) = PARAM.NUM_VARS
         PROBLEM_PARAMS (  5 ) = PARAM.PERCENT_SUM
         PROBLEM_PARAMS (  6 ) = PARAM.NUM_SPIKES
         PROBLEM_PARAMS (  7 ) = PARAM.NUM_TSTEPS
         PROBLEM_PARAMS (  8 ) = PARAM.STENCIL
         PROBLEM_PARAMS (  9 ) = PARAM.COMM_METHOD
         PROBLEM_PARAMS ( 10 ) = PARAM.ERROR_TOL
         PROBLEM_PARAMS ( 11 ) = PARAM.BC
         PROBLEM_PARAMS ( 12 ) = PARAM.REPORT_DIFFUSION
#IF DEFINED _MG_MPI
         PROBLEM_PARAMS ( 13 ) = PARAM.NPX
         PROBLEM_PARAMS ( 14 ) = PARAM.NPY
         PROBLEM_PARAMS ( 15 ) = PARAM.NPZ
#ELIF DEFINED _MG_SERIAL
         PROBLEM_PARAMS ( 13 ) = 1
         PROBLEM_PARAMS ( 14 ) = 1
         PROBLEM_PARAMS ( 15 ) = 1
#ENDIF
         PROBLEM_PARAMS ( 16 ) = PARAM.REPORT_PERF
         PROBLEM_PARAMS ( 17 ) = PARAM.CHECKPOINT_INTERVAL
         PROBLEM_PARAMS ( 18 ) = PARAM.RESTART_CP_NUM
         PROBLEM_PARAMS ( 19 ) = PARAM.DEBUG_GRID

         CALL CHECK_INPUT ( &PARAM )
         CALL MG_ASSERT ( IERR, 'ILLEGAL INPUT PARAMETER(S)' )
      }

#IF DEFINED _MG_MPI
   CALL MPI_BCAST( PROBLEM_PARAMS, COUNT_PROBLEM_PARAMS, MPI_INT, ROOT_PE, MPI_COMM_WORLD, IERR )
   CALL MG_ASSERT ( IERR == MPI_SUCCESS, MPI_BCAST ( PROBLEM_PARAMS ) )

   CALL MPI_BCAST( PARAM.CHECKPOINT_FILE, 1024, MPI_CHAR, ROOT_PE, MPI_COMM_WORLD, IERR )
   CALL MG_ASSERT ( IERR == MPI_SUCCESS )

   CALL MPI_BCAST( PARAM.RESTART_FILE, 1024, MPI_CHAR, ROOT_PE, MPI_COMM_WORLD, IERR )
   CALL MG_ASSERT ( IERR == MPI_SUCCESS )
#ENDIF

   PARAM.SCALING             = PROBLEM_PARAMS ( 0 )

   PARAM.NX                  = PROBLEM_PARAMS ( 1 )
   PARAM.NY                  = PROBLEM_PARAMS ( 2 )
   PARAM.NZ                  = PROBLEM_PARAMS ( 3 )
   PARAM.NUM_VARS            = PROBLEM_PARAMS ( 4 )
   PARAM.PERCENT_SUM         = PROBLEM_PARAMS ( 5 )

   PARAM.NUM_SPIKES          = PROBLEM_PARAMS ( 6 )
   PARAM.NUM_TSTEPS          = PROBLEM_PARAMS ( 7 )

   PARAM.STENCIL             = PROBLEM_PARAMS ( 8 )
   PARAM.COMM_METHOD         = PROBLEM_PARAMS ( 9 )

   PARAM.ERROR_TOL           = PROBLEM_PARAMS ( 10 )

   PARAM.BC                  = PROBLEM_PARAMS ( 11 )
   PARAM.REPORT_DIFFUSION    = PROBLEM_PARAMS ( 12 )

   PARAM.NPX                 = PROBLEM_PARAMS ( 13 )
   PARAM.NPY                 = PROBLEM_PARAMS ( 14 )
   PARAM.NPZ                 = PROBLEM_PARAMS ( 15 )

   PARAM.REPORT_PERF         = PROBLEM_PARAMS ( 16 )

   PARAM.CHECKPOINT_INTERVAL = PROBLEM_PARAMS ( 17 )
   PARAM.RESTART_CP_NUM      = PROBLEM_PARAMS ( 18 )

   PARAM.DEBUG_GRID          = PROBLEM_PARAMS ( 19 )

   CALL CHECK_INPUT ( &PARAM )
   CALL MG_ASSERT ( IERR, 'ILLEGAL INPUT PARAMETER(S)' )

#IF DEFINED _MG_MPI
   CALL MPI_FINALIZE ( )
#ENDIF


// ======================================== UTILITIES ======================================

// =================================== PRINT_HELP_MESSAGE ==================================

VOID PRINT_HELP_MESSAGE ()
   THEN
      WRITE(*,*) '\N\N (OPTIONAL) COMMAND LINE INPUT IS OF THE FORM: \N\N' )

      WRITE(*,*) '--STENCIL \N' )

#IF !DEFINED _MG_SERIAL
      WRITE(*,*) '--COMM_METHOD \N' )
      WRITE(*,*) '--SCALING\N' )
#ENDIF

      WRITE(*,*) '--NX  ( > 0 )\N' )
      WRITE(*,*) '--NY  ( > 0 )\N' )
      WRITE(*,*) '--NZ  ( > 0 )\N' )
      WRITE(*,*) '--NDIM : FOR CUBES, I.E. NX=NY=NZ.  ( > 0 )\N\N' )

      WRITE(*,*) '--BC (BC_DIRICHLET)'

      WRITE(*,*) '--NUM_VARS (0 < NUM_VARS <= 40)\N' )
      WRITE(*,*) '--NUM_TSTEPS ( > 0 )\N' )

      WRITE(*,*) '--ERROR_TOL ( E^THEN-ERROR_TOL}  >= 0) \N' );
      WRITE(*,*) '--REPORT_DIFFUSION (>= 0) \N' )

      WRITE(*,*) '--PERCENT_SUM (0 THROUGH 100) \N' )

      WRITE(*,*) '--REPORT_PERF : 0, 1, 2\N' )

      WRITE(*,*) '--DEBUG_GRID ( 0, 1 )\N\N' )

#IF !DEFINED _MG_SERIAL
      WRITE(*,*) '--NPX ( 0 < NPX <= NUMPES )\N' )
      WRITE(*,*) '--NPY ( 0 < NPY <= NUMPES )\N' )
      WRITE(*,*) '--NPZ ( 0 < NPZ <= NUMPES )\N' )
      WRITE(*,*) '--NPDIM : FOR CUBES, I.E. NPX=NPY=NPZ. ( NPDIM^3 = NUMPES )\N' )
#ENDIF

      WRITE(*,*) ' \N\N THE FOLLOWING CHECKPOINT OPTIONS REQUIRE CONDITIONAL COMPILATION.\N\N' )
      WRITE(*,*) '--CHECKPOINT_INTERVAL \N' )
      WRITE(*,*) '--CHECKPOINT_FILE \N\N' )
      WRITE(*,*) '--RESTART_CP_NUM \N' )
      WRITE(*,*) '--RESTART_FILE \N\N' )

      WRITE(*,*) 'ALL ASSOCIATED SETTINGS ARE INTEGERS EXCEPT --CHECKPOINT_FILE AND --RESTART_FILE. \N' )
      WRITE(*,*) 'SEE MG_OPTIONS.F FOR LISTING OF OPTIONS.\N\N')

#IF DEFINED _MG_MPI
      MPI_ABORT ( -1, MPI_COMM_WORLD )
      EXIT(0)
#ELIF DEFINED _MG_SERIAL
      EXIT(0)
#ELSE
      COMMUNICATION PROTOCOL NOT DEFINED IN MAKEFILE. ( -D_MPI OR -D_SERIAL).
#ENDIF
   }

// ======================================= CHECK_INPUT =====================================

INT CHECK_INPUT ( INPUT_PARAMS *PARAM )
   THEN
      INT NUM_INPUT_ERR = 0
      IF ( PARAM->NX <= 0 )
         THEN
            NUM_INPUT_ERR++
            WRITE(*,*) '[PE %D] ** INPUT ERROR **: NX %D <= 0. \N', MYPE, PARAM->NX )
         }
      IF ( PARAM->NY <= 0 )
         THEN
            NUM_INPUT_ERR++
            WRITE(*,*) '[PE %D] ** INPUT ERROR **: NY %D <= 0. \N', MYPE, PARAM->NY )
         }
      IF ( PARAM->NZ <= 0 )
         THEN
            NUM_INPUT_ERR++
            WRITE(*,*) '[PE %D] ** INPUT ERROR **: NZ %D <= 0. \N', MYPE, PARAM->NZ )
            }
      IF ( PARAM->NUM_VARS <= 0 )
         THEN
            NUM_INPUT_ERR++
            WRITE(*,*) '[PE %D] ** INPUT ERROR **: NUM_VARS %D <= 0. \N', MYPE, PARAM->NUM_VARS )
         }
      IF ( PARAM->NUM_TSTEPS < 1 )
         THEN
            NUM_INPUT_ERR++
            WRITE(*,*) '[PE %D] ** INPUT ERROR **: NUM_TSTEPS %D < 1. \N', MYPE, PARAM->NUM_TSTEPS )
         }
      IF ( PARAM->ERROR_TOL < 0 )
         THEN
            NUM_INPUT_ERR++
            WRITE(*,*) '[PE %D] ** INPUT ERROR **: ERROR_TOL %D < 1. \N', MYPE, PARAM->ERROR_TOL )
         }
      IF ( PARAM->REPORT_DIFFUSION < 0 )
         THEN
            NUM_INPUT_ERR++
            WRITE(*,*) '[PE %D] ** INPUT ERROR **: REPORT_DIFFUSION %D < 0. \N', MYPE, PARAM->REPORT_DIFFUSION )
         }
      IF ( PARAM->PERCENT_SUM < 0 || PARAM->PERCENT_SUM > 100 )
         THEN
            NUM_INPUT_ERR++
            WRITE(*,*) '[PE %D] ** INPUT ERROR **: PERCENT_SUM OUT OF RANGE %D. \N', MYPE, PARAM->PERCENT_SUM )
         }
      IF ( PARAM->REPORT_PERF != 0 && PARAM->REPORT_PERF != 1 && PARAM->REPORT_PERF != 2 )
         THEN
            NUM_INPUT_ERR++
            WRITE(*,*) '[PE %D] ** INPUT ERROR **: REPORT_PERF %D MUST BE EITHER (0,1,2). \N', MYPE, PARAM->REPORT_PERF )
         }
      IF ( PARAM->DEBUG_GRID != 0 && PARAM->DEBUG_GRID != 1 )
         THEN
            NUM_INPUT_ERR++
            WRITE(*,*) '[PE %D] ** INPUT ERROR **: DEBUG_GRID %D MUST BE EITHER (0,1). \N', MYPE, PARAM->DEBUG_GRID )
         }
      IF ( PARAM->NPX < 1 )
         THEN
            NUM_INPUT_ERR++
            WRITE(*,*) '[PE %D] ** INPUT ERROR **: NPX %D < 1. \N', MYPE, PARAM->NPX )
         }
      IF ( PARAM->NPY < 1 )
         THEN
            NUM_INPUT_ERR++
            WRITE(*,*) '[PE %D] ** INPUT ERROR **: NPY %D < 1. \N', MYPE, PARAM->NPY )
         }
      IF ( PARAM->NPZ < 1 )
         THEN
            NUM_INPUT_ERR++
            WRITE(*,*) '[PE %D] ** INPUT ERROR **: NPZ %D < 1. \N', MYPE, PARAM->NPZ )
         }
      IF ( PARAM->NPX*PARAM->NPY*PARAM->NPZ != NUMPES )
         THEN
            NUM_INPUT_ERR++
            WRITE(*,*)
                      '[PE %D] ** INPUT ERROR **: NPX*NPY*NPZ NOT EQUAL TO NUMPES (NPX, NPY, NPZ) = (%D, %D, %D).\N',
                       MYPE, PARAM->NPX, PARAM->NPY, PARAM->NPZ )
         }
      IF ( PARAM->CHECKPOINT_INTERVAL < 0)
         THEN
            NUM_INPUT_ERR++
            WRITE(*,*) '[PE %D] ** INPUT ERROR **: CHECKPOINT_INTERVAL %D < 0.\N', MYPE, PARAM->CHECKPOINT_INTERVAL )
         }

      IF ( PARAM->DEBUG_GRID < 0 )
         THEN
            NUM_INPUT_ERR++
            WRITE(*,*) '[PE %D] ** INPUT ERROR **: DEBUG_GRID %D < 0. \N', MYPE, PARAM->DEBUG_GRID )
         }

      RETURN ( NUM_INPUT_ERR )

   END

// ======================================== CALL MG_ASSERT ======================================

INTEGER FUNCTION CALL MG_ASSERT ( INT IERR, CHAR *ERROR_MSG )
THEN

#IF DEFINED _MG_MPI
   IF ( IERR != MPI_SUCCESS ) THEN
      WRITE(*,*)'[PE %D] ** ERROR ** %S. \N', MYPE, ERROR_MSG )
      MPI_ABORT ( MPI_COMM_WORLD, -1 )
   }
#ELSE
   IF ( IERR != 0 ) THEN
      WRITE(*,*)' ** ERROR ** %S. \N', ERROR_MSG )
      EXIT(-1)
   }
#ENDIF

}


// END MAIN.C
