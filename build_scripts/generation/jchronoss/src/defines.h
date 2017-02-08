/****************************************************************************/
/*                                                                          */
/*                         Copyright or (C) or Copr.                        */
/*       Commissariat a l'Energie Atomique et aux Energies Alternatives     */
/*                                                                          */
/* Version : 2.0                                                            */
/* Date    : Tue Jul 22 13:28:10 CEST 2014                                  */
/* Ref ID  : IDDN.FR.001.160040.000.S.P.2015.000.10800                      */
/* Author  : Julien Adam <julien.adam@cea.fr>                               */
/*           Marc Perache <marc.perache@cea.fr>                             */
/*                                                                          */
/* This file is part of JCHRONOSS software.                                 */
/*                                                                          */
/* This software is governed by the CeCILL-C license under French law and   */
/* abiding by the rules of distribution of free software.  You can  use,    */
/* modify and/or redistribute the software under the terms of the CeCILL-C  */
/* license as circulated by CEA, CNRS and INRIA at the following URL        */
/* "http://www.cecill.info".                                                */
/*                                                                          */
/* As a counterpart to the access to the source code and  rights to copy,   */
/* modify and redistribute granted by the license, users are provided only  */
/* with a limited warranty  and the software's author,  the holder of the   */
/* economic rights,  and the successive licensors  have only  limited       */
/* liability.                                                               */
/*                                                                          */
/* In this respect, the user's attention is drawn to the risks associated   */
/* with loading,  using,  modifying and/or developing or reproducing the    */
/* software by the user in light of its specific status of free software,   */
/* that may mean  that it is complicated to manipulate,  and  that  also    */
/* therefore means  that it is reserved for developers  and  experienced    */
/* professionals having in-depth computer knowledge. Users are therefore    */
/* encouraged to load and test the software's suitability as regards their  */
/* requirements in conditions enabling the security of their systems and/or */
/* data to be ensured and,  more generally, to use and operate it in the    */
/* same conditions as regards security.                                     */
/*                                                                          */
/* The fact that you are presently reading this means that you have had     */
/* knowledge of the CeCILL-C license and that you accept its terms.         */
/*                                                                          */
/****************************************************************************/


#ifndef DEFINES_H
#define DEFINES_H

/************* INCLUSION *************/
#include <cerrno>

/************************/
/****** FUNC MACROS *****/
/************************/

/** print an error message and exit with given signal **/
#define printError(chain, sig) do{ std::cerr<<COLOR_FAIL" ERROR        : "<< chain <<COLOR_NORM << std::endl; exit(sig); } while(0)

/** print a warning message and continue execution **/
#define printWarning(chain)    do{ std::cerr<<COLOR_URUN" WARNING      : "<< chain <<COLOR_NORM << std::endl;            } while(0)

/** print an info message and continue execution **/
#define printInfo(chain)       do{ std::cerr<<COLOR_INFO" INFO "<<type<<"  : "<< chain <<COLOR_NORM << std::endl;            } while(0)

/** declare a parameter as unused. Useful to avoid warning about unused paramters (like signal handlers...) **/
#define UNUSED(x) (void)(x)                  ///< define an unused paramater

/** macro to free and set to null a dynamic allocation **/
#define safeFree(u) do{if(u != NULL) {delete u; u = NULL;}} while(0)
#define safeTabFree(u) do{if(u != NULL) {delete [] u; u = NULL;}} while(0)

/** only used in testing framework, simplify file opening **/
#define safeOpenRead(u, v)  do{ u = fopen(v, "r"); assert(u != NULL);} while(0)

/** only used in testing framework, simplify file opening **/
#define safeOpenWrite(u, v) do{ u = fopen(v, "w"); assert(u != NULL);} while(0)

/** only used in testing framework, simplify file opening **/
#define safeClose(u) do{fclose(u); u = NULL;}while(0)


/*************************/
/****** VALUE MACROS *****/
/*************************/
#define NB_MS_IN_SEC (1000)                  ///< define the number of milliseconds in a second
#define NB_NS_IN_MS (1000000)                ///< define the number of nanoseconds in a millisecond
#define MAX_RANDOM 5000                      ///< define max random time in millisecond
#define MIN_RANDOM 0                         ///< define min random time
#define VAR_COEFF 0.5                        ///< define variance on random time
#define DEFAULT_MAGIK_NUMBER 42U             ///< define magik number to ensure data coherency
#define DEFAULT_OUTPUT_FILENAME "/output-"   ///< define default prefix filename for output xml file
#define RANDOM_PORT_NUMBER   0               ///< define default port number generation
#define DEFAULT_ONLINE_MODE false
#define CHECKPOINT_PREFIX "jns_chkpt"
#define CHECKPOINT_EXT    ".json"
#define CHECKPOINT_FILENAME "/" + CHECKPOINT_PREFIX + CHECKPOINT_EXT

/*******************/
/****** COLORS *****/
/*******************/
#ifdef ENABLE_COLOR
	#define COLOR_INFO "\033[1;36m" ///< Cyan    : color for info message
	#define COLOR_PASS "\033[1;32m" ///< Green   : color for passed jobs
	#define COLOR_FAIL "\033[1;31m" ///< red     : color for failed jobs
	#define COLOR_NRUN "\033[1;34m" ///< blue    : color for not run yet jobs
	#define COLOR_URUN "\033[1;33m" ///< yellow  : color for not executable jobs
	#define COLOR_DEBG "\033[1;36m" ///< Cyan    : color for debug messages
	#define COLOR_NORM "\033[0;0m"  ///< default : reset color to default
#else
	#define COLOR_INFO "" ///< value when color is disabled
	#define COLOR_PASS "" ///< value when color is disabled
	#define COLOR_FAIL "" ///< value when color is disabled
	#define COLOR_NRUN "" ///< value when color is disabled
	#define COLOR_URUN "" ///< value when color is disabled
	#define COLOR_NORM "" ///< value when color is disabled
	#define COLOR_DEBG "" ///< value when color is disabled
#endif

#define PREFIX_INFO COLOR_INFO" INFO         : "
#define PREFIX_PASS COLOR_PASS" PASSED       : "
#define PREFIX_FAIL COLOR_FAIL" FAILED       : "
#define PREFIX_NRUN COLOR_NRUN" NOT RUN      : "
#define PREFIX_URUN COLOR_URUN" NOT RUNNABLE : "
#define PREFIX_DEBG COLOR_DEBG" DEBUG        : "

#ifdef ENABLE_PROFILING
	#define TIME_MEASURE_INIT() measureTimeInterval(true)
	#define TIME_MEASURE_END(u) measureTimeInterval(false, u)
#else
	#define TIME_MEASURE_INIT()
	#define TIME_MEASURE_END(v)
#endif

#ifdef NDEBUG
	#define CHECK(u) u
	#define printDebug(chain)
#else
	#define CHECK(u) assert(u)
	#define printDebug(chain) do{ std::cout<<COLOR_DEBG" DEBUG        : "<< chain <<COLOR_NORM << endl;          } while(0)
#endif

/***********************/
/****** FORMATTING *****/
/***********************/
#define PADDED_HELP setw(40) << left
#define PADDED_OPT  setw(35) << left

/*************************/
/****** RETURN CODES *****/
/*************************/
#define JE_NKWN_OPT 73   ///< unknown option                 ! REPLACE EDOTDOT     !
#define JE_NSET_RES 45   ///< resources number not set       ! REPLACE EL2NSYNC    !
#define JE_NFND_FIL 46   ///< file not found                 ! REPLACE EL3HLT      !
#define JE_NFND_JOB 47   ///< job not found                  ! REPLACE EL3HLT      !
#define JE_INVA_VAL 48   ///< Invalid value                  ! REPLACE ELNRNG      !
#define JE_NEX_PATH 50   ///< path doesn't exist             ! REPLACE ENOCSI      !
#define JE_LACK_RES 64   ///< Need more resources to execute ! REPLACE ENONET      !
#define JE_LACK_TIM 69   ///< Need more time to execute      ! REPLACE ESRMNT      !
#define JE_WORK_INP 85   ///< false error : marked as WIP    ! REPLACE ERESTART    !
#define JE_EXEC_WRK 115  ///< Unable to start to work        ! REPLACE EINPROGRESS !
#define JE_UNKNOWN  118  ///< Unknown error                  ! REPLACE ENOTNAM     !

#endif
