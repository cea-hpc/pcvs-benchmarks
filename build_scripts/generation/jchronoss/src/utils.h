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


#ifndef UTILS_H
#define UTILS_H

/************* INCLUSION *************/
/******* C LIBS ******/
#include <cstdio>
#include <libgen.h>
#include <cstring>
#include <cstdlib>
#include <cassert>
#include <cstdarg>
/****** C++ LIBS *****/
#include <iostream>
#include <sstream>
#include <fstream>
#include <string>
#include <iomanip>
#include <vector>
#include <list>
#include <functional>
/****** UNIX LIBS *****/
#include <getopt.h>
#include <sys/time.h>
#include <unistd.h>
#include <sys/types.h>
#include <sys/wait.h>
#include <sys/stat.h>
#include <fcntl.h>
#include <signal.h>
/****** MY LIBS *****/
#include "defines.h"

extern std::string type;

/**
 * Struct which gathers global information about the whole run and print them
 */
typedef struct Summary_s {
	size_t nbJobs;        ///< number of jobs provided by input
	size_t nbSuccess;     ///< number of succeeded jobs
	size_t nbErrors;      ///< number of errors during scheduling
	size_t nbFailed;      ///< number of failed jobs
	size_t nbSkipped;     ///< number of not-scheduled jobs
	size_t nbTotalSlaves; ///< total number of run slaves
	time_t startRun;      ///< Start time (init runnerMaster)
	time_t endRun;        ///< end time (just before summary print)
	double elapsed;       ///< elapsed time with interruption (before restart)
} Summary;

/************* FUNCTIONS *************/
/// get current timestamp
/**
 * Using gettimeofday(), return number of seconds to
 * express current time
 */
double getCurrentDate();
/// print a line on screen according to prefix
/**
 * Prefix used to uniform printing method but differentiate
 * color format to apply to the output
 * \param[in] prefix the prefix to apply on the output
 * \param[in] format the formatted chain
 * \param[in] ... variable args list to print into formatted chain
 */
void printLine ( std::string prefix, std::string format, ... );
/// read with safety to avoid intempestive interruption
/**
 * When read from pipe, ensure that no interruption could block
 * output logging when the two processes communicates
 * \param[in] fd the pipe's file descriptor where read data
 * \param[in] buf the buffer where data will be read
 * \param[in] size the max size to read in once
 * \return effective number of read bytes
 */
int safeRead(FILE* fd, void* buf, int size);
/// remove from a chain, a defined prefix (used to parse options)
/**
 * \param[in] chain the chain containing the option + prefix
 * \param[in] prefix the prefix to remove
 * \return a string pointer on new option
 */
std::string* splitArgumentPrefix ( std::string chain, std::string prefix );
/// check if an option matches with a defined pattern
/**
 * checking the prefix.size() first characters in chain. if matches, then chain
 * is rattached to this option
 * \param[in] chain the chain to check
 * \param[in] prefix the option
 * \return <b>True</b> if option matches
 * \return <b>False</b> otherwise
 */
bool matchWith (std::string chain,std::string prefix );
/// print JCHRONOSS banner
void banner();
/// escape given string with specific characters patterns
/**
 * \param[in] src the string to escape
 */
void HTMLEncoding(std::string& src);
///print an header helping to identify jobs columns printing
void printHeader();
/// hash the given string with pseudo-unique value
size_t hash_fn(std::string toHash);
/// compute time interval between to call to the function (NOT THREAD SAFE FUNCTION !!!)
void measureTimeInterval(bool init, std::string info = "");
///convert a time in seconds in human-readable format.
/**
 * \param[in] elapsed time to convert
 */
std::string convertDate(double elapsed);
std::string& replace(std::string&, std::string, std::string);


#endif
