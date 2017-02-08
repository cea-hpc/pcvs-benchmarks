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

#ifndef JOBCONFIGURATION_H
#define JOBCONFIGURATION_H

#include "utils.h"
#include "FileManager.h"
#include "FilterBox.h"
#include "XMLConfigParser.h"
#include "Job.h"

/// Enum defining different verbose levels, i.e. screen displaying
typedef enum eVerboseMode{ 
	VERBOSE_SILENT=0,     ///< No jobs output (except jobs status line)
	VERBOSE_ONLY_ERROR=1, ///< print jobs output from failed jobs
	VERBOSE_ALL=2         ///< Print everything from every jobs
} verboseMode;
/// Enum defining different logging levels, i.e. output jobs kept in file (NOTHING, just failed, just success or everything)
typedef enum eLogIntoFileMode{
	LOG_NONE=0,         ///< keep nothing in JUnit files
	LOG_ONLY_FAILED=1,  ///< keep output from only failed jobs
	LOG_ONLY_SUCCESS=2, ///< keep output from only success jobs
	LOG_ALL=3          ///< keep output whatever jobs state
}logIntoFileMode;

/// Class defining a JobConfiguration, containing the whole information about jobs control
/**
 * A job configuration is a part of the global configuration.
 * In this object, we can found all options and configuration setted 
 * by default or by user and which are about jobs manipulation 
 * (execution, timers, filters, xml files, etc...)
 */
class JobConfiguration {
private:
	/************** MEMBERS **************/
	verboseMode verbosity;                            ///< application verbosity level
	logIntoFileMode logLevel;                         ///< "output conservation in file" level
	bool fakeExecution;                               ///< Defines if jobs have to be simulated (random times --> jobs won't be launched)
	bool longNames;                                   ///< select jobs printing format
	unsigned int maxJobTime;                          ///< Defines the global max time. A job execution can't overflow this max time
	unsigned int nbMaxJobs;                           ///< Represents the max number of jobs to launch in a worker
	std::vector<FileManager*> jobsFilesList;          ///< Represents jobs XML lists
	std::vector<FileManager*> filtersWhiteFileslist;  ///< Represents jobs which have to be kept for the current running
	std::vector<FileManager*> filtersBlackFileslist;  ///< Represents jobs which mustn't be launched in the current execution
	
	
public:
	/************** STATICS **************/
	/****** CONST ******/
	const static size_t DEFAULT_MAX_JOB_TIME = Job::DEFAULT_MAX_JOB_TIME; ///< if no value specified by user, the global max time for a job is this static constant
	const static unsigned int DEFAULT_MAX_NB_JOBS = 0;                    ///< max number of jobs which can be stored in a worker (if policy used it)
	
	/************* FUNCTIONS *************/
	/**** NON-CONST ****/
	/// standard JobConfiguration constructor
	/**
	 * Its aim is to configure JobConfiguration object after a call to
	 * global Configuration object initialization.
	 */
	explicit JobConfiguration();
	///Look for a matching option in its own pattern
	/**
	 * When JCHRONOSS parses options from command line, it looks for a match 
	 * between its options and the current one. If there is a matching, option 
	 * is setted and looking for is stopped and class memer is setted.
	 * \param[in] option the current option from the command line (by getopt)
	 * \return <b>true</b> if an option matches
	 * \return <b>false</b> otherwise
	 */
	bool parseJobOptions(int option);
	///add list of files in string to the current list
	/**
	 * When arguments are parsed, it's possible to provide a string as a list
	 * of files (file1,file2,file3...). This function split each list item and
	 * add it to the given list (generally the list is a class member but as
	 * we want to use one function prototype for different lists, we must 
	 * provide the list reference in parameter)
	 * \param[in] chain The chain containing a comma-separated list
	 * \param[out] list The list were item will be inserted
	 */
	void addFiles(std::string* chain, std::vector<FileManager*>& list);
	///add current argument (a XML file) to the list of jobs XML files list
	/**
	 * if an argument doesn't match with options (neither system, global nor job)
	 * this argument is considered as a job XML input file. This function allows
	 * to add this file to the current list.
	 * \param argv The current chain containing the file path
	 */
	void addJobsFiles ( char* argv );
	///get XML files list (used only if list need to be updated
	/** 
	 * There are no const on this function. Thus, the list reference can be
	 * used to update XML files list
	 * \return A reference on XML files list
	 */
	std::vector<FileManager*>& getJobsFilesNoLock();
	///get XML files white list (used only if list need to be updated
	/** 
	 * There are no const on this function. Thus, the list reference can be
	 * used to update XML files list
	 * \return A reference on XML files list
	 */
	std::vector<FileManager*>& getWhiteFilesNoLock();
	///get XML files black list (used only if list need to be updated
	/** 
	 * There are no const on this function. Thus, the list reference can be
	 * used to update XML files list
	 * \return A reference on XML files list
	 */
	std::vector<FileManager*>& getBlackFilesNoLock();
	/// load configuration file options for current job configuration
	/**
	 * Use a XMLConfigParser to get config options from file
	 * \param[in] parser XMLConfigParser initialized by parent
	 */
	void loadFile(const XMLConfigParser & parser);
	/**
	 * Re-init the whole job configuration (used when jchronoss restarts)
	 * \param[in] verb verbosity level
	 * \param[in] log log level
	 * \param[in] fake fake execution
	 * \param[in] names long or short formatted jobs names
	 * \param[in] jobTime average job time used for scheduling
	 * \param[in] nbJobs total nb jobs in job manager
	 */
	void setJobConfiguration(verboseMode verb, logIntoFileMode log, bool fake, bool names, unsigned int jobTime, unsigned int nbJobs);
	///virtual destructor to unset current JobConfiguration
	virtual ~JobConfiguration();	
	
	/****** CONST ******/
	/// print help menu for current JobConfiguration
	void printHelp () const;
	/// print current JobConfiguration on screen
	void printConfiguration() const;
	/// get verbosity level set by the configuration
	/**
	 * \return verbose value
	 */
	verboseMode getVerbosity() const; 
	/// get log level set by the configuration
	/**
	 * \return loggin level value
	 */
	logIntoFileMode getLogLevel() const;
	/// Check if fake mode is enabled
	/** 
	 * if fake Execution is activated, no jobs will be launched and execution
	 * time will be random-generated (used for scheduling policy tests)
	 * \return <b>True</b> if fake mode is enabled
	 * \return <b>False</b> otherwise
	 */
	bool isFake() const;
	/// Get the current max job time alloweb by the configuration
	/** 
	 * \return The time value
	 */
	unsigned int getMaxJobTime() const;
	///Get Jobs XML files list (const method). 
	/**
	 * If updated are required, you can see getJobsFilesNoLock
	 * \see getJobsFilesNoLock 
	 * \return a const reference on jobs list (unmodifiable) 
	 */
	const std::vector<FileManager*>& getJobsFiles() const;
	/// get XML files list containing jobs which have to be launched
	/**
	 * \return A const reference on the list (unmodifiable)
	 */
	const std::vector<FileManager*>& getFiltersWhiteFiles() const;
	/// get XML files list containing jobs which mustn't be executed
	/**
	 * \return a const reference on the list (unmodifiable)
	 */
	const std::vector<FileManager*>& getFiltersBlackFiles() const;
	/// get the max number of jobs allowed
	/**
	 * \return the value of max number allowed
	 * \return DEFAULT_MAX_NB_JOBS by default
	 */
	unsigned int getNbMaxJobs() const;
	/// get type of job name printing
	/**
	 * \return <b>true</b> if names have to be printed in long-format
	 * \return <b>false</b> otherwise
	 */
	bool wantLongNames() const;
};

#endif // JOBCONFIGURATION_H
