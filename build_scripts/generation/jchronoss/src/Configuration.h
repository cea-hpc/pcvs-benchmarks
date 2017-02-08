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

#ifndef CONFIGURATION_H
#define CONFIGURATION_H

#include "utils.h"
#include "FileManager.h"
#include "ConfigurationSystem.h"
#include "ConfigurationJob.h"

/// Enum describing modes as JCHRONOSS can be launched
typedef enum eExecMode{
	EXEC_MASTER = 0,   ///< current execution is the master one
	EXEC_SLAVE= 1,     ///< current execution is the slave on
	EXEC_RESTORED = 2  ///< current execution is a restored session
} execMode; 

/// represents the final JCHRONOSS state 
typedef enum eReturnStatus{
	RETURN_SUCCESS = 0, /**< All the tests have been passed */
	RETURN_WITH_SKIPPED = 1, /**< At least one test has been skipped (should not be raised) */
	RETURN_WITH_FAILURES = 2, /**< At least one test failed */
	RETURN_WITH_ERRORS = 3, /**< At least one test has an error */
	MAX_RETURN_VALUES
} returnStatus;

/// Object Configuration represents the whole configuration for JCHRONOSS
/**
 * this global configuration contains all information about jobs and system
 * configuration (including paths, contents, xml files, scheduling policies)
 * needed by the application. It's in this configuaration that type of execution
 * is setted (MASTER or SLAVE)
 */
class Configuration {
private:
	/************** MEMBERS **************/
	std::string executablePathName;    ///< path name of the current application (needed for recursive call)
	execMode configStatus;             ///< execution status (MASTER or SLAVE)
	SystemConfiguration configSystem;  ///< referent system configuration
	JobConfiguration configJobs;       ///< referent jobs configuration
	std::string* configFile;           ///< used to load file configuration instead of using options
	std::string restoredFile;          ///< used only when JCHRONOSS restarts after interruption : locate JSON file	
	returnStatus finalState;           ///< Will be the final return code for JCHRONOSS master
public:
	/************* FUNCTIONS *************/
	/**** NON-CONST ****/
	/// standard Configuration constructor
	/** Its aims is to configure Configuration object and job/system configuration */
	explicit Configuration();
	
	/// Look for a matching between its options and the given chain
	/** 
	 * When JCHRONOSS parses options from command line, it looks for a match 
	 * between its options and the current one. If there is a matching, option 
	 * is setted and looking for is stopped.
	 * @param[in] arg the given option from the command line
	 * @return <b>true</b> if option matches
	 * @return <b>false</b> otherwise.
	 */
	bool parseGlobalOptions ( char* arg );

	/// Global function to parse the whole command line
	/**
	 * Then program starts, command line's arguments are provided to set the global 
	 * configuration (job+system)
	 * @param[in] argc number of arguments in command line ( = argc from main())
	 * @param[in] argv string array containing command line's arguments (= argv from main())
	 */
	void parseOptions(int argc, char ** argv);
	/**
	 * Re-init the whole configuration (used when JCHORNOSS restarts)
	 * \param[in] exe JCHRONOSS exe path
	 * \param[in] mode JCHRONOSS mode (slave, master...)
	 * \param[in] config configuration file path
	 */
	void setConfiguration(std::string exe, execMode mode, std::string *config);
	/// virtual destructor to unset current configuration
	virtual ~Configuration();
	
	/****** CONST ******/
	/// check if current processus is the master one
	/**
	 * Identifies the current application as the master processus. Used to differentiate
	 * master execution from slave one
	 * @return <b>true</b> if the processus is the master
	 * @return <b>false</b> otherwise
	 */
	bool isMaster() const;
	/**
	 * Check if current process is a restored one (with --restart option)
	 * \return <b>true</b> if the process is a restored on
	 * \return <b>false</b> otherwise
	 */
	bool isRestored() const;
	/// print JCHRONOSS help (--help | -h options)
	void printHelp() const;
	/// print current running configuration 
	/**
	 * This function should be called only in master application (avoiding useless printing)
	 */
	void printConfiguration() const;
	/// get Current JobConfiguration object with its own specifications
	/**
	 * Getting JobConfiguration from global configuration.
	 * @return The current JobConfiguration reference.
	 */
	JobConfiguration& job();
	/// get Current SystemConfiguration object with its own specifications
	/**
	 * Getting SystemConfiguration from global configuration.
	 * @return The current SystemConfiguration reference.
	 */
	SystemConfiguration& system();
	/// exeName getter
	/**
	 * Allow to get path of himself in order to launch himself as a slave later
	 * @return A string containing JCHRONOSS binary file path
	 */
	std::string getExeName() const;
	/// get configuration file
	/**
	 * \return a FileManager pointer on current configuration file
	 * \return <b>NULL</b> if no file are found
	 */
	std::string * getConfigFile() const;
	/**
	 * get backup file
	 * \return a string pointing where .JSON backup file is
	 */
	std::string getRestoredFile() const;
	/// check options coherency
	/**
	 * Check if run would be safe, according to options given by user 
	 */
	void checkOptionsValidity() const;
	/// Set the final return code for JCHRONOSS
	/**
	 * \param[in] st the return code used to set (have to be higher than current)
	 */
	void setReturnStatus(returnStatus st);
	///get the final return code (used by main.cpp)
	/**
	 * \return the final return code
	 */
	returnStatus getReturnStatus() const;
};

#endif // CONFIGURATION_H
