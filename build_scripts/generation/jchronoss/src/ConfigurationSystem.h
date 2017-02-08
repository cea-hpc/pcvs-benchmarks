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

#ifndef SYSTEMCONFIGURATION_H
#define SYSTEMCONFIGURATION_H

#include "utils.h"
#include "FileManager.h"
#include "XMLConfigParser.h"

/// represents differents policies chooses for current running
typedef enum ePolicyMode{
	POLICY_DEFAULT = 0, ///< use DefaultPolicy
	POLICY_RES = 1,     ///< Use SchedByResPolicy
	POLICY_TIME = 2,    ///< Use SchedByTimePolicy
	POLICY_CUSTOM = 3   ///< Use CustomPolicy
} policyMode;
	
/// Class defining a SystemConfiguration, containing information about environment
/**
 * A system configuration is a part of the global configuration.
 * In this object, we can found all options and configuration set 
 * by default or by user and which are about system and environment manipulation 
 * (launchers, directory, resources, autokill,...)
 */
class SystemConfiguration {
private:
	/************** MEMBERS **************/
	std::string * jobsLauncherCommand;            ///< current launcher for jobs
	std::string * compilationJobsLauncherCommand; ///< current compilation launcher for jobs
	std::string * serverLauncherCommand;          ///< specific launcher for remote log server
	std::string * outputDirectory;                ///< where additionnaly files will be stored (traces...)
	std::string * buildDirectory;                 ///< where temporary files will be stored (before erasing)
	size_t nbMaxResources;                        ///< max number of resources allowed
	size_t nbMaxSlaves;                           ///< max number of simultaneous slaves launchs
	unsigned int maxAutokillTime;                 ///< max time before instance killing (in seconds)
	unsigned int maxSlaveTime;                    ///< max time before kill the child instance (in seconds)
	unsigned int minSlaveTime;                    ///< min time to start a slave (avoid wastes)
	policyMode policySelected;                    ///< policy mode selected by user
	size_t flowMaxBytes;                          ///< max bytes to keep in log
	std::vector<std::string> outputFormats;       ///< User-defined list of format to "output"
	std::string* servName;                        ///< remote log server hostname (propagated by master to slaves)
	int frontendPort;                             ///< Listen port for WebSocket interface (frontend)
	int backendPort;                              ///< Listen port for data log polling
	bool online_mode;                             ///< Switch to 1 to enable real-time support
	float interval;                               ///< User-defined data-sending interval by each worker

public:
	/************** STATICS **************/
	/****** CONST ******/
	static const size_t DEFAULT_MAX_RESOURCES = 1;                    ///< if no value set, default number of allowed resources
	static const size_t DEFAULT_SLAVES_VALUE = 0;                     ///< value used to disable the max simultaneous slaves barrier
	static const std::string* DEFAULT_OUTPUT_DIRECTORY;                ///< if no value set, default path to store data (default: ".")
	static const std::string* DEFAULT_BUILD_DIRECTORY;                 ///< if no value set, default path to store temporary files (default ".")
	static const policyMode DEFAULT_POLICY_SELECTED = POLICY_DEFAULT; ///< if not value set, default policy used
	static const unsigned int DEFAULT_MAX_SLAVE_TIME = 65536;         ///< if no value specified by user, global max time allowed to an allocation (infinite by default)
	static const unsigned int DEFAULT_MIN_SLAVE_TIME = 0;             ///< if no value specified by user, global min time allowed to a slave
	static const unsigned int DEFAULT_AUTOKILL = 0;                   ///< if no value specified by user, autokill desactived
	static const unsigned int DEFAULT_FLOW_MAX_BYTES = 0;             ///< if no value specified by user, global max bytes caught from run	
	static const float DEFAULT_ONLINE_INTERVAL;                       ///< if no value specified by user, polling interval for real-time workers
	static const std::vector<std::string> AVAIL_OUTPUT_FORMATS;       ///< possible values for formatting the results (the default used will be the first one)
	/************* FUNCTIONS *************/
	/**** NON-CONST ****/
	///standard SystemConfiguration constructor
	/**
	 * its aim is to construct a SystemConfiguration after Configuration object initialization
	 */
	explicit SystemConfiguration();
	///Look for a matching option in its own pattern
	/**
	 * When JCHRONOSS parses options from command line, it looks for a match 
	 * between its options and the current one. If there is a matching, option 
	 * is setted and looking for is stopped and class memer is setted.
	 * \param[in] option the current option from the command line (by getopt)
	 * \return <b>true</b> if an option matches
	 * \return <b>false</b> otherwise
	 */
	bool parseSystemOptions(int option);
	/// load configuration file options for current system configuration
	/**
	 * Use a XMLConfigParser to get config options from file
	 * \param[in] parser XMLConfigParser initialized by parent
	 */
	void loadFile(const XMLConfigParser & parser);
	/**
	 * set global system configuration (used when jchronoss restarts)
	 * \param[in] launcher launcher path, if exists, NULL otherwise
	 * \param[in] compil compilation launcher path, if exists, NULL otherwise
	 * \param[in] output output path, if exists, NULL otherwise
	 * \param[in] build build path
	 * \param[in] res number of resources
	 * \param[in] slaves number of slaves
	 * \param[in] autokill the autokill
	 * \param[in] minTime min Slave time
	 * \param[in] maxTime max Slave time
	 * \param[in] policy current policy
	 * \param[in] flow flow size
	 * \param[in] online online mode
	 * \param[in] interval online polling interval
	 */
	void setSystemConfiguration(std::string* launcher, std::string* compil, std::string* output, std::string*build, size_t res, size_t slaves, unsigned int autokill, unsigned int minTime, unsigned int maxTime, policyMode policy, size_t flow, bool online, float interval);
	///virtual destructor to unset the current SystemConfiguration
	virtual ~SystemConfiguration();

	/****** CONST ******/
	/// print help menu for current SystemConfiguration
	void printHelp() const;
	/// print current JobConfiguration on screen
	void printConfiguration() const;
	/// get the launcher command
	/**
	 * \return A string containing the current launcher command
	 */
	std::string* getJobsLauncherCommand() const;
	/// get the compilation launcher command
	/**
	 * \return A string containing the current compilation launcher command
	 */
	std::string* getCompilationJobsLauncherCommand() const;
	/// get the server launcher command
	/**
	 * \return A string containing the current server launcher command
	 */
	std::string* getServerLauncherCommand() const;
	///get the output directory for stored data
	/**
	 * \return A string containing the current output directory
	 */
	std::string* getOutputDirectory() const;
	/// get the build directory for temporary files
	/**
	 * \return A string containing the build directory
	 */
	std::string* getBuildDirectory() const;
	/// get the max number of resources allowed by the application
	/**
	 * \return The number of resources
	 */
	size_t getNbMaxResources() const;
	/// get the max number of simultaneous slaves allowed by the application
	/**
	 * \return the number of simultaneous slaves
	 */
	size_t getNbMaxSlaves() const;
	/// get max time to live into instance
	/**
	 * \return the number of seconds for the instance
	 */
	unsigned int getAutokill() const;
	/// get maximum time allowed per slave
	/**
	 * \return number of seconds allowed for a slave sun
	 */
	unsigned int getMaxSlaveTime() const;
	/// get current used policy
	/**
	 * \return policy mode currently used
	 */
	
	policyMode getPolicy() const;
	/// get min time to launch a worker
	/**
	 * \return a double on min slave time
	 */
	unsigned int getMinSlaveTime() const;
	/// get max bytes to keep in log
	/**
	 * \return an unsigned int which is the desired value
         */ 
	size_t getFlowMaxBytes() const;
	/// Get the list of available format for the run
	/**
	 * \return the const reference to the format vector.
	 */
	const std::vector<std::string>& getOutputFormats() const;
	/**
	 * Retrieve the remote server hostname as a pointer
	 *
	 * \return a pointer to the string name
	 */
	std::string* getServerName() const;
	/**
	 * Update the server name with the given parameter
	 * 
	 * \param[in] name the new server name.
	 */
	void setServerName(const char* name);
	/**
	 * Retrieve the backend port server log listens to.
	 * \return the port number as an int.
	 */
	int getBackendPort() const;
	/**
	 * Retrieve the frontend port (ws) the server log is listening to.
	 * \return the port number as an int.
	 */
	int getFrontendPort() const;
	/**
	 * Configure the backend port with an argument.
	 * \param[in] p the port number.
	 */
	void setBackendPort(int p);
	/**
	 * Configure the frontend port with an argument.
	 * \param[in] p the port number.
	 */
	void setFrontendPort(int p);
	/**
	 * Test if the real-time support is enabled.
	 * \return true if online mode is enabled, false otherwise.
	 */
	bool needOnlineMode() const;
	/**
	 * Disable the real-time support.
	 * 
	 * This takes place when an issue occurs with remote log server.
	 */
	void disableOnlineMode();
	/**
	 * Enable the real-time support.
	 */
	void enableOnlineMode();
	/**
	 * Get the currently set polling interval for real-time support.
	 * \return the number of second (float).
	 */
	float getOnlineInterval() const;
};

#endif // SYSTEMCONFIGURATION_H
