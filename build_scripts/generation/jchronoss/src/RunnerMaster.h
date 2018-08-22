/****************************************************************************/
/*                                                                          */
/*                         Copyright or (C) or Copr.                        */
/*       Commissariat a l'Energie Atomique et aux Energies Alternatives     */
/*                                                                          */
/* Version : 1.2                                                            */
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

#ifndef RUNNERMASTER_H
#define RUNNERMASTER_H

#include "Runner.h"
#include "HashTable.h"

/// main class for master execution mode
/**
 * This class contains all required methods and rountines needed to master
 * specific behavior. It's derived from Runner class and implement the whole
 * virtual functions from this base class. 
 */
class RunnerMaster : public Runner {
private:
	/************** MEMBERS **************/
	HashTable tabWorkers;  ///< list of currently running workers
	bool firstRound;       ///< check if a first worker have been launched

	/************** STATICS **************/
	/****** CONST ******/
	static const size_t NB_PARAMETERS_COMMAND = 14;  ///< max number of parameters used to start a new jchronoss instance
	static pid_t serverPid;	                         ///< stored PID of the remote log server
	
	/************* FUNCTIONS *************/
	/**** NON-CONST ****/
	/// behavior used to sigaction call
	/**
	 * This function is called when an signal is caught by application. This 
	 * function is passed to sigaction call
	 * \param[in] sig the signal
	 * \param[in] siginfo info about raising
	 * \param[in] context the current signal handling context
	 */
	static void handlerSignal(int sig, siginfo_t *siginfo, void *context);
public:
	/************* FUNCTIONS *************/
	/**** NON-CONST ****/
	///standard constructor
	explicit RunnerMaster();
	/// create a RunnerMaster and set its attributes
	RunnerMaster(JobManager* jobMan, Configuration* config);
	/**
	 * backup creation routine to generate backup file with job manager
	 */
	void createBackup();
	/**
	 * Start log server. 
	 * The current process is the master
	 */
	void startServer();
	/**
	 * Start the logger.
	 * The current process mimics the worker behavior.
	 * This is mandatory, as invalid jobs are not scheduled by the workers but
	 * the remote server has to know about it. When jobs are invalidated (JobMManager),
	 * the master sends these job "results" to the remote server through the
	 * socket created by this function.
	 */
	void startLogger();
	/// Destroy RunnerMaster (print last end line)
	~RunnerMaster();
	virtual void pullJobsFromFiles();
	virtual void pushJobsIntoFiles();
	virtual void fillWorker ( Worker * cur, size_t nbResources );
	virtual Worker* waitNextWorker();
	virtual void launchWorker ( Worker* cur );
	virtual void preActions();
	virtual void postActions(Worker *cur);
	virtual void startCompilation();
};

#endif // RUNNERMASTER_H
