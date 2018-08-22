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


#ifndef SLAVERUNNER_H
#define SLAVERUNNER_H

#include "Runner.h"
#include "DataFlow.h"
#include <time.h>
#ifdef ENABLE_PLUGIN_SERVER
/**
 * Structure forwarded to the polling thread.
 *
 * This struct helps the dedicated thread to interact job contents.
 */
typedef struct polling_data_s
{
	Configuration* config;               ///< the current config
	JobManager* manager;                 ///< the current manager
	std::list<Job*>::const_iterator cur; ///< iterator pointing the last job sent to the server
	int socket;                          ///< the backend connection
	pthread_t tid;                       ///< Thread id to stop it when the worker ends
} polling_data_t;
#endif

/// main class for slave execution mode
/**
 * This class contains all required methods and rountines needed to slave
 * specific behaviour. It's derived from Runner class and implement the whole
 * virtuals functions from this base class.
 */
class RunnerSlave : public Runner {
private:
	/************** MEMBERS **************/
	pthread_mutex_t waitOnEmptySocket; ///< lock on nextWorker status
	pthread_mutex_t waitOnBusySocket;  ///< lock on nextWorker availability
	Worker* nextWorker;                ///< current worker to deal with
	FileManager* outputFile;           ///< global output file (where data will be pushed)
	FileManager* traceFile;            ///< global trace file (log for JSLoc)
	int serverSock;                    ///< remote server socket (stored in polling_data_t too)
	/// represent the worker object given to pthread as argument
	typedef struct sThreadItem {
		RunnerSlave* slave;   ///< the current Runner (to have lockers)
		Worker* worker;       ///< the current worker to launch
	} ThreadItem;      

	/**** NON-CONST ****/
	/// function called by thread to launch a worker (thread function)
	/**
	 * When a worker is launched, a thread handles the worker launch and forks
	 * to track it during execution. Actually, this function wrappes launchWorkerThread
	 * for each cjob and submitting results to the runner
	 * \param[in] arg the ThreadItem pointer to pass through thread creation
	 * \return Thread exit status
	 */
	static void * launchWorkerThread(void*arg);
	/// launch a job from the worker
	/**
	 * Forks in order to start the worker on the machine
	 * \param[in] job the job to launch
	 */
	void launchWorkerThreadStart(Job* job);
	/// behaviour used to sigaction call
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
	explicit RunnerSlave();
	/// Create a RunnerSlave
	RunnerSlave(JobManager* jobMan, Configuration* config);
	virtual void fillWorker ( Worker* cur , size_t nbResources);
	virtual Worker* waitNextWorker();
	virtual void launchWorker ( Worker* cur );
	virtual void preActions();
	virtual void postActions(Worker* cur);
#ifdef ENABLE_PLUGIN_SERVER
	/**
	 * function called at startup to run the polling thread.
	 * In this function, the connection is established w/ the 
	 * remote server and the polling thread is started.
	 */
	void runClient();
	/**
	 * main function for the polling thread.
	 * This function aims to replace the event loop :
	 * 	while(1) { callback(); sleep();}
	 * \param[in] arg the thread data, can be cast into polling_data_t
	 * \return NULL
	 */
	static void * polling_handler(void* arg);
	/**
	 * Polling function.
	 * Here, we drain not-already-sent jobs to the remote log server.
	 * This function has to ensure thread-safety, as some other threads
	 * can update the JobManager too.
	 * \param[in] data thread data
	 */
	static void timer_callback(polling_data_t* data);
#endif
	virtual ~RunnerSlave();
};

#endif // SLAVERUNNER_H
