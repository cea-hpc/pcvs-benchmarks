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


#ifndef RUNNER_H
#define RUNNER_H
#include "Worker.h"
#include "JobManager.h"
#include "PolicyDefault.h"
#include "PolicySchedRes.h"
#include "PolicySchedTime.h"
#include "PolicyCustom.h"

///Represents directives to use to follow scheduling policy
/**
 * A Runner is a class which allow to deal with jobs and resources.
 * It's used as template for derived class RunnerMaster and RunnerSlave
 */
class Runner {
protected:
	/************** MEMBERS **************/
	JobManager* jobMan;     ///< referent job manager
	Configuration* config;  ///< current global configuration
	Policy* policy;         ///< policy to apply to handle jobs

public:
	/************* FUNCTIONS *************/
	/**** NON-CONST ****/
	//standard constructor
	explicit Runner();
	/// constructor which instantiate a Runner
	/**
	 * \param[in] job the referent job manager
	 * \param[in] config gloal configuration
	 */
	Runner(JobManager* job, Configuration* config);
	///< calls parsing files routines for xml files
	/**
	 * This function do some work about uploading jobs from xml files.
	 * Since work is the same in master and slave, the implementation
	 * is provided in the Runner
	 */
	virtual void pullJobsFromFiles();
	///< calls writing routines for jobs and pushing down into xml files
	/**
	 * This function writes executed jobs into their own JUnit xml Files
	 * The slave doesn't call this function (empty routine) because results
	 * are pushed from workers themselves. The master uses this call to
	 * write jobs in JUnit files
	 */
	virtual void pushJobsIntoFiles();
	/// main routine to fill a worker with jobs
	/**
	 * This routine do some pre and post actions about worker filling. 
	 * The main way to do is implemented by referent policy. This function
	 * is called only for fill "tests", i.e. jobs considered as "compilation"
	 * will have been executed before that.
	 * \param[out] cur the current corker to fill
	 * \param[in] nbResources availables resources number
	 */
	virtual void fillWorker ( Worker* cur, size_t nbResources);
	/// wait for next ended worker
	/**
	 * Do a wait call, retrieve the worker in the hash table and pull results
	 * from binary file
	 * \return A worker pointer on referent object
	 * \return <b>NULL</b> if worker not found
	 */
	virtual Worker* waitNextWorker();
	/// launch the worker and register it in the database
	/**
	 * Starts the worker and call this application in slave mode and according
	 * to launching options (batch managers, options...), and then saves it in
	 * hash table to find it out quickly later
	 * \param[in] cur the worker to launch
	 */
	virtual void launchWorker ( Worker* cur );
	/// do some work before workers creation
	/**
	 * This function is called each time a new worker is retrieved
	 */
	virtual void preActions();
	///< do some worker after worker retrieving
	/**
	 * In Master, this function dispatches jobs: if they've been 
	 * executed, there are remove from job Manager. Otherwise, they
	 * are rescheduled in the job manager
	 * \param[in] cur the worker to deal with
	 */
	virtual void postActions(Worker *cur);
	///< Specific function to launch compilation jobs first
	/**
	 * This function is not implemented in slave mode. Only master have
	 * implemented routine.
	 */
	virtual void startCompilation();
	/// virtual destructor to unset a Runner
	virtual ~Runner();
	
};

#endif // RUNNER_H

