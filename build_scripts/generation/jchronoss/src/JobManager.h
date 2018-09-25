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

#ifndef JOBMANAGER_H
#define JOBMANAGER_H

#include "json.h"
#include "utils.h"
#include "Job.h"
#include "Configuration.h"
#include "FilterBox.h"
#include "XMLJobParser.h"
#include "OutputFormat.h"
#include <functional>

/// Main class gathering all jobs for the current validation
/**
 * A Job manager is the main entity handling jobs (reading, writing results, etc...)
 * jobs are gathered according to their required resources number. There are one lists
 * for each type of job size. Thus, if the biggest runnable job requires 10 resources,
 * We will have 10 lists, for each requiremeents possibility.
 */
class JobManager {
private:
	/************** MEMBERS **************/
	std::list<Job*>* jobsList;          ///< The job lists array (a list per requiremeent possibility
	size_t nbLists;                     ///< the number of jobs list
	std::list<Job*> executedJobsList;   ///< the executed jobs list (after execution, before pushing)
	size_t nbRemainingJobs;             ///< number of jobs which still are in job Manager (at any t moment)
	size_t nbJobs;                      ///< total number of jobs parsed from files
	std::vector<OutputFormat*> outputs; ///< Output standard format required by the user
	Configuration* config;              ///< current global configuration
	std::list<Job*>::iterator jobCpt;   ///< log server utility (used by the slave) */
	std::list<HashedJob> hashTable[MAX_JOB_HT];

	/************* FUNCTIONS *************/
	/**** NON-CONST ****/
	void resolveAJobDeps( Job* current );    ///< used by jobManager to match job deps (as string) to real jobs (as pointer)
	Job* resolveADep( std::string pattern ); ///< Looking for the matching deps stored into the manager
	void pushInOneFile(FileManager* file);   ///< used by JobManager to write down results info output xml files

public:
	/**************** VARS ***************/
	Summary sumRun;                   ///< filled at the end : summary and stats about the run

	/************* FUNCTIONS *************/
	/**** NON-CONST ****/
	///standard constructor
	explicit JobManager();
	///Create a JobManager
	/**
	 * \param[in] config current global configuration
	 */
	JobManager( Configuration* config );
	/// add current job to the manager
	/**
	 * When job is added, the manager looks for the good list where job will be pushed.
	 * \param[in] current current job to add to the list
	 */
	void addJob(Job* current);
	/// add current job to the executed jobs lists
	/**
	 * After jobs executed, they are pushed into an another and waiting for writing down
	 * \param[in] current the job to push into executed list
	 */
	void addExecutedJob(Job* current);
	/// main function to resolve jobs dependences
	/**
	 * Once all jobs are parsed from files, jobs deps are resolved and deps names list are
	 * replaced by a jobs pointers list, in order to access more quickly to the job's deps
	 */
	void resolveJobsDeps();
	///update number or remaining jobs ( adding val to class member)
	/**
	 * When jobs are added or removed from manager, an increment or decrement is realized
	 * on remaining jobs number
	 * \param[in] val number of jobs to add to remaining jobs
	 */
	void update(int val);
	///clean invalid jobs in manager
	/**
	 * When jobs are invalid because some of theirs deps are failed (or whatever else which
	 * block job launching), this function clean up manager to remove all of these not runnable
	 * jobs.
	 */
	void cleanInvalidJobs();
	/// parse jobs from xml files
	/**
	 * read from xml files each job and add them to the manager. XML Files are validated with
	 * a XML scheem, in order to avoid bad tests insertion
	 * \see XMLParser
	 * \warning if enabled, this function uses OpenMP directives to improves XML parsing when
	 * there are a lot of files.
	 */
	void pullJobsFromFiles();
	/// when scheduler have ended, pushes all jobs results into files
	void pushJobsIntoFiles();
	/**
	 * Create a backup file in build path with all needed information to restart
	 * \param[in] extrasJobs pointer on jobs not currently present in job manager
	 */
	void createBackup(std::list<Job*> * extrasJobs);
	/**
	 * Load a backup file from configuration and squash current with backed up one
	 */
	void loadBackup();
	///virtual destructor to unset the job Manager
	virtual ~JobManager();

	/****** CONST ******/
	///print a job manager on screen (debug)
	void display() const;
	/// check if job Manager is job-free (all jobs lists are empty)
	/**
	 * \return <b>True</b> if Job Manager is empty
	 * \return <b>False</b> otherwise
	 */
	bool isEmpty() const;
	/// check if list at given indic is empty
	/**
	 * check if one list is empty. May be not used in our scheduler because we use vector
	 * iteration and the iterator stop is the vector is empty.
	 * Essentially used to debug
	 * \param[in] indice the list indice to check
	 * \return <b>True</b> if current list is empty
	 * \return <b>False</b> otherwise
	 */
	bool currentListIsEmpty(int indice) const;
	/// get the list with the best indice where there still are jobs
	/**
	 * This function returns the greatest jobs lists indice lower than current
	 * number of available resources
	 * \param[in] nbResources current number of availables resources
	 * \return the indice where there still are jobs
	 */
	int getHigherValidList(size_t nbResources) const;
	/// get the number of remaining jobs in the job Manager
	/**
	 * \return the number of remaining jobs
	 */
	size_t getNbRemainingJobs() const;
	/// get the total number of jobs stored into the job manager
	/**
	 * \return the total number of jobs
	 */
	size_t getNbJobs() const;
	/// get the jobs lists array
	/**
	 * Allow to get the jobs list array
	 * \warning this function allows you to update the jobsLists and eventually squash the whole job manager structure. Be careful with it. Nevertheless, the attribute pointer cannot be changed (no raalloation, etc...)
	 * \return the jobs lists array
	 */
	std::list<Job*>* jobsLists() const;
	/// get number of lists
	size_t getNbLists() const;
	/// when jchronoss finished, display some stats about runs
	void displaySummary() const;
	///Update final return code in config depending on validation state
	void updateFinalState() const;
	/// get the first element of executed list, as a const_iterator
	/**
	 * \return a const_iterator to list.begin()
	 */
	std::list<Job*>::const_iterator getExecutedStart() const;
	/// get the last element of executed list, as a const_iterator
	/**
	 * \return a const_iterator to list.end()
	 */
	std::list<Job*>::const_iterator getExecutedEnd() const;
	/**
	 * Tag a job as not executable yet and delay it in Policy scheduling.
	 * \param[in] id the list id where job has been found
	 * \param[in] job the job to delay
	 * \return the iterator to the next element
	 */
	std::list<Job*>::iterator delayJob(int id, std::list<Job*>::iterator job);
	/**
	 * Pick a job in JobManager list and return the next element.
	 * \param[in] id the list id where job has been found
	 * \param[in] job the job to remove
	 * \return the iterator to the next element
	 */
	std::list<Job*>::iterator pickJob(int id, std::list<Job*>::iterator job);
};

#endif // JOBMANAGER_H

