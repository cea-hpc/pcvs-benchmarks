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

#ifndef JOB_H
#define JOB_H

#include "utils.h"
#include "JobResult.h"
#include "JobConstraint.h"
#include "FileManager.h"

/// enum on different jobs status
typedef enum eJobStatus {
	PASSED = 0,       ///< job is passed (return codes matching)
	FAILED = 1,       ///< job is failed (return codes doesn't match)
	NOT_RUN = 2,      ///< job haven't be scheduled yet
	NOT_RUNNABLE = 3, ///< job isn't runnable (= a job dep is failed)
	INVALID_DEPS = 4, ///< job isn't schedulable (a dep haven't been found
	MUCH_TRIES = 5,   ///< job have been scheduled too much times without be executed
	DISABLED = 6      ///< job have been disabled (filter does not accept it)
} JobStatus;

/// a job class represents the most little entity from a test suite
/**
 * a job represents a command to launch (which is equivalent to a test) and some information
 * about it: name, status, dependances, etc...
 * Jobs are main purpose of scheduling in the application. Each job represents one
 * functionality to evaluate and can be whatever command which is launched with a 'sh'
 * command call. All this data are parsed from XML file given by command line
 */
class Job {
private:
	/************** MEMBERS **************/
	std::string shortName;                     ///< package attached to the job (java-like)
	std::string command;                       ///< command to execute
	std::string fullName;                      ///< job name
	size_t nbDeps;                             ///< number of dependences
	size_t nbTries;                            ///< number failed launches (not failed job)
	size_t nbResources;                        ///< number of resources required by the job
	int expectedReturnCode;                    ///< return code which means job is succeeded
	double timer;                              ///< expected time for test (DEFAULT_MAX_TEST_TIME by default)
	double delta;                              ///< Used for performance measurement.
	std::vector<std::string*>vDepsNamesTab;    ///< deps names list (as string, used to resolve dependances later)
	std::vector<Job*> vDeps;                   ///< job deps list
	std::vector<JobConstraint*> vConstraints;  ///< job constraints list
	JobStatus status;                          ///< current job status
	JobResult* result;                         ///< a pointer on result, set after launch
	std::string referentFilename;              ///< the file where job have been extracted as a string name
	size_t myId;                               ///< job id (used by slave for print the job id in the current worker)
	
	/************** STATICS **************/
	static size_t id;                          ///< static incremental id at each new created job
	
public:
	/************* STATICS *************/
	/****** CONST ******/
	static const size_t DEFAULT_MAX_TRIES = 60;     ///< if no value set, default max tries allowed before job considered as failed
	static const size_t DEFAULT_MAX_JOB_TIME = 300; ///< if no value set, default max time allowed to the job
	
	/************* FUNCTIONS *************/
	/**** NON-CONST ****/
	///standard constructor
	explicit Job();
	///main job constructor, filling the most part of class member at initialization
	/**
	 * the job id is defined by static variable id incrementation.
	 *
	 * \param[in] name job name
	 * \param[in] group job family
	 * \param[in] command the command attached to the job.
	 * \param[in] nbRes number or resources required by the job
	 * \param[in] rc the expected return code for the job.
	 * \param[in] time the estimated time for the job (DEFAULT_MAX_JOB_TIME if not specified)
	 * \param[in] deps the deps names tab to resolve deps later
	 * \param[in] constraints the list of constraints
	 * \param[in] file the referent file where job have been extracted
	 */
	Job ( std::string name, std::string group, std::string command, std::vector< std::string* > deps, std::vector< JobConstraint* > constraints, std::string file, size_t nbRes = 1, int rc = 0, double time = 0.0, double delta = 0.0);
	/// add a constraint to the current job
	/**
	 * add a constraint on the job.static size_t id; 
	 * \param[in] constraint the constraint to add
	 * \return <b>True</b> if the adding successes
	 * \return <b>False</b> otherwise
	 */
	void addConstraint(JobConstraint* constraint);
	///add a dependency to the job.
	/**
	 * After parsing XML file and create all jobs into the job Manager, we iterate
	 * on each job to resolve dependences, allowing to quickly retrieve job deps when
	 * required.
	 * \param[in] dep the dependence to add
	 * \return <b>True</b> if adding successes
	 * \return <b>False</b> otherwise
	 */
	void addDependency(Job* dep);
	//// update job status after running
	/**
	 * when results comes back from slave, job status is updated according to
	 * return code
	 * \param[in] status the new status which replaces the old one
	 */
	void updateStatus(JobStatus status);
	/// add a JobResult to the current job after running
	/**
	 * when results comes back from slave, these result is pushed into a jobResult
	 * and inserted into the job.
	 * \param[in] rc effective return code
	 * \param[in] time effective execution time
	 * \param[in] begin starting timestamp (used for trace generation)
	 * \param[in] data job output (eventually truncated)
	 */
	void addResult(int rc, double time, double begin, std::string data);
	/// set a new result for the current job
	/**
	 * As the slave create the jobResult and write it down directly into binary file,
	 * when master read this file, no need to recreate a jobresult but just add it
	 * to the corresponding job.
	 * \param[in] res the result read from file and to insert into the job.
	 */
	void setResult(JobResult* res);
	/// set id to a specific value
	/**
	 * \param[in] value the value used to set
	 */
	void setId(size_t value);
	/**
	 * add a try to current job (MAX = DEFAULT_MAX_TRIES)
	 * \return <b>true</b> if job can be relaunched (< MAX)
	 * \return <b>false</b> otherwise
	 */
	bool addATry();
	/// virtual destructor to unset Job
	virtual ~Job();
	
	/****** CONST ******/
	/// job displaying function to screen (used to debug)
	void display() const;
	/// Return true if current job satisfies given constraint
	/**
	 * \param[in] tag The constraint name used to check if job handles this constraint
	 * \return <b>True</b> if constraint belongs to the job
	 * \return <b>False</b> otherwise
	 */
	bool isValid(std::string tag) const;
	/// get number of required resources by the job
	/**
	 * \return the number of required resources
	 */
	size_t getNbResources() const;
	/// get number of deps for current test
	/**
	 * \return deps number
	 * \warning prefet to use deps vector instead
	 * 
	 */
	int getNbDeps() const;
	/// get number of times jobs have attempted to be launched
	/**
	 * \return number of tentatives
	 */
	size_t getNbTries() const;
	/// get job id (job number in xml list)
	/**
	 * This function is used by the slave to print worker state when job have been
	 * executed. It represents the n-th job in the worker
	 * \return job number
	 */
	size_t getId() const;
	/// get job name
	/**
	 * \return a string containing the job name
	 * \warning the name is not directly writable in xml file (need to escape)
	 * \see HTMLEncoding
	 */
	std::string getName() const;
	/// get deps list as jobs pointer
	/**
	 * get deps list for the current job. This function is essentialy used by unit-test
	 * to validate job class behaviour
	 * \return a const reference on deps list
	 */
	const std::vector<Job*>& getDeps() const;
	/// get deps list as string
	/**
	 * Before deps resolution, we store jobs dependences as string list.
	 * \return a const reference on deps list
	 */
	const std::vector<std::string*>& getDepsNames() const;
	/// get job status
	/**
	 * contains NOT_RUN until job have been executed (PASSED/FAILED/NOT_RUNNABLE)
	 * \see eJobStatus
	 * \return The job status associated to the job
	 */
	JobStatus getStatus() const;
	/// get job result after running
	/**
	 * this function returns NULL until job have been executed
	 * \return <b>NULL</b> if job haven't been executed yet
	 * \return JobResult pointer otherwise
	 */
	const JobResult& getResult() const;
	/// get constraints list
	/**
	 * to iterate on each job constraints, this function returns a const reference
	 * on a constraint list const reference
	 * \return a const reference on constraints list
	 */
	const std::vector<JobConstraint*>& getConstraints() const;
	/// get expected return code for the current job
	/**
	 * When job is launched, the effective return code is waited. Then, it's compared
	 * to the expected return code. If don't match, job is considered as failed
	 * \return expected return code
	 */
	int getExpectedReturn() const;
	/// get job command
	/**
	 * \return a string containing command to launch the job
	 * \warning the command isn't escaped to be directly written in xml file
	 * \see HTMLEncoding()
	 */
	std::string getCommand() const;
	/// get file where job have been extracted
	/**
	 * \return the file name
	 */
	std::string getReferentFilename() const;
	/// get job shor name
	/**
	 * Job are grouped by package (i.e. family) and this function returns 
	 * base name for the current job.
	 * \return a string containing the job base name
	 */
	std::string getShortName() const;
	/// check if some job deps are invalid
	/**
	 * When jobs comes back from running, we check if there are passed or
	 * failed. If job failed, others jobs with him as dependances are invalided
	 * \param[in] before check is done before run or not (consider NOT_RUN jobs or not)
	 * \return <b>True</b> if job mustn't be scheduled
	 * \return <b>False</b> otherwise
	 */
	bool isDepInvalid(bool before = true) const;
	/// get expected time for current test
	/**
	 * \return the expected timer
	 */
	double getExpectedTime() const;
	/// set expected time if no one is set in xml file
	/**
	 * \param[in] time the timer used to set
	 */
	void setExpectedTime(double time);
	/// get the range of seconds around "time" the test is considered as success.
	/**
	 * \returns number of seconds as a double
	 */
	double getDelta() const;
	/// evaluate if the current test is passed
	/**
	 * \param[in] rc the effective return code
	 * \param[in] time the elasped time to result
	 * \returns <b>True</b> if the test passed
	 * \returns <b>False</b> otherwise.
	 */
	bool isPassed(int rc, double time) const;

};

#endif // JOB_H
