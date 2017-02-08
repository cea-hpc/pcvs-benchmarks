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


#ifndef WORKER_H
#define WORKER_H
#include "FileManager.h"
#include "Job.h"

/// represents a group of jobs to launch
/**
 * A worker is created in both master and slave. Its aim is to be filled with
 * jobs and follows them during their execution. We distinguish two differents
 * behaviours:
 * <ul>
 * <li>
 * MASTER: a worker is considered as a group of jobs transmitted to the slave for
 * execution. The worker writes down communication information between instances
 * himself. A pointer is kept on the worker until slave terminated. Then, worker pulls
 * data from file and updates its jobs. At the end, these jobs are pushed in job manager
 * and the worker is deleted
 * </li>
 * <li>
 * SLAVE: A worker is just the entity who follows the job from it's starting selection until
 * its data writing down into binary file. A job which is pushed into a worker won't be
 * put again in the job manager. The worker handles the job's end of life.
 * </li>
 * </ul>
 */
class Worker {
private:
	/************** MEMBERS **************/
	size_t nbRequiredResources;   ///< number of required resources for this launch
	FileManager* inputFile;       ///< where data will be written (and read by slave)
	FileManager* outputFile;      ///< where data will be read (and write by slave)
	FileManager* traceFile;       ///< where log will be pushed (jsloc)
	std::list<Job*> jobsList;     ///< list of jobs to execute
	
public:
	/************** STATICS **************/
	/**** NON-CONST ****/
	static unsigned int numWorker;///< worker id incrementation (used to number temp files)
	
	/************* FUNCTIONS *************/
	/**** NON-CONST ****/
	///standard constructor
	explicit Worker();
	/// set I/O files for the worker
	/**
	 * Inverted behaviour between master and slave.
	 * \param[in] input the input file to push(pull) data
	 * \param[in] output the output file to pull(push) data
	 * \param[in] trace eventually, a trace file where log will be pushed
	 */
	void setIOFiles(FileManager* input, FileManager* output, FileManager* trace = NULL);
	/// destroy FileManagers
	/**
	 * Specific function to avoid leak memory --> unset its own file Manager
	 */
	void unsetIOFiles();
	/// configure number of required resources once worker if filled
	/**
	 * \param[in] val number of resources to reserve
	 */
	void setNbRequiredResources( size_t val );
	/// add current job to the worker
	/**
	 * \param[in] job the job to add
	 */
	void add(Job* job);
	/// push the whole job in worker into temporary files (start with slave)
	/**
	 * Reserved to MASTER
	 * \param[in] nbRemain number of remaining jobs when the worker is launched
	 */
	void pushJobsInputFile(int nbRemain);
	/// push the whole jobs results into binary file (slave end)
	/**
	 * Reserved to SLAVE
	 */
	void pushJobsOutputFile();
	/// read results from output file
	/**
	 * Reserved to MASTER
	 */
	void pullJobsOutputFile();
	/// Get the first job in the worker list
	/**
	 * \return an iterator on the first job item
	 */
	std::list<Job*>::iterator begin();
	/// Get the last job in the worker list
	/**
	 * \return an iterator on the last job item
	 */
	std::list<Job*>::iterator end();
	/// virtual destructor to unset a worker
	virtual ~Worker();
	
	/****** CONST ******/
	/// get number of required resources
	/**
	 * \return the number of resources registerd for the current worker
	 */
	size_t getNbResources() const;
	/// get the input file set for the worker
	/**
	 * \return the configured input file
	 */
	FileManager* getInputFile() const;
	/// get the output file set for the worker
	/**
	 * \return the configured output file
	 */
	FileManager* getOutputFile() const;
	/// display a worker on screen
	void display() const;
	/// get the number of jobs in the worker
	/**
	 * \return the numbe of jobs
	 */
	size_t size() const;
	/**
	 * get jobs list from running workers (used for backup, read-only)
	 * \return a const pointer on jobs list contained in running workers
	 */
	const std::list<Job*> * getList() const;	
};

#endif // WORKER_H
