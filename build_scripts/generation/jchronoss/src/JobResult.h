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

#ifndef JOBRESULT_H
#define JOBRESULT_H

#include "utils.h"
#include "FileManager.h"

///Stores results and informations about running for a given job
/**
 * When a job comes back from running, a JobResult is created and contains all
 * informations about the run. Some token are inserted to ensure data coherency, 
 * like hashes and magik numbers.
 */
class JobResult {
private:
	/************** MEMBERS **************/
	/// contains all informations about the run (except output)
	class JobResultHeader{
	public:
		size_t id;             ///< used to retrieve job after running
		size_t hashedName;     ///< the hashed job name
		size_t hashedData;     ///< the hashed output
		size_t sizeData;       ///< number of bytes in output
		double executionTime;  ///< effective execution time
		int finalReturnCode;   ///< effective return code
		double startingTime;   ///< timestamp when job started
		unsigned int magik;    ///< magik number (data coherency)
	} header;                          ///< header with global information about run
	std::string flow;                  ///< output
	
public:
	/************* FUNCTIONS *************/
	/**** NON-CONST ****/
	///standard constructor
	JobResult();
	///Fill header object with main information about run
	/**
	 * All this information are given by application and allow to generate
	 * a job result which could be directly written (binary) into file
	 * \param[in] jobName the job name
	 * \param[in] jobId the job id
	 * \param[in] rc the effective return code
	 * \param[in] time the execution time
	 * \param[in] begin the starting timestamp
	 */
	void fillHeader(std::string jobName, size_t jobId, int rc, double time, double begin);
	/// insert data flow info the jobresult
	/**
	 * Once header have been set, the job result is set with job output
	 * \param[in] data output to insert
	 */
	void insertData(std::string data);
	/// write the jobResult in the given file
	/**
	 * The job result will be written in two steps:
	 * <ol>
	 *  <li> write the header</li>
	 *  <li> write the flow</li>
	 * </ol>
	 * All writes are in binary mode
	 * \param[in] file the output file where results will be pushed
	 */
	void write(FileManager* file);
	/// read the jobResult from the given file
	/**
	 * Fill the current jobResult with data from given binary file
	 * The reading is done in two steps:
	 * <ol>
	 *  <li> read the header</li>
	 *  <li> read the flow</li>
	 * </ol>
	 * \param[in] file the input file
	 * \return <b>True</b> if read is succeeded
	 * \return <b>False</b> otherwise
	 */
	bool read(FileManager* file);
	///virtual destructor to unset a jobResult
	virtual ~JobResult();
	
	/****** CONST ******/
	/// get hashed name from header
	/**
	 * \return the hashed name
	 */
	size_t getHashName() const;
	/// get effective return code
	/**
	 * \return return code stored in header
	 */
	int getFinalRC() const;
	/// get magik number
	/**
	 * \return a value that should be DEFAULT_MAGIK_NUMBER
	 */
	unsigned int getMagik() const;
	/// print a jobresult on screen
	void display() const;
	/// check if jobresult read from file are valid
	/**
	 * Two fields are checked:
	 * <ol>
	 *  <li> Controls that hashed flow from file matches with value stored in header</li>
	 *  <li> check if magik number is equal to DEFAULT_MAGIK_NUMBER</li>
	 * </ol>
	 */	 
	void checksum() const;
	/// get job time
	/**
	 * \return the time (rounded to 2 numbers)
	 */
	double getTime() const;
	///get job start timestamp
	/**
	 * \return the starting time (rounded to 2 numbers)
	 */
	double getStartTime() const;
	/// get job result data
	/**
	 * \return a string containing job output
	 */
	std::string getData() const;
	/// get hashed data
	/**
	 * \return a int representing the hash
	 */
	size_t getHashData() const;
	/// get data number of chars
	/**
	 * \return the number of chars
	 */
	size_t getSizeData() const;
	/// get the job id
	/**
	 * In master, used to match the launched job with its result
	 * \return the job id
	 */
	size_t getId() const;
};

#endif // JOBRESULT_H
