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

#ifndef DATAFLOW_H
#define DATAFLOW_H

#include "utils.h"

/// This class manipulates jobs output streams
/**
 * When a job is started, its standard output is redirected and entirely caught
 * in a dataflow object. The stream is accessible as a string. 
 * Internaly, in construction, there are two buffer:
 * - the first buffer catches the first BUFFER_FLOX_MAX bytes of the output.
 * - the second one is a circular buffer catching the last BUFFER_FLOW_MAX bytes
 * of the output.
 * In this manner, we can keep up to 2*BUFFER_FLOW_MAX bytes, more specifically 
 * the first and last bytes of the job output. If output is truncated (more than
 * 2*BUFFER_FLOW_MAX bytes produced), an advertising is inserted into the output
 * to prevent the user that we couln't store all information
 */
class DataFlow {
private:
	/************** MEMBERS **************/
	std::string firstBuffer;          ///< first buffer for current output
	std::string secondBuffer;         ///< second buffer, circular one
	size_t posFirstBuffer;            ///< position from start where current first buffer is filled
	size_t posSecondBuffer;           ///< position from start where second buffer is filled
	bool overload;                    ///< boolean on the fact that data have been lost (truncated output)
	
	/************** STATICS **************/
	/**** NON-CONST ****/
	static size_t bufferFlowMax;      ///< MAX number of bytes kept in one buffer
public:
	/************** STATICS **************/
	/****** CONST ******/
	static const size_t DEFAULT_BUFFER_FLOW_MAX = (10*1024); ///< MAX number of bytes kept in one buffer
	static const std::string headerOverheadString;           ///< header string inserted if output need to be truncated
	static const std::string footerOverheadString;           ///< footer string inserted if output need to be truncated

	/************* FUNCTIONS *************/
	/**** NON-CONST ****/
	/// standard DataFlow constructor
	/**
	 * Its aim is to construct the job data flow
	 * \param[in] flowSize defines the max size flow used, if default is not optimal
	 */
	explicit DataFlow(size_t flowSize);
	/// constructor by copy
	DataFlow(const DataFlow& other);
	/// fill the data flow from the given file descriptor
	/**
	 * When job is started, a pipe is created to redirect output from the parent's process
	 * the fd parameter represents the pipe channel where read to get output.
	 * \param[in] fd The file descriptor (from pipe) where data are read
	 * \return The number of bytes effectively read
	 */
	size_t fillFrom ( FILE* fd );
	/// fill the flow with specitic given input
	/**
	 * Sometimes, we need to insert our own message as job output. This function erases
	 * DataFlow contents and copy the chain into the flow.
	 * The reading is interrupt-safe.
	 * \param[in] chain the chain to copy into the dataflow
	 * \return the number of bytes effectively read
	 */
	size_t fillSpecificMessage(std::string chain);
	/// operator "=" redefinition
	DataFlow& operator= ( const DataFlow& other );
	///virtual destructor to unset the DataFlow
	virtual ~DataFlow();
	
	/****** CONST ******/
	/// generate a hash from the flow
	/**
	 * This function is used to check flow integrity over file reading/writing
	 * \return the hash generated
	 */
	size_t createHash() const;
	///check if current flow hash match with given hash
	/**
	 * To ensure data integrity, we check there are no data lost. Thus, we compute a flow hash
	 * when we write and read data. The two hashs have to match.
	 * \param[in] hash The hash used to compare
	 * \return <b>True</b> if hashes matches
	 * \return <b>False</b> otherwise
	 */
	bool matchHash(size_t hash) const;
	///get the whole DataFlow content as a string
	/**
	 * \return the generated string
	 */
	std::string getContent(std::string* = NULL) const;
	///get the number of bytes stored info the flow
	/**
	 * \return The current size of the output
	 */
	size_t size() const;
};

#endif // DATAFLOW_H
