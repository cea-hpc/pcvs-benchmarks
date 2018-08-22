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

#ifndef OUTPUTFORMAT_H
#define OUTPUTFORMAT_H

#include "Job.h"
#include "FileManager.h"

/**
 * Base class representing the interface between jobs and the way we will produce results.
 * This class has to be derived by each format JCHRONOSS provides.
 */
class OutputFormat
{
protected:
	std::string group;     /**< the "package" for the current output to produce */
	Configuration *config; /**< the config */

public:
	/** defaut constructor.
	 * \param[in] config the config
	 */
	explicit OutputFormat(Configuration *config);
	/**
	 * default destructor
	 */
	virtual ~OutputFormat();

	/**
	 * abstract function: return the name of the implemented format.
	 * \return a string mapping the format name.
	 */
	virtual std::string getName() = 0;
	/**
	 * abstract function : return the extension file suffix for the implemented format
	 * \return the extension as a string
	 */
	virtual std::string getExt() = 0;
	/**
	 * abstract function : build an 'error' result from given parameters
	 * \param[in] group the package name
	 * \param[in] name job name
	 * \param[in] command job command
	 * \param[in] data the output produced by the job execution
	 * \param[in] time the elapsed time
	 */
	virtual void appendError(std::string group, std::string name, std::string command, std::string data, double time) = 0;
	/**
	 * abstract function : build an 'disabled' result from given parameters
	 * \param[in] group the package name
	 * \param[in] name job name
	 * \param[in] command job command
	 * \param[in] data the output produced by the job execution
	 * \param[in] time the elapsed time
	 */
	virtual void appendDisabled(std::string group, std::string name, std::string command, std::string data, double time) = 0;
	/**
	 * abstract function : build an 'failure' result from given parameters
	 * \param[in] group the package name
	 * \param[in] name job name
	 * \param[in] command job command
	 * \param[in] data the output produced by the job execution
	 * \param[in] time the elapsed time
	 */
	virtual void appendFailure(std::string group, std::string name, std::string command, std::string data, double time) = 0;
	/**
	 * abstract function : build an 'skipped' result from given parameters
	 * \param[in] group the package name
	 * \param[in] name job name
	 * \param[in] command job command
	 * \param[in] data the output produced by the job execution
	 * \param[in] time the elapsed time
	 */
	virtual void appendSkipped(std::string group, std::string name, std::string command, std::string data, double time) = 0;
	/**
	 * abstract function : build an 'success' result from given parameters
	 * \param[in] group the package name
	 * \param[in] name job name
	 * \param[in] command job command
	 * \param[in] data the output produced by the job execution
	 * \param[in] time the elapsed time
	 */
	virtual void appendSuccess(std::string group, std::string name, std::string command, std::string data, double time) = 0;
	/**
	 * abstract function : build a specific header from validation statistics.
	 * \param[in] err number of errors
	 * \param[in] fail number of failures
	 * \param[in] skip number of skipped
	 * \param[in] succ number of success
	 * \param[in] time overall elapsed time
	 */
	virtual void appendHeader(size_t err, size_t fail, size_t skip, size_t succ, double time) = 0;
	/**
	 * append a potential footer for the implemented format.
	 */
	virtual void appendFooter() = 0;
	/**
	 * test if the current format does not contain any data.
	 * This allow to avoid publishing empty files
	 * \return true if something can be written to the disk, false otherwise
	 */
	virtual bool isEmpty() = 0;
	/**
	 * Convert everything contained in the object into a writable string.
	 * \return a string, ready to be written into a file.
	 */
	virtual std::string stringify() = 0;
	/**
	 * clear the format buffer.
	 * after that isEmpty() should return true.
	 */
	virtual void clear() = 0;
};


#endif /* OUTPUTMODULE_H */
