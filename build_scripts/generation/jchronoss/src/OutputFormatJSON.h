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

#ifndef OUTPUTFORMATJSON_H
#define OUTPUTFORMATJSON_H

#include "OutputFormat.h"
#include "json.h"
#include "Job.h"

/**
 * OutputForat implementation for JSON format
 */
class OutputFormatJSON : public OutputFormat
{
	private:
		Json::Value writer; ///< container handling JSON-formatted data.
	public:
	/**
	 * default constructor
	 * \param[in] config the config
	 */
	explicit OutputFormatJSON(Configuration *config);

	virtual std::string stringify();
	virtual void clear();
	virtual std::string getName();
	virtual std::string getExt();
	virtual void appendError(std::string group, std::string name, std::string command, std::string data, double time);
	virtual void appendFailure(std::string group, std::string name, std::string command, std::string data, double time);
	virtual void appendSkipped(std::string group, std::string name, std::string command, std::string data, double time);
	virtual void appendSuccess(std::string group, std::string name, std::string command, std::string data, double time);
	virtual bool isEmpty();
	virtual void appendHeader(size_t err, size_t fail, size_t skip, size_t succ, double time);
	virtual void appendFooter();

	/* BACKUP HANDLERS:
	 * JSON is not only used to format output data. It is also used 
	 * to handle data transfer between ckpt files or remote log server.
	 */
	/**
	 * from the configuration, serialize a JSON object
	 */
	void appendConfig();
	/**
	 * Build a special entry for jobs caming from "tagname".
	 * This function is used to add to backup file, jobs currently scheduled in workers
	 * and removed from global job manager (to avoid double scheduling).
	 * \param[in] job the job to format
	 * \param[in] tagname the JSON component where the job belongs to
	 */
	void appendPendingJob(Job* job, const char * tagname = "jobsList");
	/**
	 * Append JCHRONOSS temp summary inside the backup.
	 * \param[in] sum the summary object
	 * \param[in] time the currently elapsed time for the run
	 */
	void appendSummary(Summary sum, double time);
	/**
	 * Append some other stuffs into the backup file.
	 * \param[in] nj total nuber of jobs
	 * \param[in] nl number of lists (JobManager)
	 * \param[in] nr number of remaining jobs.
	 */
	void appendMisc(size_t nj, size_t nl, size_t nr);
	~OutputFormatJSON();
};


#endif /* OUTPUTMODULE_H */
