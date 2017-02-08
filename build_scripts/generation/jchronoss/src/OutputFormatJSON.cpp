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

#include "Configuration.h"
#include "OutputFormatJSON.h"
#include <vector>

OutputFormatJSON::OutputFormatJSON(Configuration *config) : OutputFormat(config)
{}

OutputFormatJSON::~OutputFormatJSON()
{}

std::string OutputFormatJSON::getName()
{
	return "JSON";
}

std::string OutputFormatJSON::getExt()
{
	return ".json";
}

bool OutputFormatJSON::isEmpty() 
{
	return writer.empty();
}

void OutputFormatJSON::appendConfig()
{
	const short MAX = 256;
	char buf[MAX];
	if(getcwd(buf, MAX) != buf)
	{
		printError("Failed with getcwd()", JE_UNKNOWN);
	}
	writer["config"]["path"] = buf;

	/*********** CONFIGURATION MAIN DATA ********/
	writer["config"]["status"] = config->isMaster();
	writer["config"]["exePath"] = config->getExeName();
	writer["config"]["finalState"] = config->getReturnStatus();
	if(config->getConfigFile() != NULL) 
		writer["config"]["file"] = *config->getConfigFile();

	/******** JOB CONFIGURATION MAIN DATA ********/
	writer["configJob"]["verbosity"] = config->job().getVerbosity();
	writer["configJob"]["logging"] = config->job().getLogLevel();
	writer["configJob"]["fake"] = config->job().isFake();
	writer["configJob"]["longNames"] = config->job().wantLongNames();
	writer["configJob"]["maxJobTime"] = config->job().getMaxJobTime();
	writer["configJob"]["maxJobs"] = config->job().getNbMaxJobs();

	if(config->job().getJobsFiles().size() > 0){
		for(std::vector<FileManager*>::const_iterator it = config->job().getJobsFiles().begin(); it != config->job().getJobsFiles().end(); it++)
			writer["configJob"]["jobsList"].append((*it)->toString());
	}

	if(config->job().getFiltersWhiteFiles().size() > 0){
		for(std::vector<FileManager*>::const_iterator it = config->job().getFiltersWhiteFiles().begin(); it != config->job().getFiltersWhiteFiles().end(); it++)
			writer["configJob"]["whitelist"].append((*it)->toString());
	}
	if(config->job().getFiltersBlackFiles().size() > 0){
		for(std::vector<FileManager*>::const_iterator it = config->job().getFiltersBlackFiles().begin(); it != config->job().getFiltersBlackFiles().end(); it++)
			writer["configJob"]["blacklist"].append((*it)->toString());
	}

	/*********** SYTEM CONFIGURATION MAIN DATA ********/
	writer["configSystem"]["maxResources"] = (Json::UInt64)config->system().getNbMaxResources();
	writer["configSystem"]["nbmaxslaves"] = (Json::UInt64)config->system().getNbMaxSlaves();
	writer["configSystem"]["policy"] = config->system().getPolicy();
	writer["configSystem"]["maxbytes"] = (Json::UInt64)config->system().getFlowMaxBytes();
	writer["configSystem"]["maxslavetime"] = config->system().getMaxSlaveTime();
	writer["configSystem"]["minslavetime"] = config->system().getMinSlaveTime();
	writer["configSystem"]["autokill"] = config->system().getAutokill();
	writer["configSystem"]["output"] = *config->system().getOutputDirectory();
	writer["configSystem"]["build"] = *config->system().getBuildDirectory();
	if(config->system().getJobsLauncherCommand() != NULL)
		writer["configSystem"]["launcher"] = *config->system().getJobsLauncherCommand();
	if(config->system().getCompilationJobsLauncherCommand() != NULL)
		writer["configSystem"]["compilLauncher"] = *config->system().getCompilationJobsLauncherCommand();
	if(config->system().getServerLauncherCommand() != NULL)
		writer["configSystem"]["serverLauncher"] = *config->system().getServerLauncherCommand();
	if(config->system().getServerName() != NULL)
	{
		writer["configSystem"]["servname"] = config->system().getServerName();
		writer["configSystem"]["backend"] = config->system().getBackendPort();
		writer["configSystem"]["frontend"] = config->system().getFrontendPort();
	}
	writer["configSystem"]["online"] = config->system().needOnlineMode();
	writer["configSystem"]["refresh"] = config->system().getOnlineInterval();

	for(std::vector<std::string>::const_iterator it = config->system().getOutputFormats().begin(); it != config->system().getOutputFormats().end(); it++)
	{
		writer["configSystem"]["outputFormats"].append(*it);
	}

}

void OutputFormatJSON::appendError(std::string group, std::string name, std::string command, std::string data, double time)
{
	this->group = group;
	Json::Value test;
	test["name"]    = name;
	test["status"]  = "error";
	test["time"]    = time;
	test["command"] = command;
	if(config->job().getLogLevel() == LOG_ALL || config->job().getLogLevel() == LOG_ONLY_FAILED)
		test["log"] = data;

	writer["testsuite"].append(test);
}

void OutputFormatJSON::appendFailure(std::string group, std::string name, std::string command, std::string data, double time)
{
	this->group = group;
	Json::Value test;
	test["name"]    = name;
	test["status"]  = "failure";
	test["time"]    = time;
	test["command"] = command;
	if(config->job().getLogLevel() == LOG_ALL || config->job().getLogLevel() == LOG_ONLY_FAILED)
		test["log"] = data;

	writer["testsuite"].append(test);
}
void OutputFormatJSON::appendSkipped(std::string group, std::string name, std::string command, std::string data, double time)
{
	this->group = group;
	Json::Value test;
	test["name"]    = name;
	test["status"]  = "skipped";
	test["time"]    = time;
	test["command"] = command;
	if(config->job().getLogLevel() == LOG_ALL || config->job().getLogLevel() == LOG_ONLY_FAILED)
		test["log"] = data;

	writer["testsuite"].append(test);
}

void OutputFormatJSON::appendSuccess(std::string group, std::string name, std::string command, std::string data, double time)
{
	this->group = group;
	Json::Value test;
	test["name"]    = name;
	test["status"]  = "success";
	test["time"]    = time;
	test["command"] = command;
	if(config->job().getLogLevel() == LOG_ALL || config->job().getLogLevel() == LOG_ONLY_SUCCESS)
		test["log"] = data;

	writer["testsuite"].append(test);
}

void OutputFormatJSON::appendHeader(size_t err, size_t fail, size_t skip, size_t succ, double time)
{
	writer["package"] = this->group;
	writer["failures"] = (Json::UInt64)fail;
	writer["skipped"] = (Json::UInt64)skip;
	writer["success"] = (Json::UInt64)succ;
	writer["errors"] = (Json::UInt64)err;
	writer["total_time"] = time;
}

void OutputFormatJSON::appendFooter()
{
}

std::string OutputFormatJSON::stringify()
{
	Json::StyledWriter tmp;
	return tmp.write(writer);
}

void OutputFormatJSON::clear()
{
	writer.clear();
}

void OutputFormatJSON::appendPendingJob(Job* job, const char * tagname)
{
	Json::Value test;
	test["nbResources"] = (Json::UInt64)job->getNbResources() ;
	test["nbDeps"] = (Json::UInt64)job->getNbDeps() ;
	test["nbTries"] = (Json::UInt64)job->getNbTries() ;
	test["id"] = (Json::UInt64)job->getId() ;
	
	for(std::vector<std::string*>::const_iterator it = job->getDepsNames().begin(); it != job->getDepsNames().end(); it++)
	{
		test["deps"].append(**it);
	}
	
	for(std::vector<JobConstraint*>::const_iterator it = job->getConstraints().begin(); it != job->getConstraints().end(); it++)
		test["constraints"].append((*it)->getName());
	
	test["command"] = job->getCommand() ;
	test["file"] = job->getReferentFilename() ;
	test["returncode"] = (Json::Int)job->getExpectedReturn() ;
	test["time"] = job->getExpectedTime() ;
	test["name"] = job->getName();

	if(strcmp(tagname, "jobsExecutedList") == 0)
	{
		test["result"]["finalRC"] = (job)->getResult().getFinalRC() ;
		test["result"]["id"] = (Json::UInt64)(job)->getResult().getId() ;
		test["result"]["startTime"] = (job)->getResult().getStartTime() ;
		test["result"]["execTime"] = (job)->getResult().getTime() ;
		test["result"]["flow"] = (job)->getResult().getData() ;
	}
	
	writer[tagname].append(test);
}

void OutputFormatJSON::appendSummary(Summary sumRun, double time)
{
	writer["summary"]["nbSuccess"] = (Json::UInt64)sumRun.nbSuccess;
	writer["summary"]["nbErrors"] = (Json::UInt64)sumRun.nbErrors;
	writer["summary"]["nbFailed"] = (Json::UInt64)sumRun.nbFailed;
	writer["summary"]["nbSkipped"] = (Json::UInt64)sumRun.nbSkipped;
	writer["summary"]["nbTotalSlaves"] = (Json::UInt64)sumRun.nbTotalSlaves;
	writer["summary"]["elapsed"] = (float)time;
}

void OutputFormatJSON::appendMisc(size_t njobs, size_t nlists, size_t nremain)
{
	writer["nbJobs"] = (Json::UInt64)njobs;
	writer["nbLists"] = (Json::UInt64)nlists;
	writer["nbRemainingJobs"] = (Json::UInt64)nremain;
}


