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

#include "ConfigurationJob.h"
using namespace std;

JobConfiguration::JobConfiguration() {
	
	//setting default values
	verbosity = VERBOSE_SILENT;
	fakeExecution = false;
	logLevel = LOG_ONLY_FAILED;
	maxJobTime = Job::DEFAULT_MAX_JOB_TIME;
	nbMaxJobs = DEFAULT_MAX_NB_JOBS;
	longNames = false;
}

void JobConfiguration::printHelp() const {
	//print help
	cout 	<<  COLOR_PASS "\n JOB OPTIONS :" COLOR_NORM "\n"
		<< PADDED_HELP << "   -s, --silent "			<<"Print only job status.\n"
		<< PADDED_HELP << "   -v, --verbose "			<<"Print only failed jobs traces + --silent.\n"
		<< PADDED_HELP << "   -V, --very-verbose "		<<"Print all about jobs and status.\n"
		<< PADDED_HELP << "   -Y, --verbosity=[0|1|2] "		<<"Choose verbosity level (silent, verbose, very verbose)\n"
		<< PADDED_HELP << "       --keep-[none|fail|success|all] "<<"Keeping logs respectively nothing (-N), only failed (-n), success (-a) or all (-A) jobs\n"
		<< PADDED_HELP << "   -K, --keep=[0|1|2|3] "		<<"Keeping logs respectively nothing (0), only failed (1), success (2) or all (3) jobs\n"
		<< PADDED_HELP << "   -t, --maxt-job "			<<"Specify max time job.\n"
		<< PADDED_HELP << "   -w, --white=<file1,file2...> "	<<"List of filters file containing jobs to execute\n"
		<< PADDED_HELP << "   -b, --black=<file1,file2...> "	<<"List of filters file containing jobs to NOT execute.\n"
		<< PADDED_HELP << "       --[no-]fake "			<<"Activate (-f)/desactivate(-F) jobs times simulation (useful for debugging)\n"
		<< PADDED_HELP << "   -l, --long-names "		<<"Display jobs name in long format: package + name (just name by default)\n"
	;
}

void JobConfiguration::printConfiguration() const
{
	//print configuration (with padding and interpretation)
	cout << PADDED_OPT << "| - MAX Job Time : " << maxJobTime << " second(s)" << endl;
	cout << PADDED_OPT << "| - Verbosity : ";
	switch(verbosity){
		case VERBOSE_SILENT:
			cout << "SILENT" << endl;
			break;
		case VERBOSE_ONLY_ERROR:
			cout << "ONLY ERROR" << endl;
			break;
		case VERBOSE_ALL:
			cout << "ALL" << endl;
			break;
	}
	cout << PADDED_OPT << "| - Logging : ";
	switch(logLevel){
		case LOG_NONE:
			cout << "NONE" << endl;
			break;
		case LOG_ONLY_FAILED:
			cout << "ONLY FAILED" << endl;
			break;
		case LOG_ONLY_SUCCESS:
			cout << "ONLY SUCCESS" << endl;
			break;
		case LOG_ALL:
			cout << "ALL" << endl;
			break;
	}
	cout << PADDED_OPT << "| - Fake execution : " << boolalpha << fakeExecution << endl;
	
	//display lists only if not empty
	if(!filtersWhiteFileslist.empty()){
		cout << PADDED_OPT << "| - Whitelist files : " <<endl;
		for(vector<FileManager*>::const_iterator it = filtersWhiteFileslist.begin(); it != filtersWhiteFileslist.end(); it++){
			cout << PADDED_OPT << "|" << (*it)->toString() << endl; 
		}
	}
	if(!filtersBlackFileslist.empty()){
		cout << PADDED_OPT << "| - Blacklist files : "<<endl;
		for(vector<FileManager*>::const_iterator it = filtersBlackFileslist.begin(); it != filtersBlackFileslist.end(); it++){
			cout << PADDED_OPT << "|" << (*it)->toString() << endl; 
		}
	}
	if(!jobsFilesList.empty()){
		cout << PADDED_OPT << "| - XML Input files : "<<endl;
		for(vector<FileManager*>::const_iterator it = jobsFilesList.begin(); it != jobsFilesList.end(); it++){
			cout << PADDED_OPT << "|" << (*it)->toString() << endl; 
		}
	}
}

JobConfiguration::~JobConfiguration() {
	//cleaners
	for(vector<FileManager*>::iterator it = jobsFilesList.begin(); it != jobsFilesList.end();){
		safeFree(*it);
		it = jobsFilesList.erase(it);
	}
	for(vector<FileManager*>::iterator it = filtersWhiteFileslist.begin(); it != filtersWhiteFileslist.end();){
		safeFree(*it);
		it = filtersWhiteFileslist.erase(it);
	}
	for(vector<FileManager*>::iterator it = filtersBlackFileslist.begin(); it != filtersBlackFileslist.end();){
		safeFree(*it);
		it = filtersBlackFileslist.erase(it);
	}
}

bool JobConfiguration::parseJobOptions ( int option ) {
	bool value = true;
	switch(option){
		case 's':
			verbosity = VERBOSE_SILENT;
			break;
		case 'v':
			verbosity = VERBOSE_ONLY_ERROR;
			break;
		case 'V':	
			verbosity = VERBOSE_ALL;
			break;
		case 'n':
			logLevel = LOG_ONLY_FAILED;
			break;
		case 'N':
			logLevel = LOG_NONE;
			break;
		case 'a':
			logLevel = LOG_ONLY_SUCCESS;
			break;
		case 'A':
			logLevel = LOG_ALL;
			break;
		case 'f':
			fakeExecution = true;
			break;
		case 'F':
			fakeExecution = false;
			break;
		case 'l':
			longNames = true;
			break;
		case 'K':
			logLevel = (logIntoFileMode)atoi(optarg);
			break;
		case 'Y':
			verbosity = (verboseMode)atoi(optarg);
			break;
		case 't':
			maxJobTime = atof(optarg);
			break;
		case 'w':
			addFiles(new string(optarg), filtersWhiteFileslist);
			break;
		case 'b':
			addFiles(new string(optarg), filtersBlackFileslist);
			break;
		default:
			value = false;
			break;
	}
	return value;
}

void JobConfiguration::addFiles ( std::string* chain, std::vector<FileManager*>& list) {
	int current = 0;
	size_t next = 0;
	string *tmp = NULL;
	
	//pre actions
	assert(chain != NULL);
	
	//split chain on each comma and add each found file into given list
	do{
		//look for next comma
		next = chain->find_first_of(",", current);
		//get this substring
		tmp = new string(chain->substr(current, next-current));
		assert(tmp != NULL);
		if(*tmp != "")
			list.push_back(new FileManager(tmp, INPUT_ACCESS));
		current = next+1;
	}
	while (next != string::npos);
}
const std::vector< FileManager* >& JobConfiguration::getFiltersBlackFiles() const {
	return filtersBlackFileslist;
}
const std::vector< FileManager* >& JobConfiguration::getFiltersWhiteFiles() const {
	return filtersWhiteFileslist;
}

const std::vector< FileManager* >& JobConfiguration::getJobsFiles() const {
	return jobsFilesList;
}
std::vector< FileManager* >& JobConfiguration::getJobsFilesNoLock() {
	return jobsFilesList;
}
std::vector< FileManager* >& JobConfiguration::getWhiteFilesNoLock() {
	return filtersWhiteFileslist;
}
std::vector< FileManager* >& JobConfiguration::getBlackFilesNoLock() {
	return filtersBlackFileslist;
}

logIntoFileMode JobConfiguration::getLogLevel() const {
	return logLevel;
}
unsigned int JobConfiguration::getMaxJobTime() const {
	return maxJobTime;
}
verboseMode JobConfiguration::getVerbosity() const {
	return verbosity;
}
bool JobConfiguration::isFake() const {
	return fakeExecution;
}
bool JobConfiguration::wantLongNames() const {
	return longNames;
}
void JobConfiguration::addJobsFiles ( char* argv ) {
	string* arg;
	
	//pre actions
	assert(argv != NULL);
	
	arg = new string(argv);
	assert(arg != NULL);
	
	jobsFilesList.push_back(new FileManager(arg, INPUT_ACCESS));
	safeFree(arg);
}

unsigned int JobConfiguration::getNbMaxJobs() const
{
	return nbMaxJobs;
}

void JobConfiguration::loadFile(const XMLConfigParser& parser)
{
	int value;
	
	//loading, for each item in config file
	parser.loadInt(value, XMLConfigParser::JOB_CONF, "verbosity"); 
	verbosity = (verboseMode)value;
	parser.loadInt(value, XMLConfigParser::JOB_CONF, "logging");
	logLevel = (logIntoFileMode)value;
	parser.loadBool(fakeExecution, XMLConfigParser::JOB_CONF, "fakeExecution");
	parser.loadInt((int&)maxJobTime, XMLConfigParser::JOB_CONF, "maxJobTime");
	parser.loadInt((int&)nbMaxJobs, XMLConfigParser::JOB_CONF, "maxNbJobs");
	parser.loadList(jobsFilesList, XMLConfigParser::JOB_CONF, "jobslist");
	parser.loadList(filtersWhiteFileslist, XMLConfigParser::JOB_CONF, "whitelist");
	parser.loadList(filtersBlackFileslist, XMLConfigParser::JOB_CONF, "blacklist");
}

void JobConfiguration::setJobConfiguration(verboseMode verb, logIntoFileMode log, bool fake, bool names, unsigned int jobTime, unsigned int nbJobs){
	verbosity = verb;
	logLevel = log;
	fakeExecution = fake;
	longNames = names;
	maxJobTime = jobTime;
	nbMaxJobs = nbJobs;
}
