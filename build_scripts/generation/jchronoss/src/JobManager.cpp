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


#include "JobManager.h"

using namespace std;

JobManager::JobManager() {

}

JobManager::JobManager(Configuration* config) {
	assert(config != NULL);
	jobsList = NULL;
	nbRemainingJobs = 0;
	nbJobs = 0;
	nbLists = config->system().getNbMaxResources();
	this->config = config;
	sumRun.nbJobs = 0;
	sumRun.nbSuccess = 0;
	sumRun.nbErrors = 0;
	sumRun.nbFailed = 0;
	sumRun.nbSkipped = 0;
	sumRun.nbTotalSlaves = 0;
	sumRun.startRun = 0;
	sumRun.endRun = 0;
	sumRun.elapsed = 0.0;
}


JobManager::~JobManager() {
	
	for(list<Job*>::iterator it = executedJobsList.begin(); it != executedJobsList.end();){
		safeFree(*it);
		it = executedJobsList.erase(it);
	}
	
	safeTabFree(jobsList);
}

void JobManager::addExecutedJob ( Job * current ) {
	assert(current != NULL);
	executedJobsList.push_back(current);
}

void JobManager::addJob ( Job * current ) {
	assert(current != NULL);
	jobsList[current->getNbResources() - 1].push_back(current);
}
int JobManager::getHigherValidList(size_t nbResources) const {
	int val = -1;
	size_t size = (nbLists < nbResources) ? nbLists : nbResources; 
	for(int i = size -1 ; i >= 0; i--){
		if(!jobsList[i].empty()){
			val = i;
			break;
		}
	}
	return val;

}

size_t JobManager::getNbJobs() const
{
  return nbJobs;
}


void JobManager::cleanInvalidJobs() {
	string stringTries = 
		"/!\\ TOO MUCH TRIES TO LAUNCH; This test is considered as failed !!! /!\\";
	string stringNotRunnable = 
		"/!\\ NOT RUNNABLE TEST; Some dependencies are not resolved. This test cannot be launched !!! /!\\";
	string stringInvalid = 
		"/!\\ INVALID DEPS; Some dependencies was not found in our list for this test. By safety, this test was not be scheduled  !!! /!\\";
	string name;
	for(size_t i = 0; i < nbLists; i++){
		for(list<Job*>::iterator it = jobsList[i].begin(); it != jobsList[i].end();){
			if((*it)->isDepInvalid(false)){
				(*it)->updateStatus(NOT_RUNNABLE);
			} else if((*it)->getNbTries() > Job::DEFAULT_MAX_TRIES)
				(*it)->updateStatus(MUCH_TRIES);

			if(config->job().wantLongNames())
				name = (*it)->getName();
			else name = (*it)->getShortName();
			switch((*it)->getStatus()){
				case MUCH_TRIES:
					addExecutedJob((*it));
					nbRemainingJobs--;
					printLine(PREFIX_URUN, "     -/-      (------) %8.2f   %s\n",0.00, name.c_str());
					(*it)->addResult(-1, 0.0, 0.0, stringTries);
					it = jobsList[i].erase(it);
					break;
				
				case INVALID_DEPS:
					addExecutedJob((*it));
					nbRemainingJobs--;
					printLine(PREFIX_URUN, "     -/-      (------) %8.2f   %s\n",0.00, name.c_str());
					(*it)->addResult(-1, 0.0, 0.0, stringInvalid);
					it = jobsList[i].erase(it);
					break;
				
				case NOT_RUNNABLE:
					addExecutedJob((*it));
					nbRemainingJobs--;
					printLine(PREFIX_URUN, "     -/-      (------) %8.2f   %s\n",0.00, name.c_str());
					(*it)->addResult(-1, 0.0, 0.0, stringNotRunnable);
					it = jobsList[i].erase(it);
					break;
				
				default: 
					it++; 
					break;
			}
		}
	}
}

bool JobManager::currentListIsEmpty ( int indice ) const {
	return jobsList[indice].empty();
}
void JobManager::display() const {
	cout 	<< "+---------------- JOB MANAGER -----------------+" << endl
		<< "| - Tests = " << nbJobs << endl
		<< "| - Remaining = " << nbRemainingJobs << endl
		<< "| - Nb lists = " << nbLists << endl
		<< "+------------------- TO RUN -------------------+" << endl;

	for(size_t i = 0 ; i < nbLists; i++){
		if(!jobsList[i].empty()){
			cout	<< "|- Required : " << i+1 << endl;
			for(list<Job*>::iterator it = jobsList[i].begin(); it != jobsList[i].end(); it++){
				//cout << "|\t - " << (*it)->getName() << endl;
				(*it)->display();
			}
		}
	}
	cout << "+-------------------EXECUTED ------------------+" << endl;
	for(list<Job*>::const_iterator it = executedJobsList.begin(); it != executedJobsList.end(); it++){
		(*it)->display();
	}
			
	cout << "+----------------------------------------------+" << endl;
	

}
bool JobManager::isEmpty() const {
	for(size_t i = 0 ; i < nbLists; i++)
		if(!jobsList[i].empty()) return false;
	return true;
}

void JobManager::pullJobsFromFiles () {
	int cpt = 0;
	size_t val = 0, max_tmp = 0, max = 0;
	FilterBox filterbox;
	vector<FileManager*> vFiles = config->job().getJobsFiles();
	list<Job*>* *tabJobs = new list<Job*>*[vFiles.size()];

	for(vector<FileManager*>::const_iterator white = config->job().getFiltersWhiteFiles().begin(); white != config->job().getFiltersWhiteFiles().end(); white++){
		filterbox.addFilter((*white), WHITELIST);
	}
	
	for(vector<FileManager*>::const_iterator black = config->job().getFiltersBlackFiles().begin(); black != config->job().getFiltersBlackFiles().end(); black++){
		filterbox.addFilter((*black), BLACKLIST);
	}

	TIME_MEASURE_INIT();
#ifdef ENABLE_OPENMP
	#pragma omp parallel shared(max) firstprivate(max_tmp)
	{
		#pragma omp for nowait
#endif
			for(size_t i = 0; i < vFiles.size(); i++){
				XMLJobParser parser(vFiles[i]->toString(), &filterbox);
				tabJobs[i] = parser.parseFromFile(&val);
				if(tabJobs[i] == NULL) continue;
				for(list<Job*>::iterator it = tabJobs[i]->begin(); it != tabJobs[i]->end(); it++){
					max_tmp = std::max(max_tmp, (*it)->getNbResources());
				}
			}
#ifdef ENABLE_OPENMP
		#pragma omp critical
		{
#endif
			max = std::max(max, max_tmp);
#ifdef ENABLE_OPENMP
		}
	}
#endif
	XMLParser::cleanUp();
	TIME_MEASURE_END("Files parsing");
	
	nbLists = max;
	if(nbLists > config->system().getNbMaxResources())
		printError("Some jobs need more resources than the max availables resources !!!!", JE_LACK_RES);
	jobsList = new list<Job*>[nbLists];
	
	TIME_MEASURE_INIT();
	for(size_t i = 0; i < vFiles.size(); i++){
		if(tabJobs[i] == NULL) continue;
		for(list<Job*>::iterator it = tabJobs[i]->begin(); it != tabJobs[i]->end();){
			if((*it)->getExpectedTime() == -1)
				(*it)->setExpectedTime(config->job().getMaxJobTime());
			addJob(*it);
			cpt++;
			it = tabJobs[i]->erase(it);
		}
		safeFree(tabJobs[i]);
	}
	TIME_MEASURE_END("Jobs table filling");
	safeTabFree(tabJobs);
	
	if(config->isMaster())
		nbRemainingJobs = cpt;
	else
		nbRemainingJobs = val;
	nbJobs = cpt;
	sumRun.nbJobs = nbJobs;
	TIME_MEASURE_INIT();
	resolveJobsDeps();
	TIME_MEASURE_END("Resolving deps");
}

void JobManager::pushInOneFile(FileManager* file){
	
	string notRun = "/!\\ For unknown reasons, this test had not be launched. It is considered as an error !!! /!\\";
	string curPathName, outputFileName, command, data, name, group;
	ostringstream mainContentString, headerString;
	size_t pos = 0;
	FileManager* output = NULL;
	size_t nbFailures = 0, nbSuccess = 0, nbErrors = 0, nbSkipped = 0;
	double totalTime = 0.0;
	
	assert(file != NULL);
	outputFileName = (*config->system().getOutputDirectory() == "") ?  file->getPath()+DEFAULT_OUTPUT_FILENAME+file->getBaseName() : *config->system().getOutputDirectory()+DEFAULT_OUTPUT_FILENAME+file->getBaseName();
	output =  new FileManager(&outputFileName, OUTPUT_ACCESS);
	
	CHECK(output->open());
	
	//time display control
	mainContentString << fixed << setprecision(2);
	headerString << fixed << setprecision(2);
	
	for(list<Job*>::iterator it = executedJobsList.begin(); it != executedJobsList.end(); it++){
		if((*it)->getReferentFilename() != file->toString()) continue;
		
		name = (*it)->getShortName();
		group = (*it)->getName();
		pos = group.find_last_of(".");
		if(pos < group.size())
			group = group.substr(0,pos);
		command = (*it)->getCommand();
		data = (*it)->getResult().getData();
		
		HTMLEncoding(group);
		HTMLEncoding(name);
		HTMLEncoding(command);
		HTMLEncoding(data);
	
		totalTime += (*it)->getResult().getTime();
		
		mainContentString << "\t<testcase classname=\"" << group <<"\" name=\""<< name << "\" time=\"" << (*it)->getResult().getTime() << "\" >\n";
				
		switch((*it)->getStatus()){
			case PASSED:	
				mainContentString << "\t\t<success message=\"" << command << "\" />" << endl;
				if(config->job().getLogLevel() == LOG_ALL || config->job().getLogLevel() == LOG_ONLY_SUCCESS)
					mainContentString << "\t\t<system-out>"<< data << endl <<"\t\t</system-out>" << endl;
				
				nbSuccess++;
				break;
			case FAILED:
				mainContentString << "\t\t<failure message=\"" << command << "\" type=\"ExecutionError\"/>" << endl;
				
				if(config->job().getLogLevel() == LOG_ALL || config->job().getLogLevel() == LOG_ONLY_FAILED){
					mainContentString << "\t\t<system-out>"<< data << endl << "\t\t</system-out>" << endl;
				}
				nbFailures++;
				break;
			case NOT_RUN:
			case MUCH_TRIES:
				mainContentString << "\t\t<skipped message=\""<< command << "\" />" << endl;
				if(config->job().getLogLevel() == LOG_ALL || config->job().getLogLevel() == LOG_ONLY_FAILED)
					mainContentString << "\t\t<system-out>"<< data << endl << "\t\t</system-out>" << endl;
				nbSkipped++;
				break;
			case NOT_RUNNABLE:
				mainContentString << "\t\t<skipped message=\""<< command << "\" />" << endl;
				if(config->job().getLogLevel() == LOG_ALL || config->job().getLogLevel() == LOG_ONLY_FAILED)
					mainContentString << "\t\t<system-out>"<< data << endl << "\t\t</system-out>" << endl;
				nbSkipped++;
				break;
			case INVALID_DEPS:
				mainContentString << "\t\t<error message=\"" << command << "\" type=\"EnvironmentError\"/>" << endl;
				
				if(config->job().getLogLevel() == LOG_ALL || config->job().getLogLevel() == LOG_ONLY_FAILED)
					mainContentString << "\t\t<system-out>" << data << endl << "</system-out>" << endl;
				
				nbErrors++;
				break;
		}
		      mainContentString << "\t</testcase>" << endl;

	}

	// if empty file, avoid to create it
	if(mainContentString.str().size() == 0){
		CHECK(output->close());
		remove(outputFileName.c_str());
		safeFree(output);
		return;
	}
	
	headerString << "<?xml version=\"1.0\"?>" << endl;
	headerString << "<testsuite name=\""<< group << "\" failures=\""<< nbFailures << "\" success=\""<< nbSuccess << "\" errors=\""<< nbErrors << "\" skipped=\""<< nbSkipped <<  "\" time=\"" << totalTime << "\">" << endl;

	(*output) << headerString.str();
	(*output) << mainContentString.str();
	(*output) << "</testsuite>";
	CHECK(output->close());
	safeFree(output);

	sumRun.nbSuccess += nbSuccess;
	sumRun.nbFailed += nbFailures;
	sumRun.nbErrors += nbErrors;
	sumRun.nbSkipped += nbSkipped;
}

void JobManager::pushJobsIntoFiles() {

	vector<FileManager*> vFiles = config->job().getJobsFiles();
	for(vector<FileManager*>::iterator it = vFiles.begin(); it != vFiles.end(); it++){
		pushInOneFile((*it));
	}
}

Job* JobManager::resolveADep ( std::string pattern ) {
	for(size_t i = 0; i < nbLists; i++){
		for(list<Job*>::iterator it = jobsList[i].begin(); it != jobsList[i].end(); it++){
			if ((*it)->getName() == pattern)
				return (*it);
		}
	}
	for(list<Job*>::iterator it = executedJobsList.begin(); it != executedJobsList.end(); it++){
		if ((*it)->getName() == pattern)
			return (*it);
	}
	return NULL;
}

void JobManager::resolveAJobDeps ( Job* current ) {
	Job* dep;
	string* depName = NULL;
	
	assert(current != NULL);
	for(int i=0; i < current->getNbDeps(); i++){
		depName = current->getDepsNames().at(i);
		if(*depName == current->getName()){
			printWarning("Dependency on itself : " << *depName << " !!!");
			current->updateStatus(INVALID_DEPS);
		}
		dep = resolveADep(*depName);
		if(dep != NULL){
			current->addDependency(dep);
		} else {
			printWarning("Dependency \""+*depName+"\" not found !!!");
			current->updateStatus(INVALID_DEPS);
		}
	}
}
void JobManager::resolveJobsDeps() {
#ifdef ENABLE_OPENMP
	#pragma omp parallel 
	{
#endif
		for(size_t i = 0; i < nbLists; i++){
				for(std::list<Job*>::iterator it = jobsList[i].begin(); it != jobsList[i].end(); it++){
#ifdef ENABLE_OPENMP
					#pragma omp single nowait 
					{
#endif
						resolveAJobDeps(*it);
#ifdef ENABLE_OPENMP
					}
#endif
				}
		}
#ifdef ENABLE_OPENMP
	}	
#endif
}
size_t JobManager::getNbRemainingJobs() const
{
	return nbRemainingJobs;
}

void JobManager::update(int val) {
	nbRemainingJobs += val;
}

std::list< Job* >* JobManager::jobsLists() const
{
	assert(jobsList != NULL);
	return jobsList;
}

size_t JobManager::getNbLists() const
{
	return nbLists;
}

void JobManager::createBackup(std::list<Job*> * extrasJobs) {
	const short MAX = 256;
	std::string file = *config->system().getBuildDirectory() + "/jns_chkpt.json";
	FileManager backupFile(&file, OUTPUT_ACCESS);
	Json::Value graph;
	Json::FastWriter writer;
	std::string final, temp;
	unsigned int i = 0, j = 0, extras = 0;
	char buf[MAX];
	time_t tmp;
	
	/***** PRE ACTIONS *****/
	time(&tmp);
	backupFile.open();
	if(!backupFile.isOpen()){
		printError("Cannot open " << backupFile.toString() << " !", JE_NFND_FIL);
	}
	if(getcwd(buf, MAX) == NULL)
		printWarning("Current directory too long. Not saved...");
	else
		graph["config"]["path"] = buf;

	/*********** CONFIGURATION MAIN DATA ********/
	graph["config"]["status"] = config->isMaster();
	graph["config"]["exePath"] = config->getExeName();
	if(config->getConfigFile() != NULL) graph["config"]["file"] = *config->getConfigFile();
	
	/******** JOB CONFIGURATION MAIN DATA ********/
	graph["configJob"]["verbosity"] = config->job().getVerbosity();
	graph["configJob"]["logging"] = config->job().getLogLevel();
	graph["configJob"]["fake"] = config->job().isFake();
	graph["configJob"]["longNames"] = config->job().wantLongNames();
	graph["configJob"]["maxJobTime"] = config->job().getMaxJobTime();
	graph["configJob"]["maxJobs"] = config->job().getNbMaxJobs();
	
	if(config->job().getJobsFiles().size() > 0){
		temp = config->job().getJobsFiles().front()->toString();
		for(std::vector<FileManager*>::const_iterator it = config->job().getJobsFiles().begin()+1; it != config->job().getJobsFiles().end(); it++)
			temp += ","+(*it)->toString();
		graph["configJob"]["jobsList"] = temp;
	}
	
	if(config->job().getFiltersWhiteFiles().size() > 0){
		temp = config->job().getFiltersWhiteFiles().front()->toString();
		for(std::vector<FileManager*>::const_iterator it = config->job().getFiltersWhiteFiles().begin()+1; it != config->job().getFiltersWhiteFiles().end(); it++)
			temp += ","+(*it)->toString();
		graph["configJob"]["whiteList"] = temp;
	}	
	if(config->job().getFiltersBlackFiles().size() > 0){
		temp = config->job().getFiltersBlackFiles().front()->toString();
		for(std::vector<FileManager*>::const_iterator it = config->job().getFiltersBlackFiles().begin()+1; it != config->job().getFiltersBlackFiles().end(); it++)
			temp += ","+(*it)->toString();
		graph["configJob"]["blackList"] = temp;
	}

	/*********** SYTEM CONFIGURATION MAIN DATA ********/
	graph["configSystem"]["maxResources"] = (unsigned int)config->system().getNbMaxResources();
	graph["configSystem"]["nbmaxslaves"] = (unsigned int)config->system().getNbMaxSlaves();
	graph["configSystem"]["policy"] = config->system().getPolicy();
	graph["configSystem"]["maxbytes"] = (unsigned int)config->system().getFlowMaxBytes();
	graph["configSystem"]["maxslavetime"] = config->system().getMaxSlaveTime();
	graph["configSystem"]["minslavetime"] = config->system().getMinSlaveTime();
	graph["configSystem"]["autokill"] = config->system().getAutokill();
	graph["configSystem"]["output"] = *config->system().getOutputDirectory();
	graph["configSystem"]["build"] = *config->system().getBuildDirectory();
	if(config->system().getJobsLauncherCommand() != NULL)
		graph["configSystem"]["launcher"] = *config->system().getJobsLauncherCommand();
	if(config->system().getCompilationJobsLauncherCommand() != NULL)
		graph["configSystem"]["compilLauncher"] = *config->system().getCompilationJobsLauncherCommand();

	/*********** JOBS MANAGER MAIN DATA ********/
	i = 0;
	for(size_t a = 0; a < nbLists; a++){
		for(std::list<Job*>::iterator it = jobsList[a].begin(); it != jobsList[a].end(); it++){
			graph["jobsList"][i]["nbResources"] = (unsigned int)(*it)->getNbResources() ;
			graph["jobsList"][i]["nbDeps"] = (unsigned int)(*it)->getNbDeps() ;
			graph["jobsList"][i]["nbTries"] = (unsigned int)(*it)->getNbTries() ;
			graph["jobsList"][i]["name"] = (*it)->getName() ;
			j = 0;
			for(vector<string*>::const_iterator it2 = (*it)->getDepsNames().begin(); it2 != (*it)->getDepsNames().end(); it2++)
				graph["jobsList"][i]["deps"][j] = *(*it2);
			j = 0;
			for(vector<JobConstraint*>::const_iterator it2 = (*it)->getConstraints().begin(); it2 != (*it)->getConstraints().end(); it2++)
				graph["jobsList"][i]["constraints"][j] = (*it2)->getName() ;
			graph["jobsList"][i]["command"] = (*it)->getCommand() ;
			graph["jobsList"][i]["file"] = (*it)->getReferentFilename() ;
			graph["jobsList"][i]["returncode"] = (*it)->getExpectedReturn() ;
			graph["jobsList"][i]["time"] = (*it)->getExpectedTime() ;
			graph["jobsList"][i]["id"] = (unsigned int)(*it)->getId() ;
			i++;
		}
	}
	for(std::list<Job*>::const_iterator it = extrasJobs->begin(); it != extrasJobs->end(); it++){
		graph["jobsList"][i]["nbResources"] = (unsigned int)(*it)->getNbResources() ;
		graph["jobsList"][i]["nbDeps"] = (unsigned int)(*it)->getNbDeps() ;
		graph["jobsList"][i]["nbTries"] = (unsigned int)(*it)->getNbTries() ;
		graph["jobsList"][i]["name"] = (*it)->getName() ;
		j = 0;
		for(vector<string*>::const_iterator it2 = (*it)->getDepsNames().begin(); it2 != (*it)->getDepsNames().end(); it2++)
			graph["jobsList"][i]["deps"][j] = *(*it2);
		j = 0;
		for(vector<JobConstraint*>::const_iterator it2 = (*it)->getConstraints().begin(); it2 != (*it)->getConstraints().end(); it2++)
			graph["jobsList"][i]["constraints"][j] = (*it2)->getName() ;
		graph["jobsList"][i]["command"] = (*it)->getCommand() ;
		graph["jobsList"][i]["file"] = (*it)->getReferentFilename() ;
		graph["jobsList"][i]["returncode"] = (*it)->getExpectedReturn() ;
		graph["jobsList"][i]["time"] = (*it)->getExpectedTime() ;
		graph["jobsList"][i]["id"] = (unsigned int)(*it)->getId() ;
		i++;
		extras++;
	}
	i=0;
	for(std::list<Job*>::iterator it = executedJobsList.begin(); it != executedJobsList.end(); it++){
		graph["jobsExecutedList"][i]["nbResources"] = (unsigned int)(*it)->getNbResources() ;
		graph["jobsExecutedList"][i]["nbDeps"] = (unsigned int)(*it)->getNbDeps() ;
		graph["jobsExecutedList"][i]["nbTries"] = (unsigned int)(*it)->getNbTries() ;
		graph["jobsExecutedList"][i]["name"] = (*it)->getName() ;
		j = 0;
		for(vector<string*>::const_iterator it2 = (*it)->getDepsNames().begin(); it2 != (*it)->getDepsNames().end(); it2++)
			graph["jobsExecutedList"][i]["deps"][j] = *(*it2);
		j = 0;
		for(vector<JobConstraint*>::const_iterator it2 = (*it)->getConstraints().begin(); it2 != (*it)->getConstraints().end(); it2++)
			graph["jobsExecutedList"][i]["constraints"][j] = (*it2)->getName() ;
		graph["jobsExecutedList"][i]["command"] = (*it)->getCommand() ;
		graph["jobsExecutedList"][i]["file"] = (*it)->getReferentFilename() ;
		graph["jobsExecutedList"][i]["returncode"] = (*it)->getExpectedReturn() ;
		graph["jobsExecutedList"][i]["time"] = (*it)->getExpectedTime() ;
		graph["jobsExecutedList"][i]["status"] = (*it)->getStatus() ;
		graph["jobsExecutedList"][i]["id"] = (unsigned int)(*it)->getId() ;
		graph["jobsExecutedList"][i]["result"]["finalRC"] = (*it)->getResult().getFinalRC() ;
		graph["jobsExecutedList"][i]["result"]["id"] = (unsigned int)(*it)->getResult().getId() ;
		graph["jobsExecutedList"][i]["result"]["startTime"] = (*it)->getResult().getStartTime() ;
		graph["jobsExecutedList"][i]["result"]["execTime"] = (*it)->getResult().getTime() ;
		graph["jobsExecutedList"][i]["result"]["flow"] = (*it)->getResult().getData() ;
		i++;
	}
	graph["nbJobs"] = (unsigned int)nbJobs;
	graph["nbLists"] = (unsigned int)nbLists;
	graph["nbRemainingJobs"] = (unsigned int)(nbRemainingJobs+extras);

	/*********** SUMMARY MAIN DATA ********/
	graph["summary"]["nbJobs"] = (unsigned int)nbJobs;
	graph["summary"]["nbSuccess"] = (unsigned int)sumRun.nbSuccess;
	graph["summary"]["nbErrors"] = (unsigned int)sumRun.nbErrors;
	graph["summary"]["nbFailed"] = (unsigned int)sumRun.nbFailed;
	graph["summary"]["nbSkipped"] = (unsigned int)sumRun.nbSkipped;
	graph["summary"]["nbTotalSlaves"] = (unsigned int)sumRun.nbTotalSlaves;
	graph["summary"]["elapsed"] = (float)(sumRun.elapsed+difftime(tmp, sumRun.startRun));
	
	/****** FINAL PUSH *****/
	final = writer.write(graph);
	backupFile << final;
	backupFile.close();
	safeFree(extrasJobs);
}


void JobManager::loadBackup(){
	const short MAX = 256;
	ifstream fd(config->getRestoredFile().c_str());
	Json::Value graph;
	Json::Reader reader;
	std::string *temp = NULL, *temp2 = NULL;
	vector<string*> depVec;
	vector<JobConstraint*> consVec;
	char buf[MAX];
	if(!reader.parse(fd, graph)){
		printError("Fail during JCHRONOSS restarting.\n ERROR        : Unable to parse Json file !", JE_UNKNOWN);
	}
	
	if(getcwd(buf, MAX) == NULL)
		printWarning("Current directory too long. Not loaded...");
	else {
		string cur(buf);
		if(cur != graph["config"]["path"].asString())
		printError("Restart from diffrent directory isn't allowed yet\n ERROR        : You have to restart from " << graph["config"]["path"].asString(), JE_WORK_INP);
	}
	
	nbJobs = (size_t)graph["nbJobs"].asUInt();
	nbLists = (size_t)graph["nbLists"].asUInt();
	nbRemainingJobs = (size_t)graph["nbRemainingJobs"].asUInt();
	
	temp = (graph["config"]["file"] == Json::nullValue) ? NULL : new string(graph["config"]["file"].asString());
	config->setConfiguration(graph["config"]["exePath"].asString(), EXEC_RESTORED, temp);

	temp = (graph["configSystem"]["launcher"] == Json::nullValue) ? NULL : new string(graph["configSystem"]["launcher"].asString());
	temp2 = (graph["configSystem"]["compilLauncher"] == Json::nullValue) ? NULL : new string(graph["configSystem"]["compilLauncher"].asString());
	config->system().setSystemConfiguration(
	temp, 
	temp2, 
	new string(graph["configSystem"]["output"].asString()),
	new string(graph["configSystem"]["build"].asString()),
	graph["configSystem"]["maxResources"].asUInt(),
	graph["configSystem"]["nbmaxslaves"].asUInt(),
	graph["configSystem"]["autokill"].asUInt(),
	graph["configSystem"]["minslavetime"].asUInt(),
	graph["configSystem"]["maxslavetime"].asUInt(),
	(policyMode)graph["configSystem"]["policy"].asInt(),
	graph["configSystem"]["maxbytes"].asUInt());

	config->job().setJobConfiguration(
	(verboseMode)graph["configJob"]["verbosity"].asInt(),
	(logIntoFileMode)graph["configJob"]["logging"].asInt(),
	graph["configJob"]["fake"].asBool(),
	graph["configJob"]["longNames"].asBool(),
	graph["configJob"]["maxJobTime"].asUInt(),
	graph["configJob"]["maxJobs"].asUInt());

	std::string list;
	list = graph["configJob"]["jobsList"].asString();
	config->job().addFiles(new string(list), config->job().getJobsFilesNoLock());
	list = graph["configJob"]["whiteList"].asString();
	config->job().addFiles(new string(list), config->job().getWhiteFilesNoLock());
	list = graph["configJob"]["blackList"].asString();
	config->job().addFiles(new string(list), config->job().getBlackFilesNoLock());

	jobsList = new std::list<Job*>[config->system().getNbMaxResources()];

	if(graph["jobsList"] != Json::nullValue){
		const Json::Value jobsGraphList = graph["jobsList"];
		for(unsigned int index = 0; index < jobsGraphList.size(); index++){
			consVec.clear();
			depVec.clear();
			if(jobsGraphList[index]["deps"] != Json::nullValue){
				const Json::Value depsList = jobsGraphList[index]["deps"];
				for(unsigned int index = 0; index < depsList.size(); index++)
					depVec.push_back(new string(depsList[index].asString()));
			}

			if(jobsGraphList[index]["constraints"] != Json::nullValue){
				const Json::Value constraintsList = jobsGraphList[index]["constraints"];
				for(unsigned int index = 0; index < constraintsList.size(); index++)
					consVec.push_back(new JobConstraint(constraintsList[index].asString()));
			}
			addJob(new Job(
				jobsGraphList[index]["name"].asString(),
				jobsGraphList[index]["command"].asString(),
				jobsGraphList[index]["nbResources"].asUInt(),
				jobsGraphList[index]["returncode"].asInt(),
				jobsGraphList[index]["time"].asFloat(),
				depVec,
				consVec,
				jobsGraphList[index]["file"].asString()
			));
		}
	}
	if(graph["jobsExecutedList"] != Json::nullValue){
		const Json::Value jobsGraphList = graph["jobsExecutedList"];
		for(unsigned int index = 0; index < jobsGraphList.size(); index++){
			consVec.clear();
			depVec.clear();
			if(jobsGraphList[index]["deps"] != Json::nullValue){
				const Json::Value depsList = jobsGraphList[index]["deps"];
				for(unsigned int index = 0; index < depsList.size(); index++)
					depVec.push_back(new string(depsList[index].asString()));
			}

			if(jobsGraphList[index]["constraints"] != Json::nullValue){
				const Json::Value constraintsList = jobsGraphList[index]["constraints"];
				for(unsigned int index = 0; index < constraintsList.size(); index++){
					consVec.push_back(new JobConstraint(constraintsList[index].asString()));
				}
			}
			
			Job * cur = new Job(
				jobsGraphList[index]["name"].asString(),
				jobsGraphList[index]["command"].asString(),
				jobsGraphList[index]["nbResources"].asUInt(),
				jobsGraphList[index]["returncode"].asInt(),
				jobsGraphList[index]["time"].asFloat(),
				depVec,
				consVec,
				jobsGraphList[index]["file"].asString()
			);
			cur->updateStatus((JobStatus)jobsGraphList[index]["status"].asInt());
			cur->addResult(
				jobsGraphList[index]["result"]["finalRC"].asInt(),
				jobsGraphList[index]["result"]["execTime"].asFloat(),
				jobsGraphList[index]["result"]["startTime"].asFloat(),
				jobsGraphList[index]["result"]["flow"].asString());
			addExecutedJob(cur);
		}
	}
	sumRun.nbJobs = graph["summary"]["nbJobs"].asUInt();
	sumRun.nbErrors = graph["summary"]["nbErrors"].asUInt();
	sumRun.nbSuccess = graph["summary"]["nbSuccess"].asUInt();
	sumRun.nbFailed = graph["summary"]["nbFailed"].asUInt();
	sumRun.nbSkipped = graph["summary"]["nbSkipped"].asUInt();
	sumRun.nbTotalSlaves = graph["summary"]["nbTotalSlaves"].asUInt();
	sumRun.elapsed = (double)graph["summary"]["elapsed"].asFloat();

	time(&(sumRun.startRun));	
	fd.close();
	resolveJobsDeps();
}

void JobManager::displaySummary() const {
	double elapsed = difftime(sumRun.endRun, sumRun.startRun);
	cout << COLOR_NRUN"+------------------------- RUN SUMMARY ------------------------+"COLOR_NORM << endl;
	cout 
		<< COLOR_NRUN"|"COLOR_NRUN" * Executed jobs  : " << sumRun.nbJobs << endl
		<< COLOR_NRUN"|"COLOR_FAIL"    --> Fails     : " << sumRun.nbFailed << endl
		<< COLOR_NRUN"|"COLOR_FAIL"    --> Errors    : " << sumRun.nbErrors << endl
		<< COLOR_NRUN"|"COLOR_URUN"    --> Skips     : " << sumRun.nbSkipped << endl
		<< COLOR_NRUN"|"COLOR_PASS"    --> Successes : " << sumRun.nbSuccess << endl
		<< COLOR_NRUN"|"COLOR_NRUN" * Elapsed time   : " << convertDate(elapsed+sumRun.elapsed) << endl
		<< COLOR_NRUN"|"COLOR_NRUN" * Slave launchs  : " << sumRun.nbTotalSlaves << endl
	//<< "" << << endl
	;
	cout << COLOR_NRUN"+--------------------------------------------------------------+"COLOR_NORM << endl;
}

void JobManager::updateFinalState() const
{
	if(sumRun.nbErrors) config->setReturnStatus(RETURN_WITH_ERRORS);
	if(sumRun.nbFailed) config->setReturnStatus(RETURN_WITH_FAILURES);
	if(sumRun.nbSkipped) config->setReturnStatus(RETURN_WITH_SKIPPED);
}
