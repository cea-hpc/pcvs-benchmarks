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
#include "network.h"
#include "OutputFormatJSON.h"
#include "OutputFormatJUnit.h"
#include "OutputFormatYAML.h"

extern int backend_sock;
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
	sumRun.nbDisabled = 0;
	sumRun.nbTotalSlaves = 0;
	sumRun.startRun = 0;
	sumRun.endRun = 0;
	sumRun.elapsed = 0.0;

	// May be useless
	for(size_t i = 0; i < MAX_JOB_HT; i++)
		hashTable[i].clear();
}


JobManager::~JobManager() {

	for(list<Job*>::iterator it = executedJobsList.begin(); it != executedJobsList.end();){
		safeFree(*it);
		it = executedJobsList.erase(it);
	}

	safeTabFree(jobsList);
	for(size_t i = 0; i < MAX_JOB_HT; i++)
		hashTable[i].clear();
}

void JobManager::addExecutedJob ( Job * current ) {
	assert(current != NULL);
	executedJobsList.push_back(current);
#ifdef ENABLE_PLUGIN_SERVER
	if(current->getStatus() > NOT_RUN && backend_sock > 0)
	{
		OutputFormatJSON format(config);
		std::string name = current->getShortName();
		std::string group = current->getName();
		std::string command = current->getCommand();
		std::string data = current->getResult().getData();
		double time = current->getResult().getTime();
		HTMLEncoding(group);
		HTMLEncoding(name);
		HTMLEncoding(command);
		HTMLEncoding(data);
		switch(current->getStatus())
		{
			case PASSED:
				format.appendSuccess(group, name, command, data, time); break;
			case FAILED:
				format.appendFailure(group, name, command, data, time); break;
			case NOT_RUN:
			case MUCH_TRIES:
			case NOT_RUNNABLE:
				format.appendSkipped(group, name, command, data, time); break;
			case INVALID_DEPS:
				format.appendError(group, name, command, data, time); break;
			default:
				break;
		}	
		size_t sz = format.stringify().size() * sizeof(char);

		safe_send(backend_sock, &sz, sizeof(sz));
		safe_send(backend_sock, format.stringify().c_str(), sz );
	}
#endif
}

void JobManager::addJob ( Job * current ) {
	assert(current != NULL);
	size_t key = Job::getHash(current->getName()) % MAX_JOB_HT;
	HashedJob job(key, current);

	hashTable[key].push_back(job);
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

list<Job*>::iterator JobManager::delayJob(int numlist, list<Job*>::iterator it)
{
	string stringTries =
		"/!\\ TOO MUCH TRIES TO LAUNCH; This test is considered as failed !!! /!\\", name;
	Job* job = *it;
	if(!job->addATry())
	{
		(*it)->updateStatus(MUCH_TRIES);
		(*it)->addResult(-1, 0.0, 0.0, stringTries);
		if(config->job().wantLongNames())
			name = job->getName();
		else name = job->getShortName();
		printLine(PREFIX_URUN, "     -/-      (MTRIES) %8.2f   %s\n",0.00, name.c_str());
		addExecutedJob(job);
		return pickJob(numlist, it);
	}
	return (++it);

}

list<Job*>::iterator JobManager::pickJob(int numlist, list<Job*>::iterator it)
{
	return jobsList[numlist].erase(it);
}

size_t JobManager::getNbJobs() const
{
	return nbJobs;
}

std::list<Job*>::const_iterator JobManager::getExecutedStart() const
{
	return executedJobsList.begin();
}

std::list<Job*>::const_iterator JobManager::getExecutedEnd() const
{
	return executedJobsList.end();
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
				(*it)->updateStatus(INVALID_DEPS);
			} else if((*it)->getNbTries() > Job::DEFAULT_MAX_TRIES)
				(*it)->updateStatus(MUCH_TRIES);

			if(config->job().wantLongNames())
				name = (*it)->getName();
			else name = (*it)->getShortName();
			switch((*it)->getStatus()){
				case MUCH_TRIES:
					(*it)->addResult(-1, 0.0, 0.0, stringTries);
					addExecutedJob((*it));
					nbRemainingJobs--;
					printLine(PREFIX_URUN, "     -/-      (MTRIES) %8.2f   %s\n",0.00, name.c_str());
					it = jobsList[i].erase(it);
					break;

				case INVALID_DEPS:
					(*it)->addResult(-1, 0.0, 0.0, stringInvalid);
					addExecutedJob((*it));
					nbRemainingJobs--;
					printLine(PREFIX_URUN, "     -/-      (INVDEP) %8.2f   %s\n",0.00, name.c_str());
					it = jobsList[i].erase(it);
					break;

				case NOT_RUNNABLE:
					(*it)->addResult(-1, 0.0, 0.0, stringNotRunnable);
					addExecutedJob((*it));
					nbRemainingJobs--;
					printLine(PREFIX_URUN, "     -/-      (------) %8.2f   %s\n",0.00, name.c_str());
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
			//if((*it)->getExpectedTime() == -1)
				//(*it)->setExpectedTime(config->job().getMaxJobTime());
			if((*it)->getStatus() != NOT_RUN)
				addExecutedJob(*it);
			else
			{
				addJob(*it);
				cpt++;
			}
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
	size_t pos = 0;
	FileManager* output = NULL;
	size_t nbFailures = 0, nbSuccess = 0, nbErrors = 0, nbSkipped = 0, nbDisabled = 0;
	double totalTime = 0.0, time = 0.0;
	bool not_empty = false;

	for(vector<OutputFormat*>::iterator ito = outputs.begin(); ito != outputs.end(); ito++)
	{
		(*ito)->clear();
	}

	for(list<Job*>::iterator iter = executedJobsList.begin(); iter != executedJobsList.end(); iter++){
	//while(! executedJobsList.empty()) {
		Job* it = *iter;
		assert(it->getStatus() != NOT_RUN);
		if(it->getReferentFilename() != file->toString()) 
		{
			continue;
		}

		/* if the following is executed, at least one job will be present in the current file */
		not_empty = true;

		name = it->getShortName();
		group = it->getName();
		pos = group.find_last_of(".");
		if(pos < group.size())
			group = group.substr(0,pos);
		command = it->getCommand();
		data = it->getResult().getData();
		time = it->getResult().getTime();

		HTMLEncoding(group);
		HTMLEncoding(name);
		HTMLEncoding(command);
		HTMLEncoding(data);

		totalTime += time;

		/* iterate over differents expected formats to generate */
		for(vector<OutputFormat*>::iterator ito = outputs.begin(); ito != outputs.end(); ito++)
		{
			switch(it->getStatus())
			{
				case PASSED:
					(*ito)->appendSuccess(group, name, command, data, time); if(ito == outputs.begin()) ++nbSuccess; break;
				case FAILED:
					(*ito)->appendFailure(group, name, command, data, time); if(ito == outputs.begin()) ++nbFailures; break;
				case MUCH_TRIES:
				case NOT_RUNNABLE:
					(*ito)->appendSkipped(group, name, command, data, time); if(ito == outputs.begin())++nbSkipped; break;
				case INVALID_DEPS:
					(*ito)->appendError(group, name, command, data, time); if(ito == outputs.begin())++nbErrors; break;
				case DISABLED: 
					(*ito)->appendDisabled(group, name, command, data, time); if(ito == outputs.begin()) ++nbDisabled; break;
				default:
					break;
			}
		}

		//executedJobsList.pop_back();
	}

	// if empty file, avoid to create it
	if(! not_empty){
		return;
	}

	outputFileName = ((*config->system().getOutputDirectory() == "") ?  file->getPath()+DEFAULT_OUTPUT_FILENAME+file->getBaseName() : *config->system().getOutputDirectory()+DEFAULT_OUTPUT_FILENAME+file->getBaseName());
	for(vector<OutputFormat*>::iterator ito = outputs.begin(); ito != outputs.end(); ito++)
	{
		std::string s = outputFileName + (*ito)->getExt();
		output =  new FileManager(&s, OUTPUT_ACCESS);

		CHECK(output->open());
		(*ito)->appendHeader(nbErrors, nbFailures, nbSkipped, nbSuccess, totalTime);
		(*ito)->appendFooter();
		(*output) << (*ito)->stringify();
		CHECK(output->close());
		safeFree(output);

	}

	sumRun.nbSuccess += nbSuccess;
	sumRun.nbFailed += nbFailures;
	sumRun.nbErrors += nbErrors;
	sumRun.nbSkipped += nbSkipped;
	sumRun.nbDisabled += nbDisabled;
}

void JobManager::pushJobsIntoFiles() {

	const vector<string>& temp =  config->system().getOutputFormats();
	for(vector<string>::const_iterator it = temp.begin(); it != temp.end(); it++)
	{
		if((*it) == "JUNIT")
		{
			outputs.push_back(new OutputFormatJUnit(this->config));
			continue;
		}
		if((*it) == "JSON")
		{
			outputs.push_back(new OutputFormatJSON(this->config));
			continue;
		}
		if((*it) == "YAML")
		{
			outputs.push_back(new OutputFormatYAML(this->config));
			continue;
		}
	}

	vector<FileManager*> vFiles = config->job().getJobsFiles();
	for(vector<FileManager*>::iterator it = vFiles.begin(); it != vFiles.end(); it++){
		pushInOneFile((*it));
	}
}

Job* JobManager::resolveADep ( std::string pattern ) {

	size_t key = Job::getHash(pattern) % MAX_JOB_HT;
	for(list<HashedJob>::iterator it = hashTable[key].begin(); it != hashTable[key].end(); it++)
	{
		if (it->getName() == pattern || it->getShortName() == pattern)
		{
			return it->getPointer();
		}
	}

	return NULL;
}

void JobManager::resolveAJobDeps ( Job* current ) {
	Job* dep;
	string* depName = NULL;

	assert(current != NULL);
	for(int i=0; i < current->getNbDeps(); i++){
		depName = current->getDepsNames().at(i);
		if(*depName == current->getName() || *depName == current->getShortName()){
#ifdef ENABLE_OPENMP
#pragma omp critical
			{
#endif
				printWarning("Dependency on itself : " << *depName << " !!!");
#ifdef ENABLE_OPENMP
			}
#endif
			current->updateStatus(INVALID_DEPS);
		}
		dep = resolveADep(*depName);
		if(dep != NULL){
			current->addDependency(dep);
		} else {
#ifdef ENABLE_OPENMP
#pragma omp critical
			{
#endif
				printWarning("Dependency \""+*depName+"\" not found !!!");
#ifdef ENABLE_OPENMP
			}
#endif
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
	std::string build_dir = *config->system().getBuildDirectory(); 
	OutputFormatJSON out(config);
	Json::Value graph;
	std::string final, file;
	time_t tmp;
	
	/***** PRE ACTIONS *****/
	time(&tmp);

	/* Save the config in backup file */
	file = build_dir + CHECKPOINT_FILENAME;
	FileManager backup(&file, OUTPUT_ACCESS);
	backup.open();
	if(!backup.isOpen()){
		printError("Cannot open " << backup.toString() << " !", JE_NFND_FIL);
	}
	out.clear();
	out.appendConfig();
	out.appendMisc(nbJobs, nbLists, nbRemainingJobs + extrasJobs->size());
	out.appendSummary(sumRun, sumRun.elapsed+difftime(tmp, sumRun.startRun));
	
	for(size_t a = 0; a < nbLists; a++){
		for(std::list<Job*>::iterator it = jobsList[a].begin(); it != jobsList[a].end(); it++){
			out.appendPendingJob(*it);
		}
	}
	for(std::list<Job*>::const_iterator it = extrasJobs->begin(); it != extrasJobs->end(); it++){
			out.appendPendingJob(*it);
	}

	for(std::list<Job*>::iterator it = executedJobsList.begin(); it != executedJobsList.end(); it++){
		out.appendPendingJob(*it, "jobsExecutedList");
	}
	
	backup << out.stringify();
	backup.close();

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
			graph["configSystem"]["maxbytes"].asUInt(),
			graph["configSystem"]["online"].asBool(),
			graph["configSystem"]["refresh"].asFloat()
			);

	config->job().setJobConfiguration(
			(verboseMode)graph["configJob"]["verbosity"].asInt(),
			(logIntoFileMode)graph["configJob"]["logging"].asInt(),
			graph["configJob"]["fake"].asBool(),
			graph["configJob"]["longNames"].asBool(),
			graph["configJob"]["maxJobTime"].asUInt(),
			graph["configJob"]["maxJobs"].asUInt());

	
	Json::Value fileList = graph["configJob"]["jobsList"];
	for(unsigned int index = 0; index < fileList.size(); index++)
	{
		config->job().getJobsFilesNoLock().push_back(new FileManager(new string(fileList[index].asString()), INPUT_ACCESS));
	}
	
	fileList = graph["configJob"]["whitelist"];
	for(unsigned int index = 0; index < fileList.size(); index++)
	{
		config->job().getWhiteFilesNoLock().push_back(new FileManager(new string(fileList[index].asString()), INPUT_ACCESS));
	}
	
	fileList = graph["configJob"]["blacklist"];
	for(unsigned int index = 0; index < fileList.size(); index++)
	{
		config->job().getBlackFilesNoLock().push_back(new FileManager(new string(fileList[index].asString()), INPUT_ACCESS));
	}

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
						jobsGraphList[index]["group"].asString(),
						jobsGraphList[index]["command"].asString(),
						depVec,
						consVec,
						jobsGraphList[index]["file"].asString(),
						jobsGraphList[index]["extras"].asString(),
						jobsGraphList[index]["postCommand"].asString(),
						jobsGraphList[index]["nbResources"].asUInt(),
						jobsGraphList[index]["returncode"].asInt(),
						jobsGraphList[index]["time"].asFloat(),
						jobsGraphList[index]["delta"].asFloat()
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
					jobsGraphList[index]["group"].asString(),
					jobsGraphList[index]["command"].asString(),
					depVec,
					consVec,
					jobsGraphList[index]["file"].asString(),
					jobsGraphList[index]["extras"].asString(),
					jobsGraphList[index]["postCommand"].asString(),
					jobsGraphList[index]["nbResources"].asUInt(),
					jobsGraphList[index]["returncode"].asInt(),
					jobsGraphList[index]["time"].asFloat(),
					jobsGraphList[index]["delta"].asFloat()
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
	sumRun.nbDisabled = graph["summary"]["nbDisabled"].asUInt();
	sumRun.nbSkipped = graph["summary"]["nbSkipped"].asUInt();
	sumRun.nbTotalSlaves = graph["summary"]["nbTotalSlaves"].asUInt();
	sumRun.elapsed = (double)graph["summary"]["elapsed"].asFloat();

	time(&(sumRun.startRun));
	fd.close();
	resolveJobsDeps();
}

void JobManager::displaySummary() const {
	double elapsed = difftime(sumRun.endRun, sumRun.startRun);
	cout <<  COLOR_NRUN"+------------------------- RUN SUMMARY ------------------------+" COLOR_NORM << endl;
	cout
		<<  COLOR_NRUN "|" COLOR_NRUN" * Executed jobs  : " << sumRun.nbJobs << endl
		<<  COLOR_NRUN "|" COLOR_FAIL"    --> Fails     : " << sumRun.nbFailed << endl
		<<  COLOR_NRUN "|" COLOR_FAIL"    --> Errors    : " << sumRun.nbErrors << endl
		<<  COLOR_NRUN "|" COLOR_URUN"    --> Skips     : " << sumRun.nbSkipped << endl
		<<  COLOR_NRUN "|" COLOR_PASS"    --> Successes : " << sumRun.nbSuccess << endl
		<<  COLOR_NRUN "|" COLOR_DEBG"    --> Disabled  : " << sumRun.nbDisabled << endl
		<<  COLOR_NRUN "|" COLOR_NRUN" * Elapsed time   : " << convertDate(elapsed+sumRun.elapsed) << endl
		<<  COLOR_NRUN "|" COLOR_NRUN" * Slave launchs  : " << sumRun.nbTotalSlaves << endl
		//<< "" << << endl
		;
	cout <<  COLOR_NRUN"+--------------------------------------------------------------+" COLOR_NORM << endl;
}

void JobManager::updateFinalState() const
{
	if(sumRun.nbErrors) config->setReturnStatus(RETURN_WITH_ERRORS);
	if(sumRun.nbFailed) config->setReturnStatus(RETURN_WITH_FAILURES);
	if(sumRun.nbSkipped) config->setReturnStatus(RETURN_WITH_SKIPPED);
}
