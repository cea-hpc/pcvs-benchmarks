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


#include "RunnerMaster.h"
using namespace std;

static HashTable *globalHashTable;
static JobManager*globalJobManager;

RunnerMaster::RunnerMaster() {}

RunnerMaster::RunnerMaster(JobManager* jobMan, Configuration* config) : Runner(jobMan, config), firstRound(true)
{
	struct sigaction sig1, sig2;

	time(&(jobMan->sumRun.startRun));
	globalJobManager = jobMan;
	globalHashTable = &tabWorkers;
	
	sig1.sa_flags=SA_SIGINFO;
	sig1.sa_sigaction=&RunnerMaster::handlerSignal;
	sigemptyset(&sig1.sa_mask);
	sigaddset(&sig1.sa_mask, SIGINT);
	if(sigaction(SIGINT, &sig1, NULL) < 0) printWarning("Unable to Handle SIGINT signal\n");
	sig2.sa_flags=SA_SIGINFO;
	sig2.sa_sigaction=&RunnerMaster::handlerSignal;
	sigemptyset(&sig2.sa_mask);
	sigaddset(&sig2.sa_mask, SIGALRM);
	if(sigaction(SIGALRM, &sig2, NULL) < 0) printWarning("Unable to Handle SIGALARM signal\n");
	
	printHeader();
}

RunnerMaster::~RunnerMaster(){
	time(&(jobMan->sumRun.endRun));
	jobMan->displaySummary();
}

void RunnerMaster::fillWorker ( Worker* cur, size_t nbResources) {
	
	Runner::fillWorker ( cur , nbResources);
	size_t i = jobMan->getHigherValidList(nbResources);
	TIME_MEASURE_INIT();
	policy->fillingAlgo(cur, i, nbResources);
	TIME_MEASURE_END("Filling algorithm");
}
void RunnerMaster::launchWorker ( Worker* cur ) {
	char * tab_args[NB_PARAMETERS_COMMAND+1];
	ostringstream uniqFlux;
	string in, out;
	pid_t desc;
	int cpt = 0;
	
	jobMan->update((-1)*(int)cur->size());

	Runner::launchWorker ( cur );	
	uniqFlux << *config->system().getBuildDirectory() << "/testFile-" << setw(5) << setfill('0') << Worker::numWorker << ".in"; 
	in = uniqFlux.str();
	out = uniqFlux.str() + ".out";
	cur->setIOFiles(new FileManager(&in, OUTPUT_ACCESS),new FileManager(&out, INPUT_ACCESS_BIN));
	cur->pushJobsInputFile(jobMan->getNbRemainingJobs());
	jobMan->sumRun.nbTotalSlaves++;
	//then, fork() + add an entry in hash tab
	desc = fork();
	if(desc == 0){
		for(unsigned int i=0; i<=NB_PARAMETERS_COMMAND; i++) tab_args[i] = NULL;
		// constructing launcher script to call before slave
		if(firstRound && config->system().getCompilationJobsLauncherCommand() != NULL) {// if compilation
			tab_args[cpt++] =  const_cast<char*>(config->system().getCompilationJobsLauncherCommand()->c_str());
		}
		else if(!firstRound && config->system().getJobsLauncherCommand() != NULL) { // for jobs
			tab_args[cpt++] = const_cast<char*>(config->system().getJobsLauncherCommand()->c_str());
		}
		// main minimal options to restart a slave
		tab_args[cpt++] = const_cast<char*>(config->getExeName().c_str());
		tab_args[cpt++] = (char*)"--slave";
		tab_args[cpt++] = const_cast<char*>(in.c_str());
		
		uniqFlux.str(""); uniqFlux << "--autokill=" << config->system().getMaxSlaveTime();
		tab_args[cpt++] = const_cast<char*>((new string(uniqFlux.str()))->c_str());
		
		uniqFlux.str(""); uniqFlux << "--nb-resources=" << cur->getNbResources(); 
		tab_args[cpt++] = const_cast<char*>((new string(uniqFlux.str()))->c_str());
		
		uniqFlux.str(""); uniqFlux << "--verbosity=" << config->job().getVerbosity();
		tab_args[cpt++] = const_cast<char*>((new string(uniqFlux.str()))->c_str());
		
		uniqFlux.str(""); uniqFlux << "--size-flow=" << config->system().getFlowMaxBytes();
		tab_args[cpt++] = const_cast<char*>((new string(uniqFlux.str()))->c_str());
		
		if(*config->system().getOutputDirectory() != "")
			tab_args[cpt++] = const_cast<char*>((new string("--output="+*config->system().getOutputDirectory()))->c_str()); 

		if(config->job().wantLongNames())
			tab_args[cpt++] = (char*)"--long-names"; 

		// conditional options
		if(config->job().isFake())
			tab_args[cpt++] = (char*)"--fake";
// 		if(config->job().getMaxTestTime() != JobConfiguration::DEFAULT_MAX_TEST_TIME)
		execvp(tab_args[0], tab_args);
		printError("A worker couln't be started ! ABORT\n", JE_EXEC_WRK);
	}
	else{
		tabWorkers.add(desc, cur);
	}	
}
void RunnerMaster::postActions(Worker* cur) {
	Runner::postActions(cur);
	TIME_MEASURE_INIT();
	for(list<Job*>::iterator it = cur->begin(); it != cur->end();it++){
		if((*it)->getStatus() == NOT_RUN) {
			jobMan->update(1);
			if(!(*it)->addATry()){
				(*it)->updateStatus(MUCH_TRIES);
				jobMan->addExecutedJob(*it);
			} else {
				jobMan->addJob(*it);
			}
		}
		else{
			jobMan->addExecutedJob(*it);
		}
	}
	TIME_MEASURE_END("PostActions Master");
	cur->unsetIOFiles();
}
void RunnerMaster::preActions() {
	Runner::preActions();
	jobMan->cleanInvalidJobs();
}
Worker* RunnerMaster::waitNextWorker() {
	pid_t pid;
	Worker* cur = NULL;
	int ret;

	TIME_MEASURE_INIT();
	createBackup();	
	TIME_MEASURE_END("Backup");

	pid = wait(&ret);
	
	cur = tabWorkers.remove(pid);
	cur->pullJobsOutputFile();
	if(WIFEXITED(ret) && WEXITSTATUS(ret) == SIGINT)
		handlerSignal(SIGINT, NULL, NULL);

	return cur;
}
void RunnerMaster::pushJobsIntoFiles()
{
	TIME_MEASURE_INIT();
	jobMan->pushJobsIntoFiles();
	TIME_MEASURE_END("Results pushing");
}

void RunnerMaster::pullJobsFromFiles()
{
    Runner::pullJobsFromFiles();
}

void RunnerMaster::handlerSignal(int sig, siginfo_t * siginfo, void *context)
{
	UNUSED(siginfo);
	UNUSED(context);
	
	if(sig == SIGALRM)
		printInfo("* Interrupted by Autokill");
	else if(sig == SIGINT)
		printInfo("* Interrupted by User");
	else
		printInfo("* Interrupted by SIG " << sig);
	printInfo("* Interrupt child processes...");
	globalHashTable->stopAll();
	printInfo("[ENDED]");
	exit(sig);
}

void RunnerMaster::startCompilation()
{
	Worker cur;
	std::list<Job*>* currentJM = jobMan->jobsLists();
	int i = jobMan->getNbLists() - 1;

	if(jobMan->isEmpty()) return;
	Runner::startCompilation();
	
	do{
		for(list<Job*>::iterator it = currentJM[i].begin(); it != currentJM[i].end();){
			assert((*it)->getStatus() == NOT_RUN);
			if( (*it)->isDepInvalid()){
				it++;
				continue;
			}
			if((*it)->isValid(JobConstraint::COMPILATION_TAG)){
				cur.add(*it);
				it = currentJM[i].erase(it);
			}
			else{
				it++;
			}
		}
	i--;
	}while (i >=0);
	if(cur.size() > 0){
		cur.setNbRequiredResources(config->system().getNbMaxResources());
		launchWorker(&cur);
		waitNextWorker();
		postActions(&cur);
	}
	firstRound = false;
}

void RunnerMaster::createBackup() {
	jobMan->createBackup(globalHashTable->getRunningJobs());
}
