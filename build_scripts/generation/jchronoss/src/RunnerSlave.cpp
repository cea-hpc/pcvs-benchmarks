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


#include "RunnerSlave.h"
using namespace std;

RunnerSlave::~RunnerSlave(){
	CHECK(outputFile->close());
#ifdef ENABLE_TRACE
	CHECK(traceFile->close());
#endif
	if(nextWorker != NULL) nextWorker->unsetIOFiles();
	pthread_mutex_destroy(&waitOnBusySocket);
	pthread_mutex_destroy(&waitOnEmptySocket);
}

void RunnerSlave::fillWorker ( Worker * cur, size_t nbResources ) {
	int id = 0;
	Job * job = NULL;
	assert(cur != NULL);
	Runner::fillWorker ( cur, nbResources);
	
	id = jobMan->getHigherValidList(nbResources);
	assert(id >= 0);
	job = jobMan->jobsLists()[id].front();
	cur->add(job);
	cur->setNbRequiredResources(jobMan->jobsLists()[id].front()->getNbResources());
	jobMan->jobsLists()[id].pop_front();
	jobMan->addExecutedJob(job);
}

void RunnerSlave::launchWorker ( Worker* cur ) {
	pthread_t thread;
	int err;
	ThreadItem* item = new ThreadItem;
	
	cur->setIOFiles(NULL, outputFile, traceFile);
	
	item->worker = cur;
	item->slave = this;
	Runner::launchWorker ( cur );

	if(pthread_create(&thread, NULL, launchWorkerThread, (void*)item))
		pthread_exit((void*)&err);
}

void RunnerSlave::postActions(Worker* cur) {
	Runner::postActions(cur);
	cur->pushJobsOutputFile();
}

void RunnerSlave::preActions() {
	Runner::preActions();
}

Worker* RunnerSlave::waitNextWorker() {
	Worker *cur = NULL;

	pthread_mutex_lock(&waitOnEmptySocket);
	//push next worker to dump
	cur = nextWorker;
	//unlock master to read the next worker
	pthread_mutex_unlock(&waitOnBusySocket);
		
	assert(cur != NULL);
	return cur;
}

RunnerSlave::RunnerSlave(JobManager* jobMan, Configuration* config): Runner(jobMan, config)
{
	std::string out = config->job().getJobsFiles().front()->toString() + ".out", trace;
	ostringstream flux;
	struct sigaction sig1, sig2;
	
	sig1.sa_flags=SA_SIGINFO;
	sig1.sa_sigaction=&RunnerSlave::handlerSignal;
	sigemptyset(&sig1.sa_mask);
	sigaddset(&sig1.sa_mask, SIGINT);
	if(sigaction(SIGINT, &sig1, NULL) < 0) printWarning("Unable to Handle SIGINT signal\n");
	sig2.sa_flags=SA_SIGINFO;
	sig2.sa_sigaction=&RunnerSlave::handlerSignal;
	sigemptyset(&sig2.sa_mask);
	sigaddset(&sig2.sa_mask, SIGALRM);
	if(sigaction(SIGALRM, &sig2, NULL) < 0) printWarning("Unable to Handle SIGALARM signal\n");	
	
	outputFile = new FileManager(&out, OUTPUT_ACCESS_BIN);
	traceFile = NULL;
	CHECK(outputFile->open());
	pthread_mutex_init(&waitOnBusySocket, NULL);
	pthread_mutex_init(&waitOnEmptySocket, NULL);
	nextWorker = NULL;
	pthread_mutex_lock(&waitOnEmptySocket);
	
	srand(time(NULL) * getpid());
	
#ifdef ENABLE_TRACE
	trace = (*config->system().getOutputDirectory() != "") ? *config->system().getOutputDirectory()+ "/" +config->job().getJobsFiles().front()->getName(): config->job().getJobsFiles().front()->toString();
	trace += ".trace";
	traceFile = new FileManager(&trace, OUTPUT_ACCESS);
	CHECK(traceFile->open());
	flux << "#nbJobs:" << config->system().getNbMaxResources() << ":" << fixed << setprecision(2) << getCurrentDate() << endl;
	(*traceFile) << flux.str();
#endif

}

void* RunnerSlave::launchWorkerThread(void* arg)
{
	ThreadItem* item = (ThreadItem*)arg;
	pthread_detach(pthread_self());
	//Start the launch for the job
	for(list<Job*>::iterator it = item->worker->begin(); it != item->worker->end(); it++)
		item->slave->launchWorkerThreadStart(*it);
	
	//waiting for master availability
	pthread_mutex_lock(&item->slave->waitOnBusySocket);
	//push next worker to dump
	item->slave->nextWorker = item->worker;
	//unlock master to read the next worker
	pthread_mutex_unlock(&item->slave->waitOnEmptySocket);
	safeFree(item);	
	return NULL;
}

void RunnerSlave::launchWorkerThreadStart(Job* job)
{
	int temp = -1, finalRC, rnd;
	FILE* fd;
	DataFlow flow(config->system().getFlowMaxBytes());
	double start = 0.0, end = 0.0;
	struct timespec ts;
	string instruction, curName;
	if(config->job().isFake()){
		start = getCurrentDate();
		rnd = ((rand()%(MAX_RANDOM-MIN_RANDOM)+MIN_RANDOM))*VAR_COEFF;
		ts.tv_sec = (int)rnd/NB_MS_IN_SEC;
		ts.tv_nsec = ((long)rnd-(ts.tv_sec*NB_MS_IN_SEC))*NB_NS_IN_MS;
		nanosleep(&ts, NULL);
		end = getCurrentDate();
		finalRC = job->getExpectedReturn();
		flow.fillSpecificMessage("Fake execution");
	}
	else{
		instruction = "(" + job->getCommand() + ") 2>&1";
		start = getCurrentDate();
		// read + close on exec flag
		fd = popen(instruction.c_str(), "re");
		flow.fillFrom(fd);	
		temp = pclose(fd);	
		end = getCurrentDate();
		finalRC = (WIFEXITED(temp)) ? WEXITSTATUS(temp): -1 ;
	}
	
	job->addResult(finalRC, end - start, start , flow.getContent());
	
	if(config->job().wantLongNames())
		curName = job->getName();
	else curName = job->getShortName();
	
	if (finalRC == job->getExpectedReturn())
		printLine(PREFIX_PASS, "%6d/%-6d (%6d) %8.2f   %s\n",job->getId(), jobMan->getNbJobs(), jobMan->getNbRemainingJobs(), job->getResult().getTime(), curName.c_str());
	else 
		printLine(PREFIX_FAIL, "%6d/%-6d (%6d) %8.2f   %s\n",job->getId(), jobMan->getNbJobs(), jobMan->getNbRemainingJobs(), job->getResult().getTime(), curName.c_str());
	
	if((config->job().getVerbosity() == VERBOSE_ONLY_ERROR &&  finalRC != job->getExpectedReturn()) || config->job().getVerbosity() >= VERBOSE_ALL){
		cout << "COMMAND = " << job->getCommand() << endl;
		cout << flow.getContent() << endl;
	}
}

void RunnerSlave::handlerSignal(int sig, siginfo_t * siginfo, void *context)
{
	assert(siginfo != NULL);
	assert(context != NULL);
	UNUSED(sig);
	UNUSED(siginfo);
	UNUSED(context);
	//notify the master
	exit(130);
}
