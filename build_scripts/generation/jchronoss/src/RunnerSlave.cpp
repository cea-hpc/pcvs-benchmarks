
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

#include <netinet/in.h>
#include <arpa/inet.h>
#include <sys/socket.h>
#include <netdb.h>
#include "network.h"
#include "RunnerSlave.h"
#include "utils.h"
#include "OutputFormatJSON.h"

using namespace std;

#ifdef ENABLE_PLUGIN_SERVER
pthread_mutex_t serializeExecutedList;
polling_data_t* st;
#endif

RunnerSlave::~RunnerSlave(){
	CHECK(outputFile->close());
#ifdef ENABLE_TRACE
	CHECK(traceFile->close());
#endif
	if(nextWorker != NULL) nextWorker->unsetIOFiles();
	pthread_mutex_destroy(&waitOnBusySocket);
	pthread_mutex_destroy(&waitOnEmptySocket);

#ifdef ENABLE_PLUGIN_SERVER
	pthread_mutex_destroy(&serializeExecutedList);
	if(config->system().needOnlineMode())
	{
		RunnerSlave::timer_callback(st);
		close(serverSock);
	}
#endif
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

#ifdef ENABLE_PLUGIN_SERVER
	pthread_mutex_lock(&serializeExecutedList);
#endif
	for(std::list<Job*>::const_iterator it = cur->begin(); it != cur->end(); it++)
	{
		jobMan->addExecutedJob(*it);
	}
#ifdef ENABLE_PLUGIN_SERVER
	pthread_mutex_unlock(&serializeExecutedList);
#endif
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
	struct sigaction sig1, sig2, sig3;
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
	sig3.sa_flags=SA_SIGINFO;
	sig3.sa_sigaction=&RunnerSlave::handlerSignal;
	sigemptyset(&sig3.sa_mask);
	sigaddset(&sig3.sa_mask, SIGUSR1);
	if(sigaction(SIGUSR1, &sig3, NULL) < 0) printWarning("Unable to Handle SIGUSR1 signal\n");

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

#ifdef ENABLE_PLUGIN_SERVER
	pthread_mutex_init(&serializeExecutedList, NULL);
	if(config->system().needOnlineMode())
		runClient();
#endif
}

#ifdef ENABLE_PLUGIN_SERVER

void * RunnerSlave::polling_handler(void * arg)
{
	polling_data_t* data = (polling_data_t*)arg;
	Configuration* config = data->config;
	double sleep_time = config->system().getOnlineInterval();
	struct timespec timer;

	timer.tv_sec = (int)sleep_time;
	timer.tv_nsec = (double(sleep_time - (double)timer.tv_sec) * (1000 * 1000 * 1000));
	
	while(1)
	{
		timer_callback(data);
		nanosleep(&timer, NULL);

	}

	return NULL;
}

void RunnerSlave::timer_callback(polling_data_t* data)
{
	
	OutputFormatJSON format(data->config);
	std::list<Job*>::const_iterator ccur, cend, cstart;

	pthread_mutex_lock(&serializeExecutedList);
	cend = data->manager->getExecutedEnd();
	if((cstart = data->manager->getExecutedStart()) == cend)
	{
		/* empty list */
		pthread_mutex_unlock(&serializeExecutedList);
		return;
	}

	if(data->cur == cend)
	{
		ccur = data->cur = cstart;
	}
	else
	{
		ccur = data->cur;
		ccur++;
	}

	while(ccur != cend)
	{

		std::string name = (*ccur)->getShortName();
		std::string group = (*ccur)->getName();
		std::string command = (*ccur)->getCommand();
		std::string data = (*ccur)->getResult().getData();
		double time = (*ccur)->getResult().getTime();
		HTMLEncoding(group);
		HTMLEncoding(name);
		HTMLEncoding(command);
		HTMLEncoding(data);
		switch((*ccur)->getStatus())
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

		++(ccur);
	}
	data->cur = --ccur;
	pthread_mutex_unlock(&serializeExecutedList);
	if(!format.isEmpty())
	{
		int ret;
		size_t sz = format.stringify().size() * sizeof(char);
		if((ret = safe_send(data->socket, &sz, sizeof(sz))) <= 0)
		{
			perror("safe_send");
		}
		if((ret = safe_send(data->socket, format.stringify().c_str(), sz )) <= 0)
		{
			perror("safe_send");
		}
	}
}

void RunnerSlave::runClient(){
	struct hostent * host;
	struct sockaddr_in addr;
	int err;
	pthread_t id;

	host = gethostbyname(config->system().getServerName()->c_str());
	if(host == NULL){
		printError("Hostname " << *config->system().getServerName() << " cannot be translated to IP address", SIGINT);
	}

	serverSock = socket(AF_INET, SOCK_STREAM, 0);
	memset(&addr, 0, sizeof(sockaddr_in));
	addr.sin_family = AF_INET;
	addr.sin_port = htons(config->system().getBackendPort());
	addr.sin_addr = * (struct in_addr*)host->h_addr;
	
	if(connect(serverSock, (struct sockaddr*)&addr, sizeof(struct sockaddr)) != 0){
		printWarning("Slave disabling Logging: Unable to connect to " << *config->system().getServerName() << ":" << config->system().getBackendPort()) ;
	}

	st = (polling_data_t * )malloc(sizeof(polling_data_t));
	assert(st);
	st->socket = serverSock;
	st->config = config;
	st->manager = jobMan;
	st->cur = jobMan->getExecutedEnd();
	if(pthread_create(&id, NULL, RunnerSlave::polling_handler, st))
		pthread_exit((void*)&err);
}
#endif

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
	double time = 0.0, start = 0.0;
	struct timespec ts;
	string instruction, curName, time_str, content;
	if(config->job().isFake()){
		rnd = ((rand()%(MAX_RANDOM-MIN_RANDOM)+MIN_RANDOM))*VAR_COEFF;
		ts.tv_sec = (int)rnd/NB_MS_IN_SEC;
		ts.tv_nsec = ((long)rnd-(ts.tv_sec*NB_MS_IN_SEC))*NB_NS_IN_MS;
		start = getCurrentDate();
		nanosleep(&ts, NULL);
		time = ts.tv_sec + (ts.tv_nsec / (double)(NB_MS_IN_SEC * NB_NS_IN_MS));
		finalRC = job->getExpectedReturn();
		flow.fillSpecificMessage("Fake execution");
		content = flow.getContent(NULL);
	}
	else{
		instruction = job->getCommand();
		replace( instruction , "\"", "\\\"");
		instruction = " /usr/bin/time -f%e sh -c \"" + instruction + "\" 2>&1";
		start = getCurrentDate();
		// read + close on exec flag
		fd = popen(instruction.c_str(), "r");
		assert(fd != NULL);

		flow.fillFrom(fd);	
		temp = pclose(fd);
		assert(temp != -1);
		
		content = flow.getContent(&time_str);
		finalRC = (WIFEXITED(temp)) ? WEXITSTATUS(temp): -1 ;
	}

	job->updateStatus((finalRC == job->getExpectedReturn()) ? PASSED : FAILED);
	
	time = atof(time_str.c_str());
	job->addResult(finalRC, time, start , content);

	if(config->job().wantLongNames())
		curName = job->getName();
	else curName = job->getShortName();

	if(job->isPassed(finalRC, time))
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
	UNUSED(sig);
	UNUSED(siginfo);
	UNUSED(context);
	//notify the master
	exit(130);
}
