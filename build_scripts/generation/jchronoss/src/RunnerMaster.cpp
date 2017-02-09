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

#include <netinet/in.h>
#include <arpa/inet.h>
#include <sys/socket.h>
#include <netdb.h>
#include "RunnerMaster.h"
#include "OutputFormatJSON.h"
#include "network.h"
using namespace std;

/** global hash table to retrieve data in signal handlers */
static HashTable *globalHashTable;
/** pointer to the jobManager, to retrieve data in signal handlers */
static JobManager*globalJobManager;
#ifdef ENABLE_PLUGIN_SERVER
/** temporary socket from the master instance (server startup) */
static int sock = -1;
/** server socket once startup is done and the server attempt to conect */
static int log_sock = -1;
/** logger socket, to push not scheduler jobs to the remote server */
int backend_sock = -1;
pid_t RunnerMaster::serverPid = -1;
#endif
RunnerMaster::RunnerMaster() {}

RunnerMaster::RunnerMaster(JobManager* jobMan, Configuration* config) : Runner(jobMan, config), firstRound(true)
{
	struct sigaction sig1, sig2, sig3;

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
	sig3.sa_flags=SA_SIGINFO;
	sig3.sa_sigaction=&RunnerMaster::handlerSignal;
	sigemptyset(&sig3.sa_mask);
	sigaddset(&sig3.sa_mask, SIGUSR1);
	if(sigaction(SIGUSR1, &sig3, NULL) < 0) printWarning("Unable to Handle SIGUSR1 signal\n");

	/* First, create a new group and set the current process as the leader */
	//setpgid(0,0);
#ifdef ENABLE_PLUGIN_SERVER
	if(config->system().needOnlineMode())
	{
		startServer();
		startLogger();
	}
#endif

	config->printConfiguration();
	printHeader();
}

#ifdef ENABLE_PLUGIN_SERVER
void RunnerMaster::startLogger()
{
	struct hostent * host;
	struct sockaddr_in addr;

	host = gethostbyname(config->system().getServerName()->c_str());
	if(host == NULL){
		printError("Hostname " << *config->system().getServerName() << " cannot be translated to IP address", SIGINT);
	}
	
	backend_sock = socket(AF_INET, SOCK_STREAM, 0);
	memset(&addr, 0, sizeof(sockaddr_in));
	addr.sin_family = AF_INET;
	addr.sin_port = htons(config->system().getBackendPort());
	addr.sin_addr = * (struct in_addr*)host->h_addr;
	
	if(connect(backend_sock, (struct sockaddr*)&addr, sizeof(struct sockaddr)) != 0){
		printWarning("Master disabling Logging: Unable to connect to " << *config->system().getServerName() << ":" << config->system().getBackendPort()) ;
		backend_sock = -1;
	}
}

void RunnerMaster::startServer(){

	const short SIZE = 1024;
	char * tabArgs[5], hostname[SIZE];
	stringstream flux;
	int cpt = 0, port = RANDOM_PORT_NUMBER, fport = RANDOM_PORT_NUMBER;	
	struct sockaddr_in addr, cli_addr;
	socklen_t sockSize = sizeof(struct sockaddr_in);	
	
	addr.sin_family=AF_INET;
	addr.sin_port=htons(RANDOM_PORT_NUMBER);
	addr.sin_addr.s_addr=htonl(INADDR_ANY);

	/* create the socket to get data back from server */
	if((sock = socket(AF_INET,SOCK_STREAM, 0)) == -1)
	{
		printError("Unable to create a new socket.", JE_UNKNOWN);
	}
	if(bind(sock, (struct sockaddr*)&addr, sizeof(struct sockaddr)) != 0)
	{
		printError("Unable to attach Master socket before launching the log server.", JE_UNKNOWN);
	}
	if(listen(sock, 5) < 0)
	{
		printError("Unable to make the master socket listening, before launching the log server.", JE_UNKNOWN);
	}

	if(gethostname(hostname, SIZE) != 0){
		perror("gethostname()");
		exit(1);
	}
	if (getsockname(sock, (struct sockaddr *)&addr, &sockSize) == -1)
	{
		printError("Unable to retrieve port number from the Master side !", JE_UNKNOWN);
	}

	flux << ntohs(addr.sin_port) ;
	if(config->system().getServerLauncherCommand() != NULL){
		tabArgs[cpt++] =  const_cast<char*>((new string(*config->system().getServerLauncherCommand()))->c_str());
	}

	/* to execute the server, we need to forward him the hostname and port where the master socket is listening */
	tabArgs[cpt++] = const_cast<char*>((new string(config->getExeName()+"_online_server"))->c_str());
	tabArgs[cpt++] = hostname;
	tabArgs[cpt++] = const_cast<char*>((new string(flux.str()))->c_str());
	tabArgs[cpt++] = NULL;

	//printInfo("Listening socket at " << hostname << ":" << addr.sin_port);
	serverPid = fork();
	if(serverPid == 0){
		execvp(tabArgs[0], tabArgs);
		printWarning("Server cannot be launched ! Disabling Online Mode");
		//config->system().disableOnlineMode();
		exit(1);
	} else {
		bzero(hostname, SIZE);
		port = 100;
		size_t hostname_len;
		log_sock = accept(sock, (struct sockaddr*)&cli_addr, &sockSize);
		
		safe_recv(log_sock,(void*)&hostname_len, sizeof(size_t));
		
		assert(hostname_len <= SIZE);
		safe_recv(log_sock,(void*)hostname, hostname_len);
		safe_recv(log_sock,(void*)&port, sizeof(int));
		safe_recv(log_sock,(void*)&fport, sizeof(int));
		
		/* send the configuration object */
		OutputFormatJSON out(config);
		out.appendConfig();

		std::string s = out.stringify();
		const char * output = s.c_str();
		size_t output_size = s.size() * sizeof(char);
		
		safe_send(log_sock, (void*)&output_size, sizeof(size_t) );
		safe_send(log_sock, (void*)output, output_size) ;
		//close(log_sock);
		//close(sock);
		config->system().setServerName(hostname);
	        config->system().setBackendPort(port);
		config->system().setFrontendPort(fport);
	}
}
#endif

RunnerMaster::~RunnerMaster(){
	time(&(jobMan->sumRun.endRun));
	jobMan->displaySummary();
	#ifdef ENABLE_PLUGIN_SERVER
	if(sock > 0) { close(sock); sock = -1;}
	if(log_sock > 0) { close(log_sock); log_sock = -1;}
	if(backend_sock > 0) { close(backend_sock); backend_sock = -1;}
	if(config->system().needOnlineMode() && serverPid != -1)
	{
		kill(serverPid, SIGUSR1);
	}
	#endif
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
		tab_args[cpt++] = const_cast<char*>((new string(config->getExeName()))->c_str());
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
	
		
		if(config->system().needOnlineMode()){
			uniqFlux.str(""); uniqFlux << "--server-name=" << *config->system().getServerName();
			tab_args[cpt++] = const_cast<char*>((new string(uniqFlux.str()))->c_str());

			uniqFlux.str(""); uniqFlux << "--server-port=" << config->system().getBackendPort();
			tab_args[cpt++] = const_cast<char*>((new string(uniqFlux.str()))->c_str());
			
			tab_args[cpt++] = (char*)"--online";
		}
        
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

	pid = waitpid(0, &ret, 0);
#ifdef ENABLE_PLUGIN_SERVER
	if(pid == serverPid)
	{
		printWarning("Abnormal Log Server Termination !");
		serverPid = -1;
		config->system().disableOnlineMode();
		/* start to parse aborted slave to prepare a restart */
		pid = waitpid(0, &ret, 0);
	}
#endif
	/* The slave or wrapper have to terminate normally */
	assert(WIFEXITED(ret));

	cur = tabWorkers.remove(pid);
	cur->pullJobsOutputFile();
	if(WEXITSTATUS(ret) == SIGINT)
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
	{
		printInfo("* Interrupted by Autokill ("<<sig<<")");
	}
	else if(sig == SIGUSR1)
	{
		/* nothing special to do here...*/
		return;
	}
	else
	{
		printInfo("* Interrupted by signal " << sig << " ("<< strsignal(sig) <<")");
	}


	printInfo("* Interrupt child processes...");
	//killpg(getpgid(0), SIGUSR1);
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
