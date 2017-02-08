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

#include "ConfigurationSystem.h"
using namespace std;

//defining const public static member class
const std::string* SystemConfiguration::DEFAULT_OUTPUT_DIRECTORY = new string("");
const std::string* SystemConfiguration::DEFAULT_BUILD_DIRECTORY = new string(".");

std::string temp[] = {"JUNIT", "JSON", "YAML"};
const std::vector<std::string>  SystemConfiguration::AVAIL_OUTPUT_FORMATS (temp, temp +3);
const float SystemConfiguration::DEFAULT_ONLINE_INTERVAL = 1.0;

SystemConfiguration::SystemConfiguration() {
	//setting default values
	nbMaxSlaves = DEFAULT_SLAVES_VALUE;
	maxAutokillTime = DEFAULT_AUTOKILL;
	jobsLauncherCommand = NULL;
	compilationJobsLauncherCommand = NULL;
	serverLauncherCommand = NULL;
	outputDirectory = const_cast<string*>(DEFAULT_OUTPUT_DIRECTORY);
	buildDirectory = const_cast<string*>(DEFAULT_BUILD_DIRECTORY);
	nbMaxResources = DEFAULT_MAX_RESOURCES;
	maxSlaveTime = DEFAULT_MAX_SLAVE_TIME;
	minSlaveTime = DEFAULT_MIN_SLAVE_TIME;
	policySelected = DEFAULT_POLICY_SELECTED;
	// set to 0 by default. Dataflow will set the flow to DEFAULT_BUFFER_MAX_FLOW if current flow is <= 0
	// So don't be chocked if you see --flow-size=0 in paramater; this means default size (10 Kio)
	// Maybe this behaviour should be avoid, a default value refactoring is required
	flowMaxBytes = DEFAULT_FLOW_MAX_BYTES;
	outputFormats = std::vector<std::string>();
	outputFormats.push_back(AVAIL_OUTPUT_FORMATS[0]);

	servName = NULL;
	frontendPort = RANDOM_PORT_NUMBER;
	backendPort = RANDOM_PORT_NUMBER;
	online_mode = DEFAULT_ONLINE_MODE;
	interval=DEFAULT_ONLINE_INTERVAL;
	
	//post-actions
	assert(outputDirectory != NULL);
	assert(buildDirectory != NULL);
}

SystemConfiguration::~SystemConfiguration() {
	safeFree(outputDirectory);
	safeFree(buildDirectory);
}

void SystemConfiguration::printHelp() const {
	//print help
	cout 	<< COLOR_URUN "\n SYSTEM OPTIONS :" COLOR_NORM "\n"
		<< PADDED_HELP << "   -o, --output=<path> "		<<"Specifies output directory" << endl
		<< PADDED_HELP << "   -B, --build=<path> "		<<"Specifies build directory (storing temporary files)" << endl
		<< PADDED_HELP << "   -r, --nb-resources=<number> "	<<"Determines number of resources used for the validation" << endl
		<< PADDED_HELP << "   -j, --nb-slaves=<number> "	<<"Max slaves allowed to run simultaneously" << endl
		<< PADDED_HELP << "   -T, --maxt-slave=<number> "	<<"Max seconds allowed to a slave for running."<<endl
		<< PADDED_HELP << "   -m, --mint-slave=<number> "	<< "Min seconds need to reach to start a slave" << endl
		<< PADDED_HELP << "   -L, --launcher=<file> "		<<"Launcher used to launch jobs workers" << endl
		<< PADDED_HELP << "   -C, --compil-launcher=<file> "	<<"Launcher used to launch specific compilation jobs (first worker)" << endl
		<< PADDED_HELP << "   -W, --server-launcher=<file> "	<<"Launcher used to launch log server" << endl
		<< PADDED_HELP << "   -p, --policy=<number> "		<<"Choose policy : (0: default, 1: by resources, 2: by time, 3: custom)" << endl
		<< PADDED_HELP << "   -D, --size-flow=<number> "	<<"Choose max number of bytes to keep in logs" << endl
		<< PADDED_HELP << "   -k, --autokill<number> " 		<< "Apply a timeout to the current application" <<endl
		<< PADDED_HELP << "   -O, --output-format=<list> "	<< "Choose the type of formatting for result publishing" <<endl
		<< PADDED_HELP << "   -e, --online " 		<< "Run server for real-time results" <<endl
		<< PADDED_HELP << "   -x, --server-name=<host> "	<<"Log server addresss if external is used" << endl
		<< PADDED_HELP << "   -P, --server-port=<int> "	<<"Log server port if external is used" << endl
		<< PADDED_HELP << "   -i, --interval=<int> " << "Polling interval for the slave result agregation" <<endl
// 		<< "" << endl
	;
}

void SystemConfiguration::printConfiguration() const
{
	if(jobsLauncherCommand != NULL)            cout << PADDED_OPT << "| - Launcher script : " << *jobsLauncherCommand << endl;
	if(compilationJobsLauncherCommand != NULL) cout << PADDED_OPT << "| - Compilation launcher script : " << *compilationJobsLauncherCommand << endl;
	if(serverLauncherCommand != NULL) cout << PADDED_OPT << "| - Server launcher script : " << *serverLauncherCommand << endl;
	if(*outputDirectory != "")                 cout << PADDED_OPT << "| - Output path : " <<"'"<< *outputDirectory << "'"<< endl;
	cout << PADDED_OPT << "| - Build path : " <<"'"<< *buildDirectory << "'" << endl;
	cout << PADDED_OPT << "| - Online Mode : " << boolalpha << online_mode << endl;
	if(online_mode)
	{
		cout << PADDED_OPT << "| - Front-end Address : " << *servName << ":" << frontendPort << endl;
		cout << PADDED_OPT << "| - Back-end Address : " << *servName << ":" << backendPort << endl;
		cout << PADDED_OPT << "| - Polling interval : " << interval << "s" << endl;
	}
	cout << PADDED_OPT << "| - Scheduling policy : ";
	switch(policySelected){
		case POLICY_DEFAULT:
			cout << "DEFAULT" << endl;
			break;
		case POLICY_RES:
			cout << "RESOURCES" << endl;
			break;
		case POLICY_TIME:
			cout << "TIMES" << endl;
			break;
		case POLICY_CUSTOM:
			cout << "CUSTOM" << endl;
			break;
	}
	cout << PADDED_OPT << "| - MAX Resources : " << nbMaxResources << endl;
	cout << PADDED_OPT << "| - MAX Simultaneous Slaves : " << nbMaxSlaves << endl;
	cout << PADDED_OPT << "| - MAX Slave Time : ";
	if(maxSlaveTime == DEFAULT_MAX_SLAVE_TIME) cout << "INFINITE" << endl; else 
		cout << maxSlaveTime << " second(s)" << endl;
	cout << PADDED_OPT << "| - MIN Slave Time : ";
	if(minSlaveTime == DEFAULT_MIN_SLAVE_TIME) cout << "0 second(s)" << endl; else 
		cout << minSlaveTime << " second(s)" << endl;
	
	cout << PADDED_OPT << "| - MAX Autokill : ";
	if(maxAutokillTime == DEFAULT_AUTOKILL) cout << "DISABLED" << endl; else cout << maxAutokillTime << " second(s)" << endl;
	cout << PADDED_OPT << "| - MAX kept bytes : ";
	if(flowMaxBytes == DEFAULT_FLOW_MAX_BYTES) cout << "DEFAULT" << endl; else cout << flowMaxBytes << " byte(s)" << endl;
	cout << PADDED_OPT << "| - Result formats : ";
	for(vector<string>::const_iterator it = outputFormats.begin(); it != outputFormats.end(); it++)
		cout << *it << " ";
	cout << endl;
}


std::string* SystemConfiguration::getCompilationJobsLauncherCommand() const {
	return compilationJobsLauncherCommand;
}
std::string* SystemConfiguration::getJobsLauncherCommand() const {
	return jobsLauncherCommand;
}

std::string* SystemConfiguration::getServerLauncherCommand() const {
	return serverLauncherCommand;
}

size_t SystemConfiguration::getNbMaxResources() const {
	return nbMaxResources;
}
size_t SystemConfiguration::getNbMaxSlaves() const {
	return nbMaxSlaves;
}

bool SystemConfiguration::needOnlineMode() const {
	return online_mode;
}

void SystemConfiguration::disableOnlineMode()
{
	online_mode = false;
}

void SystemConfiguration::enableOnlineMode()
{
	online_mode = true;
}

std::string* SystemConfiguration::getOutputDirectory() const {
	return outputDirectory;	
}
std::string* SystemConfiguration::getBuildDirectory() const {
	return buildDirectory;
}
unsigned int SystemConfiguration::getMaxSlaveTime() const
{
	return maxSlaveTime;
}
bool SystemConfiguration::parseSystemOptions ( int option ) {
	bool value = true;
	switch(option){
		case 'o':
			safeFree(outputDirectory);
			outputDirectory = new string(optarg);
			assert(outputDirectory != NULL);
			break;
		case 'B':
			safeFree(buildDirectory);
			buildDirectory = new string(optarg);
			assert(buildDirectory != NULL);
			break;
		case 'r':
			nbMaxResources = atoi(optarg);
			break;
		case 'j':
			nbMaxSlaves = atoi(optarg);
			break;
		case 'T':
			maxSlaveTime = atoi(optarg);
			break;
		case 'm':
			minSlaveTime = atoi(optarg);
			break;
		case 'k':
			maxAutokillTime = atoi(optarg);
			break;
		case 'L':
			jobsLauncherCommand = new string(optarg);
			assert(jobsLauncherCommand != NULL);
			break;
		case 'C':
			compilationJobsLauncherCommand = new string(optarg);
			assert(compilationJobsLauncherCommand != NULL);
			break;
		case 'W':
			serverLauncherCommand = new string(optarg);
			assert(serverLauncherCommand != NULL);
			break;
		case 'x':
			servName = new string(optarg);
			assert(servName != NULL);
			break;
		case 'P':
			backendPort = atoi(optarg);
			break;
		case 'p':
			policySelected = (policyMode)atoi(optarg);
			break;
		case 'D':
			flowMaxBytes = atoi(optarg);
			break;
		case 'O':
			{
				std::string str(optarg);
				outputFormats.clear();

				for(size_t i = 0; i < str.length(); i++) str[i] = toupper(str[i]);
				for(vector<string>::const_iterator it = AVAIL_OUTPUT_FORMATS.begin(); it != AVAIL_OUTPUT_FORMATS.end(); it++)
				{
					if(str.find(*it) != std::string::npos)
					{
						outputFormats.push_back(*it);
					}
				}
				break;
			}
		case 'e':
			online_mode = true;
			break;
		case 'i':
			interval = atoi(optarg);
			break;
		default:
			value = false;
			break;
	}
	return value;
}

unsigned int SystemConfiguration::getAutokill() const
{
	return maxAutokillTime;
}

std::string* SystemConfiguration::getServerName() const
{
    return servName;
}

void SystemConfiguration::setServerName(const char* s) 
{
	servName = new std::string(s);
}

int SystemConfiguration::getFrontendPort() const
{
	return frontendPort;
}

void SystemConfiguration::setFrontendPort(int port)
{
	frontendPort = port;
}

int SystemConfiguration::getBackendPort() const
{
    return backendPort;
}

void SystemConfiguration::setBackendPort(int port)
{
	backendPort = port;
}

float SystemConfiguration::getOnlineInterval() const
{
	return interval;
}


policyMode SystemConfiguration::getPolicy() const
{
	return policySelected;
}

unsigned int SystemConfiguration::getMinSlaveTime() const
{
	return minSlaveTime;
}

const std::vector<std::string>& SystemConfiguration::getOutputFormats() const
{
	return outputFormats;
}

void SystemConfiguration::loadFile(const XMLConfigParser& parser)
{
	int value;
	//loading, from config file
	parser.loadStr(*jobsLauncherCommand, XMLConfigParser::SYS_CONF, "jobsCommand");
	parser.loadStr(*compilationJobsLauncherCommand, XMLConfigParser::SYS_CONF, "compilationCommand");
	parser.loadStr(*outputDirectory, XMLConfigParser::SYS_CONF, "output");
	parser.loadStr(*buildDirectory, XMLConfigParser::SYS_CONF, "build");
	parser.loadInt(value, XMLConfigParser::SYS_CONF, "maxResources");
	parser.loadInt((int&)maxSlaveTime, XMLConfigParser::SYS_CONF, "maxSlaveTime");
	parser.loadInt((int&)minSlaveTime, XMLConfigParser::SYS_CONF, "minSlaveTime");
	parser.loadBool((bool&)online_mode, XMLConfigParser::SYS_CONF, "online");
	parser.loadFloat((float&)interval, XMLConfigParser::SYS_CONF, "refresh");
	nbMaxResources = (size_t)value;
	parser.loadInt(value, XMLConfigParser::SYS_CONF, "maxSlaves");
	nbMaxSlaves = (size_t)value;
	parser.loadInt((int&)maxAutokillTime, XMLConfigParser::SYS_CONF, "autokill");
	parser.loadInt(value, XMLConfigParser::SYS_CONF, "policy");
	policySelected = (policyMode)value;
	parser.loadInt(value, XMLConfigParser::SYS_CONF, "flowsize");
	flowMaxBytes = (size_t)value;
}

size_t SystemConfiguration::getFlowMaxBytes() const {
	return flowMaxBytes;
}

void SystemConfiguration::setSystemConfiguration(std::string* launcher, std::string* compil, std::string* output, std::string*build, size_t res, size_t slaves, unsigned int autokill, unsigned int minTime, unsigned int maxTime, policyMode policy, size_t flow, bool online, float interval){
	jobsLauncherCommand = launcher;
	compilationJobsLauncherCommand = compil;
	outputDirectory = output;
	buildDirectory = build;
	nbMaxResources = res;
	nbMaxSlaves = slaves;
	maxAutokillTime = autokill;
	maxSlaveTime = maxTime;
	minSlaveTime = minTime;
	policySelected = policy;
	flowMaxBytes = flow;
	online_mode = online;
	interval = interval;
}
