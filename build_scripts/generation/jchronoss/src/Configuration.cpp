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

#include "Configuration.h"
using namespace std;
/* initialize options parsing structs with getopt
 * - shortopts : struct contains short options tags where ":" means option require argument
 * - longopts : struct contains long options tags with ("name", haveArg?, NOT_USED, short_match)
 */
static char * shortopts = (char*)"ab:c:efhi:j:k:lm:no:p:r:st:vw:x:AB:C:D:FK:L:MNO:P:R:ST:VW:XY:"; // short options
static struct option longopts[]= {                                                // long options
	/* opt			has_arg			flag	val*/
	/**************************** GLOBAL *************************/
	{"help",		no_argument,		NULL,	'h'},
	{"master",		no_argument,		NULL,	'M'},
	{"slave",		no_argument,		NULL,	'S'},
	{"config-file",		required_argument,	NULL,	'c'},
	{"restart",             required_argument,      NULL,   'R'},
	{"expect-success",	no_argument, 		NULL, 	'X'},

	/****************************   JOB  *************************/
	{"silent",		no_argument,		NULL,	's'},
	{"verbose",		no_argument,		NULL,	'v'},
	{"very-verbose",	no_argument,		NULL,	'V'},
	{"keep-none",		no_argument,		NULL,	'N'},
	{"keep-fail",		no_argument,		NULL,	'n'},
	{"keep-success",	no_argument,		NULL,	'a'},
	{"keep-all",		no_argument,		NULL,	'A'},
	{"fake",		no_argument,		NULL,	'f'},
	{"no-fake",		no_argument,		NULL,	'F'},
	{"keep",		required_argument,	NULL,	'K'},
	{"verbosity",		required_argument,	NULL,	'Y'},
	{"maxt-job",		required_argument,	NULL,	't'},
	{"white",		required_argument,	NULL,	'w'},
	{"black",		required_argument,	NULL,	'b'},
	{"long-names",          no_argument,            NULL,   'l'},

	/**************************** SYSTEM *************************/
	{"output",	    	required_argument,	NULL,	'o'},
	{"autokill",		required_argument,	NULL,	'k'},
	{"build",		required_argument,	NULL,	'B'},
	{"nb-resources",	required_argument,	NULL,	'r'},
	{"nb-slaves",		required_argument,	NULL,	'j'},
	{"maxt-slave",		required_argument,	NULL,	'T'},
	{"mint-slave",		required_argument,	NULL,	'm'},
	{"launcher",		required_argument,	NULL,	'L'},
	{"compil-launcher",	required_argument,	NULL,	'C'},
	{"server-launcher",	required_argument,	NULL,	'W'},
	{"server-name",		required_argument,	NULL,	'x'},
	{"server-port", 	required_argument,	NULL,	'P'},
	{"policy",  		required_argument,	NULL,	'p'},
	{"size-flow",		required_argument,	NULL,	'D'},
	{"output-format",	required_argument,	NULL,	'O'},
	{"online",		no_argument,		NULL,	'e'},
	{"interval", 		required_argument,	NULL,	'i'},
	
	/*************************** END TAG *************************/
	{NULL,			no_argument,		NULL,	 0 }
};

Configuration::Configuration() : configStatus(EXEC_MASTER), configFile(NULL), restoredFile(""), finalState(MAX_RETURN_VALUES) {
}

Configuration::~Configuration() {
}

bool Configuration::isMaster() const {
	return configStatus == EXEC_MASTER;
}

bool Configuration::isRestored() const {
	return configStatus == EXEC_RESTORED;
}

void Configuration::printHelp() const {
	//printing banner and help menu for global configuration
	banner();
		
	//display global help
	cout 	<< COLOR_NRUN "         Usage: ./jchronoss [options] <XML Files> " COLOR_NORM "\n"
	
		<< COLOR_FAIL "\n GLOBAL OPTIONS :" COLOR_NORM "\n"
		<< PADDED_HELP << "   -h, --help " 		<< "Print this help.\n"
		<< PADDED_HELP << "   -M, --master "		<<"Launch JChronoss as master (recommended).\n"
		<< PADDED_HELP << "   -S, --slave " 		<<"Launch JChronoss as slave (rarely to use).\n"
		<< PADDED_HELP << "   -c, --config-file=<path> "<< "Specifies a configuration file to load. \n"
		<< PADDED_HELP << "   -R, --restart=<jsonFile> "<< "Restart JChronoss after interruption with saved data. \n"
		<< PADDED_HELP << "   -X, --expect-success "   << "Set Return code depending on validation (0=success, 1=skipped, 2=failure, 3=error)" <<endl
	;
	
	//display other helps
	configSystem.printHelp();
	configJobs.printHelp();
}

returnStatus Configuration::getReturnStatus() const
{
	/* with set the real values only if the two statements are true
	 * - the instante is a Master (Restored mode or node)
	 * - If the current state is not MAX_RETURN_VALUES, set by default if the user wants '0' in any case
	 */
	return ((isMaster() || isRestored()) && finalState != MAX_RETURN_VALUES) ? finalState : RETURN_SUCCESS;
}

void Configuration::setReturnStatus(returnStatus st)
{
	/* always keep the higher raised return code */
	finalState = (st > finalState) ? st : finalState;
}

void Configuration::parseOptions ( int argc, char** argv ) {
	int option, optIndex;
	
	// pre actions
	assert(argc > 0);
	assert(argv != NULL);
	assert(argv[0] != NULL);
	
	// first arg = jchronoss path
	executablePathName = argv[0];
	
	// looking for a --config-file option (pre actions = loading configuration file)
	while((option = getopt_long(argc, argv, shortopts, longopts, &optIndex)) != -1){
		// if current option matches with config-file pattern
		if(option == 'c'){
			configFile = new string(optarg);
			assert(configFile != NULL);
			//init config file loading
			XMLConfigParser parser(*configFile);
			//parse jobs options from config file
			configJobs.loadFile(parser);
			//parse system options from config file
			configSystem.loadFile(parser);
			break;
		}
	}
	
	//re-init options parsing
	optind = 1;
	
	// override set configuration with command line options
	while((option = getopt_long(argc, argv, shortopts, longopts, &optIndex)) != -1){
		switch(option){
			case 'h':
				printHelp();
				exit(0);
				break;
			case 'M':
				configStatus = EXEC_MASTER;
				break;
			case 'S':
				configStatus = EXEC_SLAVE;
				break;
			case 'R':
				configStatus = EXEC_RESTORED;
				restoredFile = string(optarg);
				break;
			case 'X':
				finalState = RETURN_SUCCESS;
				break;
			case '?':
				printError("Some issues about options. See --help for further information !", JE_NKWN_OPT);
				exit(1);
				break;
			default:
				// if options is not considered as global configuration, looking for options in subConfiguration
				if(!configJobs.parseJobOptions(option)) //if not a job option
					configSystem.parseSystemOptions(option);
				break;
		}
	}

	// parsing arguments left which are not options (i.e. xml jobs files)
	if(optind != argc){
		while(optind != argc){
			configJobs.addJobsFiles(argv[optind++]);
		}
	}
	// some validity checks on configuration state
	if(isMaster() || isRestored()){
		banner();
		// checks options
		checkOptionsValidity();
	}
}

JobConfiguration& Configuration::job() {
	return configJobs;
}

SystemConfiguration& Configuration::system() {
	return configSystem;
}

string Configuration::getExeName() const {
	return executablePathName;
}
void Configuration::printConfiguration() const
{	
	//global configuration printing
	if(configFile != NULL){
		cout << COLOR_NRUN "+--------------------- GLOBAL CONFIGURATION -------------------+" COLOR_NORM << endl;
		cout << PADDED_OPT << "| - Configuration file : " << *configFile << endl;
	}
	
	//other printing
	cout << COLOR_NRUN "+--------------------- SYSTEM CONFIGURATION -------------------+" COLOR_NORM << endl;
	configSystem.printConfiguration();
	cout << COLOR_NRUN "+----------------------- JOB CONFIGURATION --------------------+" COLOR_NORM << endl;
	configJobs.printConfiguration();
}

string* Configuration::getConfigFile() const
{
	return configFile;
}

void Configuration::checkOptionsValidity() const {

	//if this session is a restarted one, all configuration given while restart will be
	//squashed by old one, stored in backup json file
	if(isRestored()){		
		if(!FileManager::isCreated(&restoredFile))
			printError("Restored JSON file not found ! (check --restart option)", JE_NFND_FIL);
		printWarning("This is a restarted JCHRONOSS session.");
		printWarning("All current configuration will be lost, replaced by backup !");
		return;
	}
	//if resources number is not defined, unable to guess how to schedule.
	//Maybe this is required only if not default policy and execute sequentially in this case
	if(configSystem.getNbMaxResources() < 1)
		printError("Number of resources not set. Unable to schedule without that parameter !!\n ERROR        : Try to use --nb-resources | -r options or the configuration file.", JE_NSET_RES);
	
	//max slaves is always positive (size_t) so warning if user give < 0
	//if(configSystem.getNbMaxSlaves() == 0)
	//	printError("Number of slaves have to be greater than 0 !\n ERROR        : Try to use --nb-slaves | -j options or the configuration file.", JE_INVA_VAL);
	
	//check intermediate wrappers exist
	if(!FileManager::isCreated(configSystem.getCompilationJobsLauncherCommand()))
		printError("The given Compilation launcher script not found or executable permission is missing", JE_NFND_FIL);
		
	if(!FileManager::isCreated(configSystem.getJobsLauncherCommand()))
		printError("The given launcher script not found or executable permission is missing", JE_NFND_FIL);
	
	if(configSystem.needOnlineMode() && !FileManager::isCreated(configSystem.getServerLauncherCommand()))
		printError("The given Server launcher script not found or executable permission is missing", JE_NFND_FIL);
	//specific check on policy TIME
	if(configSystem.getPolicy() != POLICY_DEFAULT){
		//user should set max time for an allocation, to set upper bound for allocation
		if(configSystem.getMaxSlaveTime() == SystemConfiguration::DEFAULT_MAX_SLAVE_TIME)
			printWarning("You choose a policy where --maxt-slave should be defined. A bound limit should be set (--maxt-slave)");
		// user should set job time, used to compute job bloc size in allocation area.
		if(configJobs.getMaxJobTime() == JobConfiguration::DEFAULT_MAX_JOB_TIME)
			printWarning("Max Time for job not set !! (ignore this if you've specified job time individually)");
	}
	
	//min < max check
	if(configSystem.getMaxSlaveTime() < configSystem.getMinSlaveTime())
		printError("The max time allowed to a slave cannot be lower than the min one.\n ERROR        : See --maxt-slave and --mint-slave options", JE_INVA_VAL);
	
	if(configSystem.getPolicy() < POLICY_DEFAULT || configSystem.getPolicy() > POLICY_CUSTOM){
		printError("Invalid policy. Choose in {0, 1, 2, 3} (see --help)", JE_INVA_VAL);
	}

	if(!(*configSystem.getOutputDirectory() == "") && !FileManager::isCreated(configSystem.getOutputDirectory()))
		printError("Output path doesn't exist !", JE_NFND_FIL);
	if(!FileManager::isCreated(configSystem.getBuildDirectory()))
		printError("Build path doesn't exist !", JE_NFND_FIL);

	if(configSystem.getOutputFormats().size() <= 0)
	{
		printError("You should have at least one valid output format !!!", JE_INVA_VAL);
	}

	bool server_compiled = false;
#ifdef ENABLE_PLUGIN_SERVER
	server_compiled = true;
#endif
	if(!server_compiled && configSystem.needOnlineMode())
	{
		printError("Unable to use Real-Time support (--online) without passing -DENABLE_PLUGIN_SERVER=ON to CMake fonfiguration !", JE_UNKNOWN);
	}

}

std::string Configuration::getRestoredFile() const {
	return restoredFile;
}

void Configuration::setConfiguration(std::string exe, execMode mode, std::string *config){
	executablePathName = exe;
	configStatus = mode;
	configFile = config;
}
