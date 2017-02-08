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

#include "Dispatcher.h"
using namespace std;

Dispatcher::~Dispatcher() {
	//cleaners
	safeFree(orchestra);
	safeFree(resMan);
	safeFree(jobsMan);
}

Dispatcher::Dispatcher ( Configuration* conf){
	assert(conf != NULL);
	config = conf;
	jobsMan = new JobManager(conf);
	assert(jobsMan != NULL);
	resMan = new ResourcesManager;
	assert(resMan != NULL);
}

void Dispatcher::setRunner()
{
	if(config->isMaster()){
		orchestra = new RunnerMaster(jobsMan, config);
	} else if (config->isRestored()){
		jobsMan->loadBackup();
		orchestra = new RunnerMaster(jobsMan, config);
	} else {
		orchestra = new RunnerSlave(jobsMan, config);
	}
	
	assert(orchestra != NULL);
}

void Dispatcher::startScheduling() {

	size_t nbCurrentLaunches = 0;
	Worker* cur = NULL;
	
	//pre actions
	assert(resMan != NULL);
	assert(orchestra != NULL);
	assert(jobsMan != NULL);

	resMan->setNbResources(config->system().getNbMaxResources());
	if(!config->isRestored())
		orchestra->pullJobsFromFiles();
    		
	orchestra->preActions();
	if(!config->isRestored())
		orchestra->startCompilation();
	/* THE IDEA HERE:
	 * Fill the available resources with max workload as we can according to 
	 * number of left jobs and max simultaneous slaves.
	 * When no slaves can be started, JCHRONOSS waits for the first result and apply some post-treatment
	 * Then, we try to reuse resources immediatly.
	 * 
	 * This steps are repeated until no jobs left
	 */
	
	//while some jobs haven't be executed or at least one slave are running
	while(!jobsMan->isEmpty() || nbCurrentLaunches > 0){
		orchestra->preActions();
		//while jobs are left AND (some slaves can be launched AND availables resources
 		while(!jobsMan->isEmpty() && (nbCurrentLaunches < config->system().getNbMaxSlaves() || config->system().getNbMaxSlaves() == SystemConfiguration::DEFAULT_SLAVES_VALUE) && resMan->getNbResources() > 0){

			//get best list to take jobs (list with biggest jobs)
			if(jobsMan->getHigherValidList(resMan->getNbResources()) < 0){
				break;
			}
			
			cur = new Worker;
			assert(cur != NULL);
			// fill the worker
			orchestra->fillWorker(cur, resMan->getNbResources());
			//check if at least one job have been selected
			//if worker is empty, it means that remaining jobs have at least one not-resolved dep (currently running)
			if(cur->size() == 0) break;
			
			resMan->update((int)cur->getNbResources()*(-1));
			nbCurrentLaunches++;
			
			//start the worker
			orchestra->launchWorker(cur);
			
			//detach the worker
			cur = NULL;
		}
		//if some slaves are running
		if(nbCurrentLaunches > 0){
			//wait next slave end
			cur = orchestra->waitNextWorker();
			assert(cur != NULL);
			nbCurrentLaunches--;
			resMan->update(cur->getNbResources());
			
			//post treatments about results
			orchestra->postActions(cur);
			safeFree(cur);
		}
	}
	
	//push results in output files
	orchestra->pushJobsIntoFiles();

	jobsMan->updateFinalState();
}


