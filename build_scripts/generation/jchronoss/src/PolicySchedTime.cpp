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

#include "PolicySchedTime.h"
using namespace std;

double SchedTimePolicy::curJobManSurface = 0.0;
int SchedTimePolicy::nbCalls = 0;

SchedTimePolicy::SchedTimePolicy(JobManager* job, Configuration* config) : Policy(job, config)
{

}

void SchedTimePolicy::fillingAlgo(Worker * cur, size_t maxIndice, size_t nbCurResources)
{

	//datas
	bool fullWorker = false;
	std::list<Job*>* currentJM = jobMan->jobsLists();
	int i;
	//resources
	size_t nRes_1passe = 0, nWorkers = 0, nRes = 0, biggestJob = 1;
	//surface
	size_t surface_1passe = 0, currentSurface = 0, workloadSurface = 0;
	// timers
	size_t time_1passe = 0, tWorkerMax = 0, tWorkerMin = 0, tJobMax = 0;
	size_t maxRequiredResources = 0;
	//getters
	nRes = config->system().getNbMaxResources();
	nWorkers = config->system().getNbMaxSlaves();
	nWorkers = (nWorkers <= 0) ? 1 : nWorkers;
	//warning : if worker max not defined, value = -1 (unsigned)
	tWorkerMax = config->system().getMaxSlaveTime();
	tWorkerMin = config->system().getMinSlaveTime();
	//tJobMax = config->job().getMaxJobTime();
	UNUSED(tJobMax);

	if (nbCalls%nWorkers == 0)
		SchedTimePolicy::recomputeCurSurface(jobMan->jobsLists(), jobMan->getNbLists());
	nbCalls++;

	workloadSurface = curJobManSurface;

	//before computing, some checks have to be done to ensure coherency
	assert(nRes > 0);
	assert(nWorkers > 0);
	assert(tWorkerMax >= tWorkerMin);
	assert(tJobMax <= tWorkerMax);

	//computing
	nRes_1passe = (int)(nRes / nWorkers);
	time_1passe = std::min((size_t)(workloadSurface/nRes), tWorkerMax);

	//define min computing time for a resources (here 1 second), then all jobs are given to the next worker
	if(time_1passe < 1){
		surface_1passe = workloadSurface;
	} else {
		surface_1passe = nRes_1passe * time_1passe;
	}

// 	cout << "nWorkers = " << nWorkers << endl;
// 	cout << "nRes = " << nRes << endl;
// 	cout << "nJobs = " << nJobs << endl;
// 	cout << "tWorkerMax = " << tWorkerMax << endl;
// 	cout << "tWorkerMin = " << tWorkerMin << endl;
// 	cout << "tJobMax = " << tJobMax << endl;
// 	cout << "workloadSurface = " << workloadSurface << endl;
// 	cout << "nRes_1passe = " << nRes_1passe << endl;
//	cout << "time_1passe = " << time_1passe << endl;
// 	cout << "surface_1passe = " << surface_1passe << endl;

	i = maxIndice;
	do{
		for(list<Job*>::iterator it = currentJM[i].begin(); it != currentJM[i].end();){
			Job * jcur = *it;
			assert(jcur->getStatus() == NOT_RUN);
			if(jcur->getExpectedTime() > tWorkerMax){
				printError("A job time is bigger than max time allowed to allocation.\n ERROR        : The \"Sched by Time\" won't schedule this job !", JE_LACK_TIM);
			}

			// warning when handling with effective jobs time
			if(currentSurface >= surface_1passe){
				fullWorker = true;
				break;
			}
			else if(jcur->isDepInvalid()){
				it = jobMan->delayJob(i, it);
				continue;
			}
			cur->add(jcur);
			currentSurface += (jcur->getNbResources() * jcur->getExpectedTime());
			maxRequiredResources += jcur->getNbResources();

			biggestJob = std::max(biggestJob, jcur->getNbResources());
			it = jobMan->pickJob(i, it);
		}
	i--;
	}while(i >= 0 && !fullWorker);

	/* first, we check we take at least one job */
	if(maxRequiredResources == 0)
	{
		/* the worker won't be scheduled */
		nRes_1passe = 0;
		goto ret_func;
	}
	/*** State some corner case in that scheduling: ***
	 * 1. If no jobs left, take all remanining resources
	 * 2. If the number of resources requested by all jobs is lower than number of assigned resources => shrink
	 * 3. If the min time per resource is not reached => shrink
	 * 4. Check if the biggest job can still be run inside the shrunk allocation
	 */

	/* 1. */
	if(jobMan->isEmpty())
		nRes_1passe = nbCurResources;

	/* 2. */
	nRes_1passe = std::min(maxRequiredResources, nRes_1passe);

	/* 3. */
	if((size_t)(currentSurface/nRes_1passe) < tWorkerMin)
		nRes_1passe = std::max((size_t)(currentSurface/tWorkerMin), (size_t)1);

	/* 4. */
	nRes_1passe = std::max(biggestJob, nRes_1passe);

ret_func: /* this label is used to handle special case (no jobs to run,...) */
	assert(nRes_1passe <= nbCurResources);
	cur->setNbRequiredResources(nRes_1passe);
}

void SchedTimePolicy::recomputeCurSurface(std::list<Job*>* currentJM, size_t nbLists)
{
	curJobManSurface = 0.0;

	for(size_t i = 0; i < nbLists ; i++){
		for(list<Job*>::iterator it = currentJM[i].begin(); it != currentJM[i].end(); it++){
			curJobManSurface += ((*it)->getExpectedTime() * (*it)->getNbResources());
		}
	}
}
