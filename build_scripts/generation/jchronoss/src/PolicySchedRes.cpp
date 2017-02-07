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

#include "PolicySchedRes.h"
using namespace std;

SchedResPolicy::SchedResPolicy(JobManager* jobMan, Configuration* config): Policy(jobMan, config) {

}

void SchedResPolicy::fillingAlgo(Worker * cur, size_t maxIndice,
				 size_t nbCurResources)
{
	std::list<Job*>* currentJM = jobMan->jobsLists();
	int i;
	size_t nbJobs = 0, nbMaxJobs = 0, time = 0, maxTime = 0;
	
	assert(maxIndice <= nbCurResources);
	UNUSED(nbCurResources);
	
	nbMaxJobs = config->job().getNbMaxJobs();
	i = maxIndice;
	maxTime = config->system().getMaxSlaveTime();
	
	do{
		for(list<Job*>::iterator it = currentJM[i].begin(); it != currentJM[i].end();){
			Job * jcur = *it;
			assert(jcur->getStatus() == NOT_RUN);
			if(nbJobs >= nbMaxJobs && nbMaxJobs != JobConfiguration::DEFAULT_MAX_NB_JOBS && time <= maxTime)
				break;
			else if( jcur->isDepInvalid()){
				if(!jcur->addATry())
					jcur->updateStatus(MUCH_TRIES);
				it++;
				continue;
			}
			time += jcur->getExpectedTime();
			cur->add(jcur);
			// here the it++ is done by erase function !!!!
			it = currentJM[i].erase(it);
			nbJobs++;
		}
	i--;
	}while(i >= 0 && cur->size() <= 0);
	// +2 = +1 for list indice and +1 for extra decrement
	cur->setNbRequiredResources(i+2);
}
