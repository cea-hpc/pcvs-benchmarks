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


#include "Worker.h"
using namespace std;

unsigned int Worker::numWorker = 0;

size_t Worker::getNbResources() const {
	return nbRequiredResources;
}
void Worker::setNbRequiredResources(size_t val) {
	nbRequiredResources = val;
}
FileManager* Worker::getInputFile() const {
	return inputFile;
}

FileManager* Worker::getOutputFile() const {
	return outputFile;
}

void Worker::setIOFiles(FileManager* input, FileManager* output, FileManager* trace)
{
	//input can be NULL from RunnerSlave
	assert(output != NULL);
	inputFile = input;
	outputFile = output;
	traceFile = trace;
}

void Worker::unsetIOFiles()
{
	safeFree(traceFile);
	safeFree(outputFile);
	safeFree(inputFile);
}

void Worker::add(Job* job)
{
	jobsList.push_back(job);
}

Worker::Worker()
{
	inputFile = NULL;
	outputFile = NULL;
	nbRequiredResources = 0;
	numWorker++;
}

Worker::~Worker()
{}

void Worker::pushJobsInputFile(int nbRemain)
{
	size_t id = 1;
	XMLJobWriter writer(inputFile, nbRemain+jobsList.size());
	
	
	for(list<Job*>::iterator it = jobsList.begin(); it != jobsList.end() ; it++){
		(*it)->setId(id++);
		writer.writeJob(*it);
	}
	
	writer.flush();
}

void Worker::pullJobsOutputFile()
{
	JobResult* resCur = NULL;
	Job* jobCur = NULL;
	std::string file = outputFile->toString();
	if(!FileManager::isCreated(&file))
		printError("An output file should exist by slave but it didn't !\n ERROR        : Have you provide scripts which do not call JCHRONOSS again ?", JE_UNKNOWN);
	CHECK(outputFile->open());
	resCur = new JobResult;
	while(resCur->read(outputFile)){
		jobCur = NULL;
		resCur->checksum();

		for(list<Job*>::iterator it = jobsList.begin(); it != jobsList.end() ; it++){
			if((*it)->getId() == resCur->getId()){
				assert(hash_fn((*it)->getName()) == resCur->getHashName());
				jobCur = (*it);
			}
		}
		assert(jobCur != NULL);
		
		if(jobCur->isPassed(resCur->getFinalRC(), resCur->getTime()))
		{
			jobCur->updateStatus(PASSED);
		}
		else{
			jobCur->updateStatus(FAILED);
		}
		jobCur->setResult(resCur);
		resCur = new JobResult;
	}
	safeFree(resCur);
	CHECK(outputFile->close());
	
#ifdef NDEBUG
// 	remove worker files when worker isn't required anymore
 	remove(inputFile->toString().c_str());
	remove(outputFile->toString().c_str());
#endif
}

void Worker::pushJobsOutputFile()
{
	ostringstream trace;
	for(list<Job*>::iterator it = jobsList.begin(); it != jobsList.end() ; it++){
		JobResult cur = (*it)->getResult();
		cur.write(outputFile);
#ifdef ENABLE_TRACE
		trace << (*it)->getName() << fixed << setprecision(2) << ":" << cur.getStartTime() << ":" << (cur.getStartTime() + cur.getTime()) << ":" << (*it)->getNbResources() << endl;  
		(*traceFile) << trace.str();
#endif
	}
}

std::list< Job* >::iterator Worker::begin() {
	return jobsList.begin();
}

std::list< Job* >::iterator Worker::end() {
	return jobsList.end();
}

const std::list<Job*>* Worker::getList() const {
	return &jobsList;
}

void Worker::display() const {
	cout 	<< "+------------------ WORKER -----------------+" << endl;
	cout	<< "| - NbResources = " << nbRequiredResources << endl;
	cout	<< "+-------------------------------------------+" << endl;
	for(list<Job*>::const_iterator it = jobsList.begin(); it != jobsList.end(); it++)
		(*it)->display();
	cout	<< "+-------------------------------------------+" << endl;
}

size_t Worker::size() const
{
	return jobsList.size();
}
