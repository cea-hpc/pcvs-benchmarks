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

#include "Job.h"
using namespace std;

size_t Job::id = 0;

Job::Job() : status(NOT_RUN) {}

Job::Job ( std::string name, std::string command, size_t nbRes,  int rc, double time, std::vector< std::string* > deps, std::vector< JobConstraint* > constraints, std::string file ) :
	fullName(name), command(command), nbDeps(deps.size()), nbTries(0), nbResources(nbRes), expectedReturnCode(rc), timer(time), vDepsNamesTab(deps), vConstraints(constraints), status(NOT_RUN), result(NULL), referentFilename(file)
{
	myId = ++id;
	size_t ind = fullName.find_last_of(".");
	if(ind >= fullName.size()){
		ind = 0;	
	}

	shortName = fullName.substr(ind+1);
}

Job::~Job() {
	safeFree(result);
	for(vector<string*>::iterator it = vDepsNamesTab.begin(); it != vDepsNamesTab.end();){
		safeFree(*it);
		it = vDepsNamesTab.erase(it);
	}
	
	for(vector<JobConstraint*>::iterator it = vConstraints.begin(); it != vConstraints.end();){
		safeFree(*it);
		it = vConstraints.erase(it);
	}
}

void Job::display() const {
	cout << "|\t - "<< fullName << " --> " << hex << this << dec
		<< "\n|\t\t - command: \"" << command << "\""
		<< "\n|\t\t - Expected code: " << expectedReturnCode
		<< "\n|\t\t - status: " << status
		<< "\n|\t\t - NbDeps: " << nbDeps;
	for(size_t i=0; i<nbDeps; i++)
		cout<< "\n|\t\t\t --> " << *vDepsNamesTab.at(i);
	
	cout << "\n|\t\t - nbConstraints: " << vConstraints.size();
	for(size_t i=0; i<vConstraints.size(); i++)
		cout<< "\n|\t\t\t -->" << vConstraints.at(i)->getName();
	cout << "\n|\t\t - Into " << referentFilename;
	cout << endl;
	if(result != NULL)
		result->display();
	
	for(vector<JobConstraint*>::const_iterator it = vConstraints.begin(); it != vConstraints.end(); it++)
		(*it)->display();

}

bool Job::isValid ( std::string tag ) const {
	
	for(vector<JobConstraint*>::const_iterator it = vConstraints.begin(); it != vConstraints.end(); it++)
		if(tag == (*it)->getName()) 
			return true;
		
	return false;
}

void Job::addConstraint ( JobConstraint* constraint ) {
	assert(constraint != NULL);
	vConstraints.push_back(constraint);
}

void Job::addDependency ( Job* dep ) {
	vDeps.push_back(dep);
}

const std::vector< JobConstraint* >& Job::getConstraints() const {
	return vConstraints;
}

const std::vector< std::string* >& Job::getDepsNames() const {
	return vDepsNamesTab;
}

const std::vector< Job* >& Job::getDeps() const {
	return vDeps;
}

size_t Job::getNbResources() const {
	return nbResources;
}

int Job::getNbDeps() const {
	return nbDeps;
}
size_t Job::getId() const
{
	return myId;
}

string Job::getName() const {
	return fullName;
}

void Job::updateStatus ( JobStatus status ) {
	this->status = status;
}

JobStatus Job::getStatus() const {
	return status;
}

void Job::addResult(int rc, double time, double begin, std::string data){
	result = new JobResult;
	result->fillHeader(fullName, myId, rc, time, begin);
	result->insertData(data);
}

std::string Job::getCommand() const
{
	return command;
}
int Job::getExpectedReturn() const
{
	return expectedReturnCode;
}

 const JobResult& Job::getResult() const
{
	return *result;
}

string Job::getReferentFilename() const
{
	return referentFilename;
}

string Job::getShortName() const
{
	return shortName;
}

void Job::setResult(JobResult* res)
{
	assert(res != NULL);
	result = res;
}

void Job::setId(size_t value)
{
	myId = value;
}

size_t Job::getNbTries() const
{
	return nbTries;
}

bool Job::addATry()
{
	return ++nbTries < DEFAULT_MAX_TRIES;

}

double Job::getExpectedTime() const
{
	return timer;
}

void Job::setExpectedTime(double time){
	timer = time;
}

bool Job::isDepInvalid(bool before) const
{
	for(vector<Job*>::const_iterator it = vDeps.begin(); it != vDeps.end(); it++){
		if(
			(*it)->getStatus() == FAILED || 
			(*it)->getStatus() == NOT_RUNNABLE || 
			(*it)->getStatus() == INVALID_DEPS || 
			(*it)->getStatus() == MUCH_TRIES || 
			(before && (*it)->getStatus() == NOT_RUN))

			return true;
	}
	return false;
}

