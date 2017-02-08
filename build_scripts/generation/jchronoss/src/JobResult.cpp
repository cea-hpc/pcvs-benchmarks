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

#include "JobResult.h"
using namespace std;

JobResult::JobResult() {
	this->header.hashedName = 0;
	this->header.hashedData = 0;
	this->header.executionTime = 0;
	this->header.magik = 0;
	this->header.sizeData = 0;
	this->header.finalReturnCode = 0;
	this->header.startingTime = 0;
}

void JobResult::fillHeader ( std::string jobName, size_t jobId, int rc,
			   double time, double begin ) {
	this->header.id = jobId;
	this->header.hashedName = hash_fn(jobName);
	this->header.finalReturnCode = rc;
	this->header.executionTime = time;
	this->header.startingTime = begin;
	this->header.magik = DEFAULT_MAGIK_NUMBER;
}

void JobResult::insertData ( std::string data ) {
	this->flow= data;
	this->header.hashedData = hash_fn(data);
	this->header.sizeData = data.size();
}

JobResult::~JobResult(){
}

size_t JobResult::getHashName() const
{
	return header.hashedName;
}

unsigned int JobResult::getMagik() const
{
	return header.magik;
}

void JobResult::display() const
{
	cout << "+-------------- JOB RESULT --------------+" << endl;
	cout << "| - Name Hash : " <<  header.hashedName << endl;
	cout << "| - RC : " << header.finalReturnCode << endl;
	cout << "| - magik : " << header.magik << endl;
	cout << "| - Time : " << header.executionTime << endl;
	cout << "| - Data : \"" << flow << "\"" <<endl;
	cout << "+----------------------------------------+" << endl;	
}

void JobResult::checksum() const
{
	assert(header.magik == DEFAULT_MAGIK_NUMBER);
	assert(hash_fn(flow) == header.hashedData);
}

int JobResult::getFinalRC() const
{
	return header.finalReturnCode;
}

bool JobResult::read(FileManager* file) {
	bool ok = true;
	CHECK(file->isOpen());
	
	ok = file->read(reinterpret_cast<char*>(&this->header), sizeof(JobResultHeader));
	if(!ok) return false;
	
	ok = file->read(&flow, this->header.sizeData);
	if(!ok) return false;
	return true;
}

void JobResult::write(FileManager* file) {

	CHECK(file->isOpen());
	file->write(reinterpret_cast<char*>(&this->header), sizeof(JobResultHeader));
	file->write(flow.c_str(), flow.size());
}

double JobResult::getTime() const
{
	return header.executionTime;
}

std::string JobResult::getData() const
{
	return flow;
}

double JobResult::getStartTime() const
{
	return header.startingTime;
}

size_t JobResult::getHashData() const
{
	return header.hashedData;
}
size_t JobResult::getSizeData() const
{
	return header.sizeData;
}

size_t JobResult::getId() const
{
	return header.id;
}


