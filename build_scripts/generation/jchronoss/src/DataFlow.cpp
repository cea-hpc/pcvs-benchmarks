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

#include "DataFlow.h"
using namespace std;

//const static public class member
const string DataFlow::headerOverheadString = "\n--------------!!! WARNING !!!--------------\n WARNING : THIS FILE IS TOO BIG,\n\t ";
const string DataFlow::footerOverheadString = " BYTES WAS REMOVED          \n-------------------------------------------\n";
size_t DataFlow::bufferFlowMax = DataFlow::DEFAULT_BUFFER_FLOW_MAX;

DataFlow::DataFlow(size_t flowSize) {
	// init current buffer
	posFirstBuffer = 0;
	posSecondBuffer = 0;
	overload = false;
	if(flowSize > 0) bufferFlowMax = flowSize;
}

size_t DataFlow::fillFrom ( FILE* fd ) {
	char *firstBuffer = new char[bufferFlowMax+1];
	char *secondBuffer = new char[bufferFlowMax+1];
	size_t nbBytesRead;
	
	assert(fd != NULL);
	
	//ends final buffers
	memset(firstBuffer, '\0', bufferFlowMax+1);
	memset(secondBuffer, '\0', bufferFlowMax+1);
	
	//read up to BUFFER_FLOW_MAX bytes
	nbBytesRead = safeRead(fd, firstBuffer, bufferFlowMax);
	posFirstBuffer = nbBytesRead;
	/* if first reading isn't enough */
	if(nbBytesRead == bufferFlowMax){
		/* the next reading erase the previous */
		do{
			nbBytesRead = safeRead(fd, secondBuffer, bufferFlowMax);
			posSecondBuffer += nbBytesRead;
		}
		while( nbBytesRead > 0);
		overload = ((long int)(posSecondBuffer - bufferFlowMax) > 0);
	}
	
	nbBytesRead = size();
	this->firstBuffer = firstBuffer;
	this->secondBuffer = secondBuffer;
	return nbBytesRead;
}

size_t DataFlow::fillSpecificMessage(std::string chain)
{
	posFirstBuffer = chain.size();
	firstBuffer = chain;
	return chain.size();
}


size_t DataFlow::createHash() const {
	return hash_fn(string(getContent()));
}

bool DataFlow::matchHash ( size_t hash ) const {
	return (createHash() == hash);
}

std::string DataFlow::getContent()  const {
	string insert, finalChain;
	int overhead, modPosSecBuf;

	overhead = posSecondBuffer - bufferFlowMax;
	finalChain = firstBuffer;
	
	//detect if second buffer have been overwritten
	if(overload){
		//adding header in ouput in case of overwritting
 		insert = headerOverheadString +  static_cast<ostringstream*>(&(ostringstream() << overhead))->str() + footerOverheadString;
		finalChain += insert;
		
		//split buffer to reorder correct output
		modPosSecBuf = posSecondBuffer%bufferFlowMax;
		finalChain += secondBuffer.substr( modPosSecBuf, secondBuffer.size() -1);
		finalChain += secondBuffer.substr(0, modPosSecBuf - 1);
	} else if(posSecondBuffer > 0){
		finalChain += secondBuffer.substr(0, posSecondBuffer - 1);
	}

	//return correct chain
	return(finalChain);
}

size_t DataFlow::size() const {
	return (overload) ? 2*bufferFlowMax : posFirstBuffer + posSecondBuffer;
}

DataFlow::~DataFlow() {

}

DataFlow& DataFlow::operator= ( const DataFlow& other ) {
	this->firstBuffer = other.firstBuffer;
	this->secondBuffer = other.secondBuffer;
	
	this->overload = other.overload;
	this->posFirstBuffer = other.posFirstBuffer;
	this->posSecondBuffer = other.posSecondBuffer;
	return *this;
}

DataFlow::DataFlow(const DataFlow& other)
{
	*this = other;
}
