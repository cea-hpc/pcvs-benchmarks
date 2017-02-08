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

#include "FileManager.h"
using namespace std;

//static counter to add unicity in file name
size_t FileManager::newFileID = 0;

FileManager::FileManager() {
}

FileManager::FileManager ( std::string* exeName, FileMode access) {
	char * name;
	
	//pre actions
	assert(exeName != NULL);
	
	name = new char[exeName->size()+1];
	assert(name != NULL);
	
	name[exeName->size()] = '\0';
	exeName->copy(name, exeName->size());
	
	this->name = new string(basename(name));
	this->path = new string(dirname(name));
	assert(this->name != NULL);
	assert(this->path != NULL);
	
	mode = access; 
	stream = NULL;
	safeTabFree(name);
}

bool FileManager::close() {
	//flush & close
	stream->flush();
	stream->close();
	safeFree(stream);
	return stream == NULL;
}
bool FileManager::open() {
	switch(mode){
		case INPUT_ACCESS:
			stream = new fstream(toString().c_str(), ios::in);
			break;
		case OUTPUT_ACCESS:
			stream = new fstream(toString().c_str(), ios::out | ios::trunc);
			break;
		case INPUT_ACCESS_BIN:
			stream = new fstream(toString().c_str(), ios::in | ios::binary);
			break;
		case OUTPUT_ACCESS_BIN:
			stream = new fstream(toString().c_str(), ios::out | ios::trunc | ios::binary);
			break;
		default:
			printError("Unknown file mode !",JE_NFND_FIL);	
	}
	assert(stream != NULL);
	return stream->is_open();
}

bool FileManager::isOpen() const {
	return stream->is_open();
}

bool FileManager::isCreated(const std::string* file){
	if(file == NULL) return true;
	//check if file exists, trying to open it
	ifstream p((*file).c_str());
	return p;
}

bool FileManager::isCreated(std::string* file){
	return isCreated(const_cast<const std::string*>(file));
}

FileManager::~FileManager() {
	safeFree(name);
	safeFree(path);
}

FileManager& FileManager::operator<< ( std::string chain ) {
	stream->write(chain.c_str(), chain.size());
	return *this;
}



bool FileManager::read(std::string* arg1, long unsigned int arg2) {
	char *argChar = new char[arg2+1];
	assert(arg1 != NULL);
	for(size_t i = 0; i < arg2+1; i++)
		argChar[i] = '\0';
	stream->read(argChar, arg2);
	*arg1 = argChar;
	safeTabFree(argChar);
	return (!stream->eof());
}

bool FileManager::read(char* arg1, long unsigned int arg2) {
	assert(arg1 != NULL);
	stream->read(arg1, arg2);
	return (!stream->eof());
	
}

void FileManager::write(const char* obj, long unsigned int size) {
	assert(obj != NULL);
	stream->write(obj, size);
	stream->flush();
}

string FileManager::toString() const
{
	//pre_actions
	assert(path != NULL);
	assert(name != NULL);
	
	return ((*path)+"/"+(*name));
}

std::string FileManager::getPath() const
{
	assert(path != NULL);
	
	return ((*path)+"/");
}

string FileManager::getBaseName() const
{
	stringstream flux;
	
	assert(name != NULL);
	
	FileManager::newFileID++;
	flux << FileManager::newFileID << "-" << *name;
	return flux.str();
}

string FileManager::getName() const
{
	assert(name != NULL);
	
	return *name;
}
