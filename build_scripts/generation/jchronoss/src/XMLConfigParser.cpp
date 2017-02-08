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

#include "XMLConfigParser.h"
using namespace std;

XMLConfigParser::XMLConfigParser(std::string fileName)
{
	nameFile = fileName;
	document = xmlParseFile(fileName.c_str());
	if (document == NULL) {
		printError("XML file '"+fileName+"' not found !\n", JE_NFND_FIL);
		abort();
	}
	checkConfig();
	
	jobNode = findChildNode(xmlDocGetRootElement(document), (char*)"job");
	systemNode = findChildNode(xmlDocGetRootElement(document), (char*)"system");
}

void XMLConfigParser::checkConfig()
{
	XMLValidator validator;
	
	if(!validator.validate(document, XMLValidator::xsdConfigValidation)){
		printError("XML Config file '"+nameFile+"' not valid !\n", JE_NFND_FIL);
	}
}

void XMLConfigParser::loadInt(int &val, const bool parent, const char nodeName[]) const
{
	xmlNodePtr dad = (parent == JOB_CONF) ? jobNode : systemNode;
	string chain = findChildNodeContent(dad, (char*)nodeName);
	if(chain != "")
		val =  atoi(chain.c_str());
}
void XMLConfigParser::loadFloat(float &val, const bool parent, const char nodeName[]) const
{
	xmlNodePtr dad = (parent == JOB_CONF) ? jobNode : systemNode;
	string chain = findChildNodeContent(dad, (char*)nodeName);
	if(chain != "")
		val =  atof(chain.c_str());
}

void XMLConfigParser::loadBool(bool& val, const bool parent, const char nodeName[]) const{
	xmlNodePtr dad = (parent == JOB_CONF) ? jobNode : systemNode;
	string chain = findChildNodeContent(dad, (char*)nodeName);
	if(chain != ""){
		for(size_t i=0; i < chain.size(); i++) chain[i] = tolower(chain[i]);
		val = (chain == "true");
	}
}
void XMLConfigParser::loadList(std::vector< FileManager* >& list, const bool parent, const char globalNodeName[]) const
{
	xmlNodePtr dad = (parent == JOB_CONF) ? jobNode : systemNode;
	xmlNodePtr child = findChildNode(dad, (char*)globalNodeName);
	child = findChildNode(child, (char*)"item");
	while(child != NULL){
		list.push_back(new FileManager(new string(getNodeContent(child)), INPUT_ACCESS));
		child = getNextSiblingNode(child);
	}
}
void XMLConfigParser::loadStr(std::string& val, const bool parent, const char nodeName[]) const
{
	xmlNodePtr dad = (parent == JOB_CONF) ? jobNode : systemNode;
	string chain = findChildNodeContent(dad, (char*)nodeName);
	if(chain != "")
		val =  chain;
}

XMLConfigParser::~XMLConfigParser()
{
	xmlFreeDoc(document);
}
