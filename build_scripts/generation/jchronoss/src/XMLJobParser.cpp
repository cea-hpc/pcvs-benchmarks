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

#include "XMLJobParser.h"
using namespace std;


XMLJobParser::XMLJobParser() {

}

XMLJobParser::XMLJobParser(std::string name, FilterBox* const filtering) {
	nameFile = name;
	filter = filtering;
	jobsGroup  = "";

	document = xmlParseFile(name.c_str());
	/* check if file exists */
	if (document == NULL) {
		printError("XML file '"+name+"' not found !\n", JE_NFND_FIL);
		abort();
	}
}

XMLJobParser::~XMLJobParser() {
	xmlFreeDoc(document);
}

list<Job*>* XMLJobParser::parseFromFile(size_t * property) {
	XMLValidator validator;

	if(validator.validate(document, XMLValidator::xsdJobsvalidation))
		return parseJobsSuite(property);
	else {
		printWarning("XML file '"+nameFile+"' not valid !\n");
		return NULL;
	}
}

void XMLJobParser::parseDeps(std::vector<std::string*>& list, xmlNodePtr location) {
	xmlNodePtr child = NULL;
	string *dep;
	
	assert(list.empty() == true);
	
	checkNodeExists(location, (char*)"job");
	
	child = findChildNode(location, (char*)"deps");
	if(child == NULL) return;

	child = findChildNode(child, (char*)"dep");
	
	while(child != NULL){
		dep = new string(getNodeContent(child));
		if(dep->find_last_of(".") >= dep->size())
			*dep = jobsGroup +"."+ *dep;
		list.push_back(dep);
		child = getNextSiblingNode(child);
	}
}

void XMLJobParser::parseConstraints ( std::vector< JobConstraint* >& list, xmlNodePtr location ) {
	xmlNodePtr child = NULL;
	string* constraint;
	
	assert(list.empty() == true);
	
	checkNodeExists(location, (char*)"job");

	
	child = findChildNode(location, (char*)"constraints");
	if(child == NULL) return;

	child = findChildNode(child, (char*)"constraint");
	
	while(child != NULL){
		constraint = new string(getNodeContent(child));
		
		list.push_back(new JobConstraint(*constraint));
		safeFree(constraint);
		child = getNextSiblingNode(child);
	}
}

Job* XMLJobParser::parseJob(xmlNodePtr location) {
	string name = "", command, postCommand, extras, chain;
	size_t nbResources = 1;
	int rc = 0;
	double time = -1.0, delta = -1.0;
	vector<string*> vDeps;
	vector<JobConstraint*> vConstraints;
	
	checkNodeExists(location, (char*)"job");

	name = findChildNodeContent(location,(char*)"name");
	
	command = findChildNodeContent(location,(char*)"command");
	postCommand = findChildNodeContent(location,(char*)"postCommand");
	extras = findChildNodeContent(location,(char*)"extras");
	if ((chain = findChildNodeContent(location, (char*)"resources").c_str()) != "")
		nbResources = atoi(chain.c_str());
	if ((chain = findChildNodeContent(location, (char*)"rc").c_str()) != "")
		rc = atoi(chain.c_str());
	if ((chain = findChildNodeContent(location, (char*)"time").c_str()) != "")
		time = atof(chain.c_str());
	if ((chain = findChildNodeContent(location, (char*)"delta").c_str()) != "")
		delta = atof(chain.c_str());

	parseDeps(vDeps, location);
	parseConstraints(vConstraints, location);

	//for(vector<JobConstraint*>::iterator it = vConstraints.begin(); it != vConstraints.end() && !directly_selected; it++){
		//if((*it)->getName() == JobConstraint::COMPILATION_TAG)
			//directly_selected=true;
	//}

	Job * obj =  new Job(name, jobsGroup, command, vDeps, vConstraints, nameFile, extras, postCommand, nbResources, rc, time, delta);
	
	if(!filter->accept(name))
	{
		obj->updateStatus(DISABLED);
		obj->addResult(-1, 0.0, 0.0, "Job disabled (Filter does not accept it)");
	}
	

	return obj;
}

std::list<Job*>* XMLJobParser::parseJobsSuite(size_t *property) {
	xmlNodePtr root = xmlDocGetRootElement(document), cur;
	list<Job*>* group = NULL;
	Job* curJob = NULL;
	char * prop = NULL;
	
	group = new list<Job*>();
	checkNodeExists(root, (char*)"jobSuite");

	if((prop = (char*)xmlGetProp(root, (const xmlChar *)"package")) != NULL) {
		jobsGroup = prop;
		free(prop);
	} 
	else if((prop = (char*)xmlGetProp(root, (const xmlChar *)"nbRemain")) != NULL){
		(*property) = atoi(prop);
		free(prop);
	}
	
	cur = getNextChildNode(root);
	while(cur != NULL){
		curJob = parseJob(cur);
		if(curJob != NULL) group->push_back(curJob);
		cur = getNextSiblingNode(cur);
	}
	
	return group;
}
