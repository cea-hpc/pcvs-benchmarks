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

#include "XMLParser.h"
using namespace std;

xmlNodePtr XMLParser::findChildNode ( xmlNodePtr parent, char* tagname ) const {
	xmlNodePtr child = NULL;

	/* checks */
	assert(parent);
	assert(tagname != NULL);

	child = getNextChildNode(parent);
	if(child != NULL){	
		if(xmlStrcmp(child->name,(const xmlChar*)tagname) == 0 )
			return(child);
		child = findSiblingNode(child, tagname);
	}
	return child;
}


xmlNodePtr XMLParser::findSiblingNode ( xmlNodePtr node, char* tagname ) const {
	xmlNodePtr sibling = NULL;

	/* checks */
	assert(node != NULL);
	assert(tagname != NULL);

	sibling = getNextSiblingNode(node);
	
	while(sibling != NULL){
		if(xmlStrcmp(sibling->name, (const xmlChar*)tagname) == 0 ){
			return(sibling);
		}
		sibling = getNextSiblingNode(sibling);
	}

	return sibling;
}


xmlNodePtr XMLParser::getNextChildNode ( xmlNodePtr parent ) const {
	
	xmlNodePtr cur = NULL;
	assert(parent != NULL);

	if (parent == NULL)
		return(NULL);

	if(!checkValidNode(parent))
		return(NULL);
	
	cur = parent->children;

	while (cur != NULL) {
		if (cur->type == XML_ELEMENT_NODE)
			return(cur);
		cur = cur->next;
	}
	return(NULL);
}


xmlNodePtr XMLParser::getNextSiblingNode ( xmlNodePtr node ) const {
	if (node == NULL)
		return(NULL);

	if(! checkValidNode(node))
		return(NULL);
	
	node=node->next;

	while (node != NULL) {
		if (node->type == XML_ELEMENT_NODE)
			return(node);
		node = node->next;
	}

	return(NULL);
}


string XMLParser::findChildNodeContent ( xmlNodePtr node, char* tagname ) const {
	assert(node != NULL);
	assert(tagname != NULL);

	node = findChildNode(node, tagname);
	if(node == NULL) return "";
	return(getNodeContent(node));
}


string XMLParser::findSiblingNodeContent ( xmlNodePtr node, char* tagname ) const {
	assert(node != NULL);
	assert(tagname != NULL);
	
	node = findSiblingNode(node, tagname);
	if(node == NULL) return "";
	return(getNodeContent(node));
}


string XMLParser::getNodeContent ( xmlNodePtr cur ) const {
	char * tmpResult = NULL;
	string result = "";
	if(cur == NULL) return (result);
	
	tmpResult = (char*)xmlNodeGetContent(cur);
	assert(tmpResult != NULL);
	
	result = tmpResult;
	free(tmpResult);
	
	return(result);
}


bool XMLParser::checkNodeExists ( xmlNodePtr node, char* name ) const {
	assert(name != NULL);

	if(xmlStrcmp(node->name, (const xmlChar*)name) != 0){
		printError("Node "+string(name)+" not found in one of XML File ! ("+string((char*)node->name)+")", JE_NFND_JOB); 
		abort();
	}
	return true;
}


bool XMLParser::checkValidNode ( xmlNodePtr node ) const {
	switch (node->type) {
		case XML_ELEMENT_NODE:
		case XML_TEXT_NODE:
		case XML_CDATA_SECTION_NODE:
		case XML_ENTITY_REF_NODE:
		case XML_ENTITY_NODE:
		case XML_PI_NODE:
		case XML_COMMENT_NODE:
		case XML_DTD_NODE:
		case XML_XINCLUDE_START:
		case XML_XINCLUDE_END:
			return true;
		default:
			return false;
	}
}

void XMLParser::cleanUp()
{
	xmlCleanupParser();
}
