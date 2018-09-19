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

#include "XMLJobWriter.h"
using namespace std;

XMLJobWriter::XMLJobWriter(FileManager* f, size_t nb) {
	char buf[12];

	document = xmlNewDoc(BAD_CAST "1.0");
	this->f = f;
	/* check if file exists */
	if (document == NULL) {
		printError("Unable to write XML file '"+f->toString()+"'!\n", JE_NFND_FIL);
		abort();
	}

	root_node = xmlNewNode(NULL, BAD_CAST "jobSuite");
	snprintf(buf, 12, "%lu", nb);
	xmlNewProp(root_node, BAD_CAST "nbRemain", BAD_CAST  buf);
	xmlDocSetRootElement(document, root_node);
}

XMLJobWriter::~XMLJobWriter() {
	xmlFreeDoc(document);
	xmlCleanupParser();
}

void XMLJobWriter::flush()
{
	xmlSaveFormatFileEnc(f->toString().c_str(), document, "UTF-8", 0);
}

void XMLJobWriter::writeJob(Job* job)
{
	char buf[32];
	xmlNodePtr cur = xmlNewNode(NULL, BAD_CAST "job");
	//command = (*it)->getCommand();
	//HTMLEncoding(command);
	xmlNewTextChild(cur, NULL, BAD_CAST "name", BAD_CAST job->getName().c_str());
	xmlNewTextChild(cur, NULL, BAD_CAST "deps", BAD_CAST "");
	xmlNewTextChild(cur, NULL, BAD_CAST "constraints", BAD_CAST "");
	xmlNewTextChild(cur, NULL, BAD_CAST "postCommand", BAD_CAST job->getPostCommand().c_str());
	xmlNewTextChild(cur, NULL, BAD_CAST "extras", BAD_CAST job->getExtras().c_str());
	xmlNewTextChild(cur, NULL, BAD_CAST "command", BAD_CAST job->getCommand().c_str());
	
	snprintf(buf, 32, "%lu", job->getNbResources());
	xmlNewTextChild(cur, NULL, BAD_CAST "resources", BAD_CAST buf);
	
	snprintf(buf, 32, "%d", job->getExpectedReturn());
	xmlNewTextChild(cur, NULL, BAD_CAST "rc", BAD_CAST buf);
	
	snprintf(buf, 32, "%f", job->getExpectedTime());
	xmlNewTextChild(cur, NULL, BAD_CAST "time", BAD_CAST buf);
	
	snprintf(buf, 32, "%f", job->getDelta());
	xmlNewTextChild(cur, NULL, BAD_CAST "delta", BAD_CAST buf);
	
	xmlAddChild(root_node, cur);
}
