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

#ifndef XMLPARSER_H
#define XMLPARSER_H

#include "utils.h"
#include "FileManager.h"
#include "XMLValidator.h"

/// Interface between XML and application
/**
 * This class implements routines to manipulate XML files in order to catch each node in
 * DOM Tree easily.
 */
class XMLParser {
protected:
	/************** MEMBERS **************/
	std::string nameFile; ///< a pointer on real file
	xmlDocPtr document;    ///< the referent document (libxml type)
	
public:
	/************** STATICS **************/
	/// Clean LibXML allocations
	/**
	 * This function is a routine for xmlCleanupParser() call. It should be called
	 * once because this function deletes all memory allocation at the same time, even
	 * if multiple threads have used the library.
	 * \warning this function is not thread safe. can only be called in sequential code
	 */
	static void cleanUp();
	
	/************* FUNCTIONS *************/
	/**** NON-CONST ****/
	///find a child node from parent in XML tree.
	/**
	 * @param parent the node where research is started 
	 * @param tagname node pattern we are looking for
	 * @return a pointer on founded node, NULL otherwise
	 */
	xmlNodePtr findChildNode(xmlNodePtr parent, char *tagname) const;
	///find a sibling node, (horizonal research) in XML tree
	/**
	 * @param node level research
	 * @param tagname node name we looking for
	 * @return a pointer on founded node, NULL otherwise
	 */
	xmlNodePtr findSiblingNode(xmlNodePtr node, char *tagname) const;
	///get contents of child node
	/**
	 * @param parent parent node
	 * @return a pointer on founded node, NULL otherwise
	 */
	xmlNodePtr getNextChildNode(xmlNodePtr parent) const;
	/// get contents of sibling node
	/** 
	 * @param node current node
	 * @return a pointer on founded node, NULL otherwise
	 */
	xmlNodePtr getNextSiblingNode(xmlNodePtr node) const;
	///Find a child node matching with pattern and return its contents 
	/**
	 * @param node the node where the research is launched
	 * @param tagname the node pattern to match
	 * @return node contents, NULL otherwise
	 */
	std::string findChildNodeContent(xmlNodePtr node, char *tagname) const;
	///Find a sibling node matching with pattern and return its contents 
	/**
	 * @param node the node where the research is launched
	 * @param tagname the node pattern to match
	 * @return node contents, NULL otherwise
	 */
	std::string findSiblingNodeContent(xmlNodePtr node, char *tagname) const;
	///get content of a node
	/** 
	 * @param cur node where data are getted
	 * @return node content, NULL otherwise
	 */
	std::string getNodeContent(xmlNodePtr cur) const;
	///Controls if specific node exists in xml File
	/**
	 * @param node node to control
	 * @param name pattern wich have to match with node name
	 * @warning his functions aborts if pattern doesn't match
	 * \return <b>True</b> if node have the same name as "name"
	 * \return <b>False</b> otherwise
	 */
	bool checkNodeExists(xmlNodePtr node, char *name) const;
	///Check if current node is a valid one
	/** 
	 * @param node node to control
	 * @return 1 if valid, 0 otherwise
	 */
	bool checkValidNode(xmlNodePtr node) const;
};

#endif // XMLPARSER_H
