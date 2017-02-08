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

#ifndef XMLCONFIGPARSER_H
#define XMLCONFIGPARSER_H

#include "XMLParser.h"

/// class handling configuration file
/**
 * Instead of using arguments line, options can be loaded with a configuration
 * file. This class extracts data from the file and fill a given configuration
 */
class XMLConfigParser : private XMLParser {
private:
	/************** MEMBERS **************/
	xmlNodePtr jobNode;    ///< a pointer on \<job/\> in configuration file
	xmlNodePtr systemNode; ///< a pointer on \<system/\> in configuration file
	
public:
	static const bool JOB_CONF = true;  ///< looking for job options
	static const bool SYS_CONF = false; ///< looking for system options
	
	/************* FUNCTIONS *************/
	/**** NON-CONST ****/
	/// cosntruct a parser with the given file name
	/**
	 * This function validates the given file with pre-registered xml schema
	 * and set class members.
	 * \param[in] fileName configuration file
	 */
	XMLConfigParser(std::string fileName);
	/// validates current config file with xml schema
	void checkConfig();
	/// load a node content as a integer value
	/**
	 * This function loads the content of nodeName node, located is the subtree
	 * belonging to parent (job or system ones). Then, the value is casted into
	 * integer and assigned to val
	 * \param[out] val where to store value
	 * \param[in] parent a bool to select job or system parent node
	 * \param[in] nodeName node name where data will be extracted.
	 */
	void loadInt(int& val, const bool parent, const char nodeName[]) const;
	/// load a node content as a float value
	/**
	 * This function loads the content of nodeName node, located is the subtree
	 * belonging to parent (job or system ones). Then, the value is casted into
	 * float and assigned to val
	 * \param[out] val where to store value
	 * \param[in] parent a bool to select job or system parent node
	 * \param[in] nodeName node name where data will be extracted.
	 */
	void loadFloat(float& val, const bool parent, const char nodeName[]) const;
	/// load a node content as a string value
	/**
	 * This function loads the content of nodeName node, located is the subtree
	 * belonging to parent (job or system ones). Then, the value is casted into
	 * string and assigned to val
	 * \param[out] val where to store value
	 * \param[in] parent a bool to select job or system parent node
	 * \param[in] nodeName node name where data will be extracted.
	 */
	void loadStr(std::string& val, const bool parent, const char nodeName[]) const;
	/// fill a list with data stored in given node
	/**
	 * This function identifies a given node. Then, it extracts all \<item/\> nodes
	 * and, for each one, add a new entry in given list. Each node is considered as
	 * file name and thus, a FileManager is directly created as input file
	 * \param[out] list where to store value
	 * \param[in] parent a bool to select job or system parent node
	 * \param[in] globalNodeName global node name gathering \<item/\> nodes.
	 */
	void loadList(std::vector < FileManager * >&list, const bool parent, const char globalNodeName[]) const;
	/// load a node content as a boolean value
	/**
	 * This function loads the content of nodeName node, located is the subtree
	 * belonging to parent (job or system ones). Then, the value is casted into
	 * boolean and assigned to val
	 * \param[out] val where to store value
	 * \param[in] parent a bool to select job or system parent node
	 * \param[in] nodeName node name where data will be extracted.
	 */
	void loadBool(bool& val, const bool parent, const char nodeName[]) const;
	/// virtual destructor to unset a Config parser
	~XMLConfigParser();
};

#endif // XMLCONFIGPARSER_H
