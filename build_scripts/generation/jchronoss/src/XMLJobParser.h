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


#ifndef XMLJOBPARSER_H
#define XMLJOBPARSER_H

#include "Job.h"
#include "FilterBox.h"
#include "XMLParser.h"

/// Interface between XML Jobs files and the application.
/**
 * This class wrappes LibXML2 using, with routines helping to extract data from DOM tree
 * More specifically, XMLJobParser is destinated to load data from XML Job File. This include
 * a xml validation with a xml scheme defined in XMLValidator.
 */
class XMLJobParser : private XMLParser {
private:
	/************** MEMBERS **************/
	std::string jobsGroup; ///< the package attached to the loaded file
	FilterBox* filter;     ///< the whole filter used to decide whether a job is valid or not
	
	/************* FUNCTIONS *************/
	/**** NON-CONST ****/
	/// start file reading
	/**
	 * After some preActions, the XMLParser::parseFromFile function starts file dumping. This function loads
	 * each \<job\>\</job\> and create the associated job object.
	 * \param[out] property For the slave only, the property is set with remaining jobs number in master
	 * \return A jobs list pointer
	 * \return <b>NULL</b> if there are no job to parse
	 */
	std::list< Job* >* parseJobsSuite(size_t* property);
	/// create one job from current reading
	/**
	 * For each \<job\>\</job\> encoutered, this function is called to extract data from file and return a 
	 * Job pointer
	 * \param[in] location the current "job" node in the file
	 * \return a job pointer on new created object
	 */
	Job* parseJob( xmlNodePtr location );
	/// parse the whole constraints for a given job
	/**
	 * This function load all constraints depicted in the file for the given job
	 * \param[out] list the constraints' list to fill
	 * \param[in] location the "constraint" node in the file for the current job
	 * 
	 */
	void parseConstraints( std::vector< JobConstraint* >& list, xmlNodePtr location );
	/// parse the whole dependences for a given job
	/**
	 * This function load all dependences depicted in the file for the given job
	 * \param[out] list the dependences' list to fill
	 * \param[in] location the "dependences" node in the file for the current job
	 */
	void parseDeps( std::vector< std::string* >& list, xmlNodePtr location );
	
public:
	/************* FUNCTIONS *************/
	/**** NON-CONST ****/
	///standard constructor
	explicit XMLJobParser();
	///constructor initializing the default XML Parser
	XMLJobParser( std::string name, FilterBox* const filtering );
	///dump data from file and generate a jobs list
	/**
	 * This function read data from file assigned by constructor and generate a pointer
	 * on a jobs list, with all valid jobs (i.e. the filtering is done here)
	 * \param[out] property For the slave only, the property is set with remaining jobs number in master
	 * \return A jobs list pointer
	 * \return <b>NULL</b> if there are no job to parse
	 */
	std::list< Job* >* parseFromFile(size_t* property);

	///virtual destructor to unset the parser
	virtual ~XMLJobParser();
};

#endif // XMLPARSER_H
