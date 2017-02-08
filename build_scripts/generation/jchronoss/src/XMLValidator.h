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

#ifndef XMLVALIDATOR_H
#define XMLVALIDATOR_H

#include "utils.h"
#include <libxml/parser.h>
#include <libxml/tree.h>
#include <libxml/xmlschemas.h>

/// validate a XML file with a XML scheme
/**
 * This class is used to ensure data are well-formatted
 */
class XMLValidator {
private:
	/************** MEMBERS **************/
	xmlSchemaPtr schema;          ///< the schema pointer on the doc
	xmlSchemaValidCtxtPtr vctxt;  ///< the xml validator
	xmlSchemaParserCtxtPtr pctxt; ///< the schema loader

public:
	/************** STATICS **************/
	/****** CONST ******/	
	static const std::string xsdJobsvalidation; ///< the xml schema in string format
	static const std::string xsdConfigValidation; ///< the xml schema in string format for configuration file validation
	
	/************* FUNCTIONS *************/
	/**** NON-CONST ****/
	/// apply xml validation to the given xml document
	/**
	 * The xml schema is applied to the given document in order to ensure well-formatted
	 * data
	 * \param[in] doc the xml document to validate
	 * \param[in] xsd the xml schema to use (in statics one)
	 * \return <b>True</b> if document is found, opened and valid
	 * \return <b>False</b> otherwise
	 */
	bool validate(xmlDocPtr doc, std::string xsd);
};

#endif // XMLVALIDATOR_H
