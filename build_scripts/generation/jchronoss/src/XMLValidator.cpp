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

#include "XMLValidator.h"
using namespace std;

bool XMLValidator::validate(xmlDocPtr doc, std::string xsd) {

	bool ret = false;
	this->pctxt = xmlSchemaNewMemParserCtxt(xsd.c_str(), xsd.size());
	this->schema = xmlSchemaParse(pctxt);
	xmlSchemaFreeParserCtxt(pctxt);
	this->vctxt = xmlSchemaNewValidCtxt(this->schema);

 	ret = (xmlSchemaValidateDoc(this->vctxt, doc) == 0 ) ? true : false;
	xmlSchemaFree(schema);
	xmlSchemaFreeValidCtxt(vctxt);
	
	
	return ret;
}

const string XMLValidator::xsdJobsvalidation = " \
<!--<?xml version=\"1.0\" encoding=\"UTF-8\"?> -->\
<xs:schema xmlns:xs=\"http://www.w3.org/2001/XMLSchema\"> \
        <xs:element name=\"name\" type=\"xs:string\"/> \
        <xs:element name=\"command\" type=\"xs:string\"/>\
        <xs:element name=\"postCommand\" type=\"xs:string\"/>\
        <xs:element name=\"rc\" type=\"xs:string\"/>\
        <xs:element name=\"time\" type=\"xs:decimal\"/>\
        <xs:element name=\"delta\" type=\"xs:decimal\"/>\
        <xs:element name=\"resources\" type=\"xs:string\"/>\
        <xs:element name=\"dep\" type=\"xs:string\"/>\
        <xs:element name=\"constraint\" type=\"xs:string\"/>\
        <xs:element name=\"extras\" type=\"xs:string\"/>\
        <xs:element name=\"deps\">\
                <xs:complexType>\
                        <xs:sequence>\
                                <xs:element ref=\"dep\" minOccurs=\"0\" maxOccurs=\"unbounded\"/>\
                        </xs:sequence>\
                </xs:complexType>\
        </xs:element>\
        <xs:element name=\"constraints\">\
                <xs:complexType>\
                        <xs:sequence>\
                                <xs:element ref=\"constraint\" minOccurs=\"0\" maxOccurs=\"unbounded\"/>\
                        </xs:sequence>\
                </xs:complexType>\
        </xs:element>\
        <xs:element name=\"job\">\
                <xs:complexType>\
                        <xs:all>\
                                <xs:element ref=\"name\" minOccurs=\"1\" maxOccurs=\"1\"/>\
                                <xs:element ref=\"command\" minOccurs=\"1\" maxOccurs=\"1\"/>\
                                <xs:element ref=\"rc\" minOccurs=\"0\" maxOccurs=\"1\"/>\
                                <xs:element ref=\"time\" minOccurs=\"0\" maxOccurs=\"1\"/>\
                                <xs:element ref=\"delta\" minOccurs=\"0\" maxOccurs=\"1\"/>\
                                <xs:element ref=\"resources\" minOccurs=\"0\" maxOccurs=\"1\"/>\
                                <xs:element ref=\"deps\" minOccurs=\"0\" maxOccurs=\"1\"/>\
                                <xs:element ref=\"constraints\" minOccurs=\"0\" maxOccurs=\"1\"/>\
                                <xs:element ref=\"extras\" minOccurs=\"0\" maxOccurs=\"1\"/>\
                                <xs:element ref=\"postCommand\" minOccurs=\"0\" maxOccurs=\"1\"/>\
                        </xs:all>\
                </xs:complexType>\
        </xs:element>\
        <xs:element name=\"jobSuite\">\
                <xs:complexType>\
                        <xs:sequence>\
                                <xs:element ref=\"job\" minOccurs=\"0\" maxOccurs=\"unbounded\"/>\
                        </xs:sequence>\
                        <xs:attribute name=\"package\" type=\"xs:string\" use=\"optional\"/>\
                        <xs:attribute name=\"nbRemain\" type=\"xs:string\" use=\"optional\"/>\
                </xs:complexType>\
        </xs:element>\
</xs:schema>";

const string XMLValidator::xsdConfigValidation = "\
<?xml version=\"1.0\" encoding=\"UTF-8\" ?>\
<xs:schema xmlns:xs=\"http://www.w3.org/2001/XMLSchema\">\
        <xs:element name=\"item\" type=\"xs:string\"/>\
        <xs:element name=\"verbosity\" type=\"xs:nonNegativeInteger\"/>\
        <xs:element name=\"logging\" type=\"xs:nonNegativeInteger\"/>\
        <xs:element name=\"fakeExecution\" type=\"xs:string\"/>\
        <xs:element name=\"maxJobTime\" type=\"xs:nonNegativeInteger\"/>\
        <xs:element name=\"maxNbJobs\" type=\"xs:nonNegativeInteger\"/>\
        <xs:element name=\"jobsCommand\" type=\"xs:string\"/>\
        <xs:element name=\"compilationCommand\" type=\"xs:string\"/>\
        <xs:element name=\"output\" type=\"xs:string\"/>\
        <xs:element name=\"build\" type=\"xs:string\"/>\
        <xs:element name=\"maxResources\" type=\"xs:nonNegativeInteger\"/>\
        <xs:element name=\"maxSlaves\" type=\"xs:nonNegativeInteger\"/>\
        <xs:element name=\"maxSlaveTime\" type=\"xs:nonNegativeInteger\"/>\
        <xs:element name=\"minSlaveTime\" type=\"xs:nonNegativeInteger\"/>\
        <xs:element name=\"autokill\" type=\"xs:nonNegativeInteger\"/>\
        <xs:element name=\"policy\" type=\"xs:nonNegativeInteger\"/>\
        <xs:element name=\"flowsize\" type=\"xs:nonNegativeInteger\"/>\
        <xs:element name=\"online\" type=\"xs:boolean\"/>\
        <xs:element name=\"refresh\" type=\"xs:nonNegativeInteger\"/>\
        <xs:element name=\"exit_type\" type=\"xs:string\"/>\
        <xs:element name=\"jobslist\">\
                <xs:complexType>\
                        <xs:sequence>\
                                <xs:element ref=\"item\" minOccurs=\"0\" maxOccurs=\"unbounded\"/>\
                        </xs:sequence>\
                </xs:complexType>\
        </xs:element>\
        <xs:element name=\"whitelist\">\
                <xs:complexType>\
                        <xs:sequence>\
                                <xs:element ref=\"item\" minOccurs=\"0\" maxOccurs=\"unbounded\"/>\
                        </xs:sequence>\
                </xs:complexType>\
        </xs:element>\
        <xs:element name=\"blacklist\">\
                <xs:complexType>\
                        <xs:sequence>\
                                <xs:element ref=\"item\" minOccurs=\"0\" maxOccurs=\"unbounded\"/>\
                        </xs:sequence>\
                </xs:complexType>\
        </xs:element>\
        <xs:element name=\"job\">\
                <xs:complexType>\
                        <xs:all>\
                                <xs:element ref=\"jobslist\" minOccurs=\"0\" maxOccurs=\"1\"/>\
                                <xs:element ref=\"whitelist\" minOccurs=\"0\" maxOccurs=\"1\"/>\
                                <xs:element ref=\"blacklist\" minOccurs=\"0\" maxOccurs=\"1\"/>\
                                <xs:element ref=\"verbosity\" minOccurs=\"0\" maxOccurs=\"1\"/>\
                                <xs:element ref=\"logging\" minOccurs=\"0\" maxOccurs=\"1\"/>\
                                <xs:element ref=\"fakeExecution\" minOccurs=\"0\" maxOccurs=\"1\"/>\
                                <xs:element ref=\"maxJobTime\" minOccurs=\"0\" maxOccurs=\"1\"/>\
                                <xs:element ref=\"maxNbJobs\" minOccurs=\"0\" maxOccurs=\"1\"/>\
                        </xs:all>\
                </xs:complexType>\
        </xs:element>\
	<xs:element name=\"system\">\
                <xs:complexType>\
                        <xs:all>\
                                <xs:element ref=\"jobsCommand\" minOccurs=\"0\" maxOccurs=\"1\"/>\
                                <xs:element ref=\"compilationCommand\" minOccurs=\"0\" maxOccurs=\"1\"/>\
                                <xs:element ref=\"output\" minOccurs=\"0\" maxOccurs=\"1\"/>\
                                <xs:element ref=\"build\" minOccurs=\"0\" maxOccurs=\"1\"/>\
                                <xs:element ref=\"maxResources\" minOccurs=\"0\" maxOccurs=\"1\"/>\
                                <xs:element ref=\"maxSlaves\" minOccurs=\"0\" maxOccurs=\"1\"/>\
                                <xs:element ref=\"maxSlaveTime\" minOccurs=\"0\" maxOccurs=\"1\"/>\
                                <xs:element ref=\"minSlaveTime\" minOccurs=\"0\" maxOccurs=\"1\"/>\
                                <xs:element ref=\"policy\" minOccurs=\"0\" maxOccurs=\"1\"/>\
                                <xs:element ref=\"flowsize\" minOccurs=\"0\" maxOccurs=\"1\"/>\
                                <xs:element ref=\"autokill\" minOccurs=\"0\" maxOccurs=\"1\"/>\
                                <xs:element ref=\"online\" minOccurs=\"0\" maxOccurs=\"1\"/>\
                                <xs:element ref=\"refresh\" minOccurs=\"0\" maxOccurs=\"1\"/>\
                        </xs:all>\
                </xs:complexType>\
        </xs:element>\
        <xs:element name=\"configuration\">\
                <xs:complexType>\
                        <xs:all>\
                                <xs:element ref=\"job\" minOccurs=\"0\" maxOccurs=\"1\"/>\
                                <xs:element ref=\"system\" minOccurs=\"0\" maxOccurs=\"1\"/>\
                                <xs:element ref=\"exit_type\" minOccurs=\"0\" maxOccurs=\"1\"/>\
                        </xs:all>\
                </xs:complexType>\
        </xs:element>\
</xs:schema>";
