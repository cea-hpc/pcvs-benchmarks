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

#include <svUnitTest/svUnitTest.h>
#include <XMLParser.h>

using namespace std;
using namespace svUnitTest;

class TestXMLParser : public svutTestCase {
	public:
		void testMethodsRegistration(void);
		virtual void setUp(void);
		virtual void tearDown(void);
	protected:
		void testFindChildNode(void);
		void testFindSiblingNode(void);
		void testGetNextChildNode(void);
		void testGetNextSiblingNode(void);
		void testFindSiblingNodeContent(void);
		void testFindChildNodeContent(void);
		void testGetNodeContent(void);
		void testCheckNodeExists(void);
		void testCheckValidNode(void);
		XMLParser* mainParser;
		xmlDocPtr doc;
};

void TestXMLParser::testMethodsRegistration(void){
	setTestCaseName("TestXMLParser");
	SVUT_REG_TEST_METHOD(testFindChildNode);
	SVUT_REG_TEST_METHOD(testFindSiblingNode);
	SVUT_REG_TEST_METHOD(testGetNextChildNode);
	SVUT_REG_TEST_METHOD(testGetNextSiblingNode);
	SVUT_REG_TEST_METHOD(testFindSiblingNodeContent);
	SVUT_REG_TEST_METHOD(testFindChildNodeContent);
	SVUT_REG_TEST_METHOD(testGetNodeContent);
	SVUT_REG_TEST_METHOD(testCheckNodeExists);
	SVUT_REG_TEST_METHOD(testCheckValidNode);

}

void TestXMLParser::setUp(void){
	mainParser = new XMLParser;
	doc = xmlParseFile((char*)"../../../tests/unittest/xmlFile.xml");
}

void TestXMLParser::tearDown(void){
	delete mainParser;
	xmlCleanupParser();
}

void TestXMLParser::testCheckNodeExists(void)
{
	xmlNodePtr root = xmlDocGetRootElement(doc);
	SVUT_ASSERT_TRUE(mainParser->checkNodeExists(root, (char*)"jobSuite"));
}

void TestXMLParser::testCheckValidNode(void)
{
	xmlNodePtr root = xmlDocGetRootElement(doc);
	
	SVUT_ASSERT_TRUE(mainParser->checkValidNode(root));	
	root = (root->children);
	SVUT_ASSERT_TRUE(mainParser->checkValidNode(root));
}

void TestXMLParser::testFindChildNode(void)
{
	xmlNodePtr root = xmlDocGetRootElement(doc);
	SVUT_ASSERT_TRUE(mainParser->findChildNode(root, (char*)"job") != NULL);
	SVUT_ASSERT_TRUE(mainParser->findChildNode(root, (char*)"system") == NULL);
}

void TestXMLParser::testFindChildNodeContent(void)
{
	xmlNodePtr root = xmlDocGetRootElement(doc);
	root = mainParser->findChildNode(root, (char*)"job");
	SVUT_ASSERT_TRUE(mainParser->findChildNodeContent(root, (char*)"name") == (char*)"test_01"); 	
}

void TestXMLParser::testFindSiblingNode(void)
{
	xmlNodePtr root = xmlDocGetRootElement(doc);
	root = mainParser->findChildNode(root, (char*)"job");
	root = mainParser->findChildNode(root, (char*)"name");
	SVUT_ASSERT_TRUE(mainParser->findSiblingNode(root, (char*)"command") != NULL);
	SVUT_ASSERT_TRUE(mainParser->findSiblingNode(root, (char*)"blabla") == NULL);

}

void TestXMLParser::testFindSiblingNodeContent(void)
{
	xmlNodePtr root = xmlDocGetRootElement(doc);
	root = mainParser->findChildNode(root, (char*)"job");
	root = mainParser->findChildNode(root, (char*)"name");
	SVUT_ASSERT_TRUE(mainParser->findSiblingNodeContent(root, (char*)"command") == (char*)"ls -l"); 	
}

void TestXMLParser::testGetNextChildNode(void)
{
	xmlNodePtr root = xmlDocGetRootElement(doc);
	root = mainParser->getNextChildNode(root);
	SVUT_ASSERT_TRUE(root != NULL);
	root = mainParser->getNextChildNode(root);
	root = mainParser->getNextChildNode(root);
	SVUT_ASSERT_TRUE(root == NULL);
}

void TestXMLParser::testGetNextSiblingNode(void)
{
	xmlNodePtr root = xmlDocGetRootElement(doc);
	root = mainParser->getNextChildNode(root);
	SVUT_ASSERT_TRUE(root != NULL);
	root = mainParser->getNextSiblingNode(root);
	SVUT_ASSERT_TRUE(root != NULL);
	root = mainParser->getNextSiblingNode(root);
	SVUT_ASSERT_TRUE(root == NULL);
}

void TestXMLParser::testGetNodeContent(void)
{
	xmlNodePtr root = xmlDocGetRootElement(doc);
	root = mainParser->findChildNode(root, (char*)"job");
	root = mainParser->findChildNode(root, (char*)"name");
	SVUT_ASSERT_TRUE(mainParser->getNodeContent(root) == "test_01");
	root = mainParser->findSiblingNode(root, (char*)"blabla");
	SVUT_ASSERT_TRUE(mainParser->getNodeContent(root) == "");
}

SVUT_REGISTER_STANDELONE(TestXMLParser)
