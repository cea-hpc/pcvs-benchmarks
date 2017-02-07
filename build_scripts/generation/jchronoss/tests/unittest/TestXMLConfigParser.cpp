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

#include <svUnitTest/svUnitTest.h>
#include <Configuration.h>

using namespace std;
using namespace svUnitTest;

class TestConfiguration : public svutTestCase {
	public:
		void testMethodsRegistration(void);
		virtual void setUp(void);
		virtual void tearDown(void);
	protected:
		void testparseOptions(void);
		Configuration* mainConfiguration;
};

void TestConfiguration::testMethodsRegistration(void){
	setTestCaseName("TestConfiguration");
	SVUT_REG_TEST_METHOD(testparseOptions);
}

void TestConfiguration::setUp(void){
	mainConfiguration = new Configuration;
}

void TestConfiguration::tearDown(void){
	delete mainConfiguration;
}

void TestConfiguration::testparseOptions ( void ) {
	int argc = 5;
	char * argv[] = {(char*)"/path/to/exe", (char*)"--master", (char*)"--nb-resources=10", (char*)"--verbose", (char*)"--config-file=../../../tests/integration/configSimple.xml"};
	
	mainConfiguration->parseOptions(argc, argv);
	SVUT_ASSERT_EQUAL(mainConfiguration->getExeName(), "/path/to/exe");
	SVUT_ASSERT_EQUAL(mainConfiguration->isMaster(), true);
	
	SVUT_ASSERT_EQUAL(mainConfiguration->system().getNbMaxResources(), 10);
	SVUT_ASSERT_EQUAL(mainConfiguration->job().getVerbosity(), VERBOSE_ONLY_ERROR);
	SVUT_ASSERT_EQUAL(*mainConfiguration->getConfigFile(),"../../../tests/integration/configSimple.xml");
	
}


SVUT_REGISTER_STANDELONE(TestConfiguration);
