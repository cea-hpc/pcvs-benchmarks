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
#include <JobResult.h>

using namespace std;
using namespace svUnitTest;

class TestJobResult : public svutTestCase {
	public:
		TestJobResult ();
		virtual ~TestJobResult ();
		void testMethodsRegistration(void);
		virtual void setUp(void);
		virtual void tearDown(void);
	
	protected:
		void testFillHeader( void );
		void testInsertData (void );
		
		JobResult* mainJobResult;
};

TestJobResult::TestJobResult() : svutTestCase() {
}

TestJobResult::~TestJobResult() {
}

void TestJobResult::testMethodsRegistration(void){
	setTestCaseName("TestJobResult");
	SVUT_REG_TEST_METHOD(testFillHeader);
	SVUT_REG_TEST_METHOD(testInsertData);
}

void TestJobResult::setUp(void) {
	mainJobResult = new JobResult;
}

void TestJobResult::tearDown(void){
	delete mainJobResult;
}

void TestJobResult::testFillHeader ( void ) {
	mainJobResult->fillHeader("test1", 1, 0, 1.0, 123456789.23);
	SVUT_ASSERT_EQUAL(mainJobResult->getFinalRC(), 0);
	SVUT_ASSERT_EQUAL(mainJobResult->getHashName(), hash_fn("test1"));
	SVUT_ASSERT_EQUAL(mainJobResult->getMagik(), DEFAULT_MAGIK_NUMBER);
	SVUT_ASSERT_EQUAL(mainJobResult->getStartTime(),123456789.23);
	SVUT_ASSERT_EQUAL(mainJobResult->getTime(), 1.0);
	SVUT_ASSERT_EQUAL(mainJobResult->getId(), (size_t)1);
}

void TestJobResult::testInsertData ( void ) {
	string chain = "This is an input to store into the job result and in order to test the JobResult";
	mainJobResult->insertData(chain);
	SVUT_ASSERT_EQUAL(mainJobResult->getData(),chain);
	SVUT_ASSERT_EQUAL(mainJobResult->getSizeData(), chain.size());
	SVUT_ASSERT_EQUAL(mainJobResult->getHashData(), hash_fn(chain));
}


SVUT_REGISTER_STANDELONE(TestJobResult)
