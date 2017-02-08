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
#include <JobConstraint.h>

using namespace std;
using namespace svUnitTest;

class TestJobConstraint : public svutTestCase {
	public:
		TestJobConstraint ();
		virtual ~TestJobConstraint ();
		void testMethodsRegistration(void);
		virtual void setUp(void);
		virtual void tearDown(void);
	
	protected:
		void testConstructWithName( void );
		void testIsBelongingTo (void );
		void testEqual(void);
};

TestJobConstraint::TestJobConstraint() : svutTestCase() {
}

TestJobConstraint::~TestJobConstraint() {
}

void TestJobConstraint::testMethodsRegistration(void){
	setTestCaseName("TestJobConstraint");
	SVUT_REG_TEST_METHOD(testConstructWithName);
	SVUT_REG_TEST_METHOD(testIsBelongingTo);
	SVUT_REG_TEST_METHOD(testEqual);
}

void TestJobConstraint::setUp(void) {
}

void TestJobConstraint::tearDown(void){
}

void TestJobConstraint::testConstructWithName ( void ) {
	JobConstraint job("JobConstraint1");
	SVUT_ASSERT_EQUAL(job.getName(), "JobConstraint1");
}

void TestJobConstraint::testIsBelongingTo ( void ) {
	JobConstraint job("JobConstraint4");
	SVUT_ASSERT_TRUE(job.isBelongingTo("JobConstraint4"));
	SVUT_ASSERT_FALSE(job.isBelongingTo("JobConstraint5"));
}

void TestJobConstraint::testEqual( void ){
	SVUT_ASSERT_TRUE(JobConstraint("AJobConstraint") == JobConstraint("AJobConstraint"));
}

SVUT_REGISTER_STANDELONE(TestJobConstraint)
