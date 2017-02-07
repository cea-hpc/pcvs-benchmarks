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
#include <Job.h>

using namespace std;
using namespace svUnitTest;

class TestJob : public svutTestCase {
	public:
		TestJob ();
		virtual ~TestJob ();
		void testMethodsRegistration(void);
		virtual void setUp(void);
		virtual void tearDown(void);
	
	protected:
		void testIsDepInvalid( void );
		void testAddConstraint(void);
		void testAddDep(void);
		void testUpdateStatus(void);
		void testAddResult(void);
		
		Job* mainJob;
};

TestJob::TestJob() : svutTestCase() {
}

TestJob::~TestJob() {
}

void TestJob::testMethodsRegistration(void){
	setTestCaseName("TestJob");
	SVUT_REG_TEST_METHOD(testIsDepInvalid);
	SVUT_REG_TEST_METHOD(testAddConstraint);
	SVUT_REG_TEST_METHOD(testAddDep);
	SVUT_REG_TEST_METHOD(testUpdateStatus);
	SVUT_REG_TEST_METHOD(testAddResult);
}

void TestJob::setUp(void) {
	mainJob = new Job;
}

void TestJob::tearDown(void){
	delete mainJob;
}

void TestJob::testAddConstraint ( void ) {
	JobConstraint* job = new JobConstraint("Constraint0");
	mainJob->addConstraint(job);
	SVUT_ASSERT_EQUAL(mainJob->getConstraints()[0], job);
}

void TestJob::testAddDep ( void ) {
	Job* jobDep = new Job;
	mainJob->addDependency(jobDep);
	SVUT_ASSERT_EQUAL(mainJob->getDeps()[0], jobDep);
}

void TestJob::testIsDepInvalid ( void ) {
	Job* jobDep = new Job;
	mainJob->addDependency(jobDep);
	jobDep->updateStatus(FAILED);
	SVUT_ASSERT_TRUE(mainJob->isDepInvalid());

	jobDep->updateStatus(INVALID_DEPS);
	SVUT_ASSERT_TRUE(mainJob->isDepInvalid());

	jobDep->updateStatus(NOT_RUNNABLE);
	SVUT_ASSERT_TRUE(mainJob->isDepInvalid());

	jobDep->updateStatus(MUCH_TRIES);
	SVUT_ASSERT_TRUE(mainJob->isDepInvalid());

	jobDep->updateStatus(PASSED);
	SVUT_ASSERT_FALSE(mainJob->isDepInvalid());
}

void TestJob::testAddResult(void)
{
	string chain = "I'm an test output";
	mainJob->addResult(0, 10.0, 123456789.20, chain);
	SVUT_ASSERT_EQUAL(mainJob->getResult().getData(), chain);
	SVUT_ASSERT_EQUAL(mainJob->getResult().getFinalRC(), 0);
	SVUT_ASSERT_EQUAL(mainJob->getResult().getTime(), 10.0);
	SVUT_ASSERT_EQUAL(mainJob->getResult().getStartTime(), 123456789.20 );
}

void TestJob::testUpdateStatus(void)
{
	SVUT_ASSERT_EQUAL(mainJob->getStatus(), NOT_RUN);
	mainJob->updateStatus(FAILED);
	SVUT_ASSERT_EQUAL(mainJob->getStatus(), FAILED);
	mainJob->updateStatus(INVALID_DEPS);
	SVUT_ASSERT_EQUAL(mainJob->getStatus(), INVALID_DEPS);
	mainJob->updateStatus(NOT_RUNNABLE);
	SVUT_ASSERT_EQUAL(mainJob->getStatus(), NOT_RUNNABLE);
	mainJob->updateStatus(MUCH_TRIES);
	SVUT_ASSERT_EQUAL(mainJob->getStatus(), MUCH_TRIES);
	mainJob->updateStatus(PASSED);
	SVUT_ASSERT_EQUAL(mainJob->getStatus(), PASSED);
}


SVUT_REGISTER_STANDELONE(TestJob);
