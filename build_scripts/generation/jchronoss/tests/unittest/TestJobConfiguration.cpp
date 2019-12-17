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
#include <utils.h>
#include <ConfigurationJob.h>

using namespace std;
using namespace svUnitTest;

class TestJobConfiguration : public svutTestCase {
	public:
		void testMethodsRegistration(void);
		virtual void setUp(void);
		virtual void tearDown(void);
	protected:
		void testAddFiles(void);
		void testAddJobsFiles ( void );
		JobConfiguration* mainJobConfiguration;
};

void TestJobConfiguration::testMethodsRegistration(void){
	setTestCaseName("TestJobConfiguration");
	SVUT_REG_TEST_METHOD(testAddFiles);
	SVUT_REG_TEST_METHOD(testAddJobsFiles);
}

void TestJobConfiguration::setUp(void){
	mainJobConfiguration = new JobConfiguration;
}

void TestJobConfiguration::tearDown(void){
	delete mainJobConfiguration;
}

void TestJobConfiguration::testAddFiles ( void ) {
	string chain1 = "file1,file2,file3";
	string chain2 = "file4";
	mainJobConfiguration->addFiles(&chain1,mainJobConfiguration->getJobsFilesNoLock());
	SVUT_ASSERT_EQUAL(mainJobConfiguration->getJobsFilesNoLock().at(0)->toString(), "./file1");
	SVUT_ASSERT_EQUAL(mainJobConfiguration->getJobsFiles().at(1)->toString(), "./file2");
	SVUT_ASSERT_EQUAL(mainJobConfiguration->getJobsFiles().at(2)->toString(), "./file3");

	mainJobConfiguration->addFiles(&chain2,mainJobConfiguration->getJobsFilesNoLock());
	SVUT_ASSERT_EQUAL(mainJobConfiguration->getJobsFiles()[3]->toString(), "./file4");
	
}

void TestJobConfiguration::testAddJobsFiles ( void ) {
	string chain = "file";
	SVUT_ASSERT_TRUE(mainJobConfiguration->getJobsFiles().size() == 0);
	mainJobConfiguration->addFiles(&chain,mainJobConfiguration->getJobsFilesNoLock());
	SVUT_ASSERT_EQUAL(mainJobConfiguration->getJobsFilesNoLock().at(0)->toString(), "./file");
	SVUT_ASSERT_TRUE(mainJobConfiguration->getJobsFiles().size() == 1);	
}

SVUT_REGISTER_STANDELONE(TestJobConfiguration)
