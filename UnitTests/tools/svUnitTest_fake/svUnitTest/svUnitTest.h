/*****************************************************
             PROJECT  : svUnitTest
             VERSION  : 0.3.0
             DATE     : 05/2011
             AUTHOR   : Valat Sébastien
             LICENSE  : CeCILL-C
*****************************************************/

#ifndef FAKE_SVUNITTEST_HEADER
#define FAKE_SVUNITTEST_HEADER

/********************  MACRO  ***********************/
/** Define the library version. **/
#define SVUT_LIBARY_VERSION "0.3.0"
/** Define that we are using the implementation of the library. **/
#define SVUT_FAKE_LIBRARY

/********************  HEADERS  *********************/
//include "svutTestCase.h"
//include "svutAssert.h"
//include "svutFlatTestCase.h"
//include "svutDefaultMain.h"

#endif

/*****************************************************
             PROJECT  : svUnitTest
             VERSION  : 0.3.0
             DATE     : 05/2011
             AUTHOR   : Valat Sébastien
             LICENSE  : CeCILL-C
*****************************************************/

#ifndef SVUT_EX_ASSERT_HEADER
#define SVUT_EX_ASSERT_HEADER

/********************  HEADERS  *********************/
#include <string>

/********************  NAMESPACE  *******************/
namespace svUnitTest
{

/********************  MACROS  **********************/
#define SVUT_STATUS_TODO "TODO"
#define SVUT_STATUS_INDEV "INDEV"
#define SVUT_STATUS_SKIPED "SKIPED"
#define SVUT_STATUS_SUCCESS "SUCCESS"
#define SVUT_STATUS_FAILED "FAILED"
#define SVUT_STATUS_UNKNOWN "UNKNOWN"

/********************** TYPEDEF *********************/
typedef std::string svutCodeLocation;
typedef std::string svutStatus;

/*********************  CLASS  **********************/
class svutExAssertFake
{
	public:
		svutExAssertFake(svutStatus status,std::string message,svutCodeLocation location)  throw()
		{
			this->status = status;
			this->message = message;
			this->location = location;
		}
		svutExAssertFake(void) {}
		svutStatus status;
		std::string message;
		svutCodeLocation location;
};

/*********************  CLASS  **********************/
class svutExTestStatus : public svutExAssertFake
{
	public:
		svutExTestStatus(void) {}
		svutExTestStatus(std::string name,svutStatus status,svutCodeLocation  location,std::string message="")  throw()
		{
			this->location = location;
			this->message = message;
		}
};

/*********************  CLASS  **********************/
class svutExAssertFail : public svutExTestStatus
{
	public:
		svutExAssertFail(std::string name, svutStatus status, const svutCodeLocation & location, std::string message) throw()
			: svutExTestStatus("AssertFail:"+name, status, location, message)
		{
		}
};

/*********************  CLASS  **********************/
class svutExAssertFailBool : public svutExAssertFail
{
	public:
		svutExAssertFailBool(bool expected, const svutCodeLocation &  location) throw()
			: svutExAssertFail("AssertBool",SVUT_STATUS_FAILED,location,"Failed on BOOLEAN test.")
		{
		}
};

/*********************  CLASS  **********************/
class svutExAssertFailNullPointer : public svutExAssertFail
{
	public:
		svutExAssertFailNullPointer(bool expectNull, svutCodeLocation  location) throw()
			: svutExAssertFail("AssertNull",SVUT_STATUS_FAILED,location,"Failed on NULL pointer test.")
		{
			
		}
};

/*********************  CLASS  **********************/
class svutExAssertFailEqual : public svutExAssertFail
{
	public:
		svutExAssertFailEqual(bool expectTestRes,std::string exptected,std::string actual,
			svutCodeLocation  location) throw()
			: svutExAssertFail("AssertEqual",SVUT_STATUS_FAILED,location,"Failed on expected value.")
		{
		}
};

/*********************  CLASS  **********************/
class svutExAssertFailCustom : public svutExAssertFail
{
	public:
		svutExAssertFailCustom(std::string message, svUnitTest::svutCodeLocation location) throw()
			: svutExAssertFail("AssertCustom",SVUT_STATUS_FAILED,location,message)
		{
		}
};

/*********************  CLASS  **********************/
class svutExAssertFailNotExec : public svutExAssertFail
{
	public:
		svutExAssertFailNotExec(svutCodeLocation  location) throw()
			:svutExAssertFail("AssertNotExec",SVUT_STATUS_FAILED,location,"Failed on execution of forbidden bloc.")
		{
			
		}
};

/*********************  CLASS  **********************/
class svutExAssertFailThrow : public svutExAssertFail
{
	public:
		svutExAssertFailThrow(std::string expected,std::string actual,
			svutCodeLocation  location) throw()
			: svutExAssertFail("AssertThrow",SVUT_STATUS_FAILED,location,"Failed on waiting exception.")
		{
			
		}
};

}

#endif

/*****************************************************
             PROJECT  : svUnitTest
             VERSION  : 0.3.0
             DATE     : 05/2011
             AUTHOR   : Valat Sébastien
             LICENSE  : CeCILL-C
*****************************************************/

#ifndef FAKE_SVUT_ASSERTER_HEADER
#define FAKE_SVUT_ASSERTER_HEADER

/********************  HEADERS  *********************/
#include <sstream>
#include <cstring>
//include "svutExAssert.h"

/********************  NAMESPACE  *******************/
namespace svUnitTest
{

/*******************  FUNCTION  *********************/
template <class T1,class T2>
bool asserterOperatorEqual(const T1 & v1,const T2 & v2)
{
	return (v1 == v2);
}

/*******************  FUNCTION  *********************/
template <class T1,class T2>
bool asserterOperatorNotEqual(const T1 & v1,const T2 & v2)
{
	return (v1 != v2);
}

/*******************  FUNCTION  *********************/
template <class T1,class T2>
static bool asserterOperatorGT(const T1 & v1,const T2 & v2)
{
	return (v1 > v2);
}

/*******************  FUNCTION  *********************/
template <class T1,class T2>
static bool asserterOperatorGE(const T1 & v1,const T2 & v2)
{
	return (v1 >= v2);
}

/*******************  FUNCTION  *********************/
template <class T1,class T2>
static bool asserterOperatorLT(const T1 & v1,const T2 & v2)
{
	return (v1 < v2);
}

/*******************  FUNCTION  *********************/
template <class T1,class T2>
static bool asserterOperatorLE(const T1 & v1,const T2 & v2)
{
	return (v1 <= v2);
}

/*******************  FUNCTION  *********************/
template <class T>
std::string asserterToString(const T & value)
{
	std::stringstream res;
	res << value;
	return res.str();
}

/*******************  FUNCTION  *********************/
template <class T>
bool asserterOperatorEqualStrict(const T & expected,const T & actual)
{
	return asserterOperatorEqual(expected,actual);
}

/*******************  FUNCTION  *********************/
template <class T>
bool asserterOperatorNotEqualStrict(const T & expected,const T & actual)
{
	return asserterOperatorNotEqual(expected,actual);
}

/*******************  FUNCTION  *********************/
template <class T>
static bool asserterOperatorEqualZero(const T & value)
{
	return (value == (T)0);
}

/*******************  FUNCTION  *********************/
template <class T>
static bool asserterOperatorEqualZeros(const T * value,size_t size)
{
	for (size_t i = 0 ; i < size ; ++i)
		if (asserterOperatorEqualZero(value[i]) == false)
			return false;
	return true;
}

/*******************  FUNCTION  *********************/
// template <class T>
// static void assertNotEqualStrict(const T & expected,const T & actual,svutCodeLocation location) throw(svutExAssertFailEqual)
// {
// 	assertNotEqual(expected,actual,location);
// }

/*******************  FUNCTION  *********************/
static inline bool asserterOperatorEqual(const char * expected,const char * actual)
{
	return strcmp(expected,actual) == 0;
}

/*******************  FUNCTION  *********************/
static inline bool asserterOperatorNotEqual(const char * expected,const char * actual)
{
	return strcmp(expected,actual) != 0;
}

/*******************  FUNCTION  *********************/
static inline bool asserterOperatorEqualStrict(const char * expected,const char * actual)
{
	return asserterOperatorEqual(expected,actual);
}

/*******************  FUNCTION  *********************/
static inline bool asserterOperatorNotEqualStrict(const char * expected,const char * actual)
{
	return asserterOperatorNotEqual(expected,actual);
}

/*******************  FUNCTION  *********************/
static inline bool asserterOperatorGE(const char * v1,const char * v2)
{
	return v1 >= v2;
}

/*******************  FUNCTION  *********************/
static inline bool asserterOperatorGT(const char * v1,const char * v2)
{
	return v1 > v2;
}

/*******************  FUNCTION  *********************/
static inline bool asserterOperatorLE(const char * v1,const char * v2)
{
	return v1 <= v2;
}

/*******************  FUNCTION  *********************/
static inline bool asserterOperatorLT(const char * v1,const char * v2)
{
	return v1 < v2;
}

/*******************  FUNCTION  *********************/
static inline bool asserterOperatorGE(char * v1,char * v2)
{
	return v1 >= v2;
}

/*******************  FUNCTION  *********************/
static inline bool asserterOperatorGT(char * v1,char * v2)
{
	return v1 > v2;
}

/*******************  FUNCTION  *********************/
static inline bool asserterOperatorLE(char * v1,char * v2)
{
	return v1 <= v2;
}

/*******************  FUNCTION  *********************/
static inline bool asserterOperatorLT(char * v1,char * v2)
{
	return v1 < v2;
}

}

#endif

/*****************************************************
             PROJECT  : svUnitTest
             VERSION  : 0.3.0
             DATE     : 05/2011
             AUTHOR   : Valat Sébastien
             LICENSE  : CeCILL-C
*****************************************************/

#ifndef FAKE_SVUT_ASSERT_HEADER
#define FAKE_SVUT_ASSERT_HEADER

/********************  HEADERS  *********************/
#include <string>
#include <sstream>
//include "svutAsserter.h"

/********************  NAMESPACE  *******************/
namespace svUnitTest
{

/*******************  FUNCTION  *********************/
inline std::string getLocationString(int line,const char * filename,const char * function)
{
	std::stringstream res;
	res << "line " << line << " of file " << filename << " on methode " << function << "()";
	return res.str();
}

/********************  MACRO  ***********************/
#define SVUT_NOT_SUPPORTED_MACRO(x) throw svUnitTest::svutExAssertFake("FAILED","Macro not supported : " #x,SVUT_CODE_LOCATION)

/********************  MACRO  ***********************/
#define SVUT_CODE_LOCATION getLocationString(__LINE__,__FILE__,__FUNCTION__)

/********************  MACRO  ***********************/
#define SVUT_CAPTURE_ASSERT_EXCEPTIONS(x) \
	try{\
		x;\
	} catch (svutExAssertFake & e) {\
		setErrorMessage(e);\
	} catch (...) {\
		svUnitTest::svutExAssertFake e("UNKNOWN","Unknown exception thrown",SVUT_CODE_LOCATION);\
		setErrorMessage(e);\
	}

/********************  MACRO  ***********************/
#define SVUT_ASSERT_TRUE(value) \
	if (((bool)(value)) ==  false) throw svUnitTest::svutExAssertFake("FAILED",("Assert fail : " #value " == true"),SVUT_CODE_LOCATION)

/********************  MACRO  ***********************/
#define SVUT_ASSERT_FALSE(value) \
	if (((bool)(value)) ==  true) throw svUnitTest::svutExAssertFake("FAILED",("Assert fail : " #value " == false"),SVUT_CODE_LOCATION)

/********************  MACRO  ***********************/
#define SVUT_ASSERT_NULL(value)\
	if (((void*)(value)) != NULL) \
		throw svUnitTest::svutExAssertFake("FAILED",("Assert fail : " #value " == NULL"),SVUT_CODE_LOCATION)


/********************  MACRO  ***********************/
#define SVUT_ASSERT_NOT_NULL(value)\
	if (((void*)(value)) == NULL) \
		throw svUnitTest::svutExAssertFake("FAILED",("Assert fail : " #value " != NULL"),SVUT_CODE_LOCATION)

/********************  MACRO  ***********************/
#define SVUT_ASSERT_EQUAL(expected,actual) \
	if (asserterOperatorEqual(expected,actual) == false)\
		throw svUnitTest::svutExAssertFake("FAILED",("Assert fail : " #actual " == " #expected),SVUT_CODE_LOCATION)

/********************  MACRO  ***********************/
#define SVUT_ASSERT_NOT_EQUAL(expected,actual) \
	if (asserterOperatorNotEqual(expected,actual) == false)\
		throw svUnitTest::svutExAssertFake("FAILED",("Assert fail : " #actual " != " #expected),SVUT_CODE_LOCATION)

/********************  MACRO  ***********************/
#define SVUT_ASSERT_ZERO(value) \
	if (asserterOperatorEqualZero((value)) == false)\
		throw svUnitTest::svutExAssertFake("FAILED",("Assert fail : " #value " != 0"),SVUT_CODE_LOCATION)

/********************  MACRO  ***********************/
#define SVUT_ASSERT_ZEROS(value,size) \
	if (asserterOperatorEqualZeros((value),(size)) == false)\
		throw svUnitTest::svutExAssertFake("FAILED",("Assert fail : each of " #value "[" #size "] != 0"),SVUT_CODE_LOCATION)

/********************  MACRO  ***********************/
#define SVUT_ASSERT_EQUAL_STRICT(expected,actual)\
	if (asserterOperatorEqualStrict(expected,actual) == false)\
		throw svUnitTest::svutExAssertFake("FAILED",("Assert fail : " #actual " == " #expected),SVUT_CODE_LOCATION)

/********************  MACRO  ***********************/
#define SVUT_ASSERT_NOT_EQUAL_STRICT(expected,actual)\
	if (asserterOperatorNotEqualStrict(expected,actual) == false)\
		throw svUnitTest::svutExAssertFake("FAILED",("Assert fail : " #actual " != " #expected),SVUT_CODE_LOCATION)

/********************  MACRO  ***********************/
#define SVUT_ASSERT_GT(expectedLimit,actual)\
	if (asserterOperatorGT((actual),(expectedLimit)) == false) \
		throw svUnitTest::svutExAssertFake("FAILED",("Assert fail : " #actual " >= " #expectedLimit),SVUT_CODE_LOCATION)

/********************  MACRO  ***********************/
#define SVUT_ASSERT_GE(expectedLimit,actual)\
	if (asserterOperatorGE((actual),(expectedLimit)) == false) \
		throw svUnitTest::svutExAssertFake("FAILED",("Assert fail : " #actual " >= " #expectedLimit),SVUT_CODE_LOCATION)

/********************  MACRO  ***********************/
#define SVUT_ASSERT_LT(expectedLimit,actual)\
	if (asserterOperatorLT((actual),(expectedLimit)) == false) \
		throw svUnitTest::svutExAssertFake("FAILED",("Assert fail : " #actual " >= " #expectedLimit),SVUT_CODE_LOCATION)

/********************  MACRO  ***********************/
#define SVUT_ASSERT_LE(expectedLimit,actual)\
	if (asserterOperatorLE((actual),(expectedLimit)) == false) \
		throw svUnitTest::svutExAssertFake("FAILED",("Assert fail : " #actual " >= " #expectedLimit),SVUT_CODE_LOCATION)

/********************  MACRO  ***********************/
#define SVUT_ASSERT_SAME(expected,actual)\
	if ((void*)(expected) != (void*)(actual))\
		throw svUnitTest::svutExAssertFake("FAILED",("Assert fail : " #actual " == " #expected),SVUT_CODE_LOCATION)

/********************  MACRO  ***********************/
#define SVUT_ASSERT_NOT_SAME(expected,actual)\
	if ((void*)(expected) == (void*)(actual))\
		throw svUnitTest::svutExAssertFake("FAILED",("Assert fail : " #actual " != " #expected),SVUT_CODE_LOCATION)

/********************  MACRO  ***********************/
#define SVUT_ASSERT_FAIL(message) \
	throw svUnitTest::svutExAssertFake("FAILED",message,SVUT_CODE_LOCATION)

/********************  MACRO  ***********************/
#define SVUT_ASSERT_TODO(message)\
	throw svUnitTest::svutExAssertFake("TODO",("Todo : " message),SVUT_CODE_LOCATION)

/********************  MACRO  ***********************/
#define SVUT_ASSERT_INDEV(message)\
	throw svUnitTest::svutExAssertFake("INDEV",("Indev : " message),SVUT_CODE_LOCATION)

/********************  MACRO  ***********************/
#define SVUT_ASSERT_NOT_EXEC_THIS()\
	throw svUnitTest::svutExAssertFake("FAILED",("This line must be skiped"),SVUT_CODE_LOCATION)

/********************  MACRO  ***********************/
#define SVUT_ASSERT_THROW(name,what)\
	try {\
		what;\
		throw svUnitTest::svutExAssertFake("FAILED",("Don't get expected " #name " exception"),SVUT_CODE_LOCATION);\
	} catch(name) {\
	}  catch(svUnitTest::svutExAssertFake & e) { \
		throw e; \
	} catch(...) {\
		throw svUnitTest::svutExAssertFake("UNKNOWN",("Get unknown exception instead of expected " #name " exception"),SVUT_CODE_LOCATION);\
	}

/********************  MACRO  ***********************/
#define SVUT_ASSERT_THROW_SOMETHING(what) \
	try {\
		what;\
		throw svUnitTest::svutExAssertFake("FAILED",("Don't get expected exception"),SVUT_CODE_LOCATION);\
	}  catch(svUnitTest::svutExAssertFake & e) { \
		throw e; \
	} catch(...) {\
	}

/********************  MACRO  ***********************/
#define SVUT_ASSERT_NOT_THROW(name,what)\
	try {\
		what;\
	} catch(name) {\
		throw svUnitTest::svutExAssertFake("FAILED",("Get unexpected " #name " exception"),SVUT_CODE_LOCATION);\
	} catch(...) {\
	}

/********************  MACRO  ***********************/
#define SVUT_ASSERT_MAY_NOT_THROW(what) \
	try {\
		what;\
	} catch(...) {\
		throw svUnitTest::svutExAssertFake("FAILED",("Get unexpected unknown exception"),SVUT_CODE_LOCATION);\
	}

}

#endif

/*****************************************************
             PROJECT  : svUnitTest
             VERSION  : 0.3.0
             DATE     : 05/2011
             AUTHOR   : Valat Sébastien
             LICENSE  : CeCILL-C
*****************************************************/

#ifndef FAKE_SVUT_TEST_CASE_HEADER
#define FAKE_SVUT_TEST_CASE_HEADER

/********************  HEADERS  *********************/
#include <string>
#include <vector>
#include <iostream>
#include <cstdlib>
#include <cstdio>
//include "svutAssert.h"

/********************  NAMESPACE  *******************/
namespace svUnitTest
{
	
/********************  MACROS  **********************/
#define SVUT_REG_TEST_METHOD(x) markStartTest(#x); SVUT_CAPTURE_ASSERT_EXCEPTIONS(setUp();x();tearDown();); markStatus()
#define SVUT_REGISTER_TEST_CASE(name)\
	static svUnitTest::svutTestCaseBuilderGeneric<name> ___FAKE_SVUNITTEST_case_##name##___;\
	static int ___FAKE_SVUNITTEST_res_case_registration_of##name##__ = svUnitTest::fakeRegisterTestCase(___FAKE_SVUNITTEST_case_##name##___)

/********************  MACROS  **********************/
#define SVUT_SET_CONTEXT(name,value) /*this->setContextEntry((name),(value))*/

/********************  MACROS  **********************/

#define SVUT_UNSET_CONTEXT(name) /*this->clearContexEntry((name))*/

/********************  MACROS  **********************/
#define SVUT_RESET_CONTEXT() /*this->resetContexEntries()*/

/********************  MACROS  **********************/
//printf
#define SVUT_PRINTF 

/********************  MACROS  **********************/
#define SVUT_COUT std::cout

/********************  MACROS  **********************/
#define SVUT_PUTS(x) /* puts((x)) */

/*********************  CLASS  **********************/
/**
 * This is just a light implementation to fake the full library implementation. This may
 * permit to run a test suite bases on svUnitTest in degraded mode even if the library wasn't
 * present. It will directly use assertion so crash on first error and didn't provide any nice
 * output presentation.
**/
class svutTestCase
{
	public:
		inline svutTestCase(std::string name="Undefined");
		inline svutTestCase(const svutTestCase & testCase);
		inline virtual ~svutTestCase(void);
		inline bool runTestCase(void);
		inline std::string getName(void) const;
		inline void setErrorMessage(svutExAssertFake & e);
	protected:
		inline void markStatus(void);
		inline void markStartTest(std::string name);
		inline virtual void testMethodsRegistration(void);
		inline void setTestCaseName(std::string name);
		inline void MARK_AS_KNOWN_ERROR(std::string message);
	private:
		std::string caseName;
		svutExAssertFake status;
		bool finalRes;
};

/*********************  CLASS  **********************/
class svutTestCaseBuilder
{
	public:
		inline virtual ~svutTestCaseBuilder(void);
		virtual svutTestCase * build(void) = 0;
};

/********************  GLOBALS  **********************/
extern std::vector<svutTestCaseBuilder *> * __fake_svut_test_cases_registry__;

/*********************  CLASS  **********************/
template <class T>
class svutTestCaseBuilderGeneric : public svutTestCaseBuilder
{
	public:
		inline virtual svutTestCase * build(void) { return new T();}
};

/*******************  FUNCTION  *********************/
inline svutTestCase::svutTestCase(std::string name)
{
	this->caseName = name;
}

/*******************  FUNCTION  *********************/
inline svutTestCase::svutTestCase(const svutTestCase & testCase)
{
	std::cerr << "Can't made a copy of svutTestCase, it was forbidden." << std::endl;
	abort();
}

/*******************  FUNCTION  *********************/
inline svutTestCase::~svutTestCase(void)
{
}

/*******************  FUNCTION  *********************/
inline bool svutTestCase::runTestCase(void)
{
	this->finalRes = true;
	this->testMethodsRegistration();
	return finalRes;
}

/*******************  FUNCTION  *********************/
inline void svutTestCase::markStatus(void)
{
	std::cout << '[' << status.status << ']' << std::endl;
	if (status.status != "SUCCESS" && status.message.empty() == false)
	{
		std::cout << "- - - - - - - - - - - - - - - - - - - - - - - - - - - - -" << std::endl;
		std::cout << status.location << std::endl;
		std::cout << status.message << std::endl;
		std::cout << "- - - - - - - - - - - - - - - - - - - - - - - - - - - - -" << std::endl;
	}
	if (status.status != "SUCCESS" && status.status != "TODO")
		finalRes = false;
}

/*******************  FUNCTION  *********************/
inline void svutTestCase::markStartTest(std::string name)
{
	std::cout.fill('.');
	std::cout.width(49);
	std::cout << name;
	status.status = "SUCCESS";
}

/*******************  FUNCTION  *********************/
inline void svutTestCase::testMethodsRegistration(void)
{
}

/*******************  FUNCTION  *********************/
inline void svutTestCase::setTestCaseName(std::string name)
{
	this->caseName = name;
}

/*******************  FUNCTION  *********************/
inline void svutTestCase::MARK_AS_KNOWN_ERROR(std::string message)
{
}

/*******************  FUNCTION  *********************/
inline void svutTestCase::setErrorMessage(svutExAssertFake & e)
{
	this->status = e;
}

/*******************  FUNCTION  *********************/
inline std::string svutTestCase::getName(void) const
{
	return caseName;
}

/*******************  FUNCTION  *********************/
inline svutTestCaseBuilder::~svutTestCaseBuilder(void)
{
}

/*******************  FUNCTION  *********************/
static inline int fakeRegisterTestCase(svUnitTest::svutTestCaseBuilder & builder)
{
	if (__fake_svut_test_cases_registry__ == NULL)
		__fake_svut_test_cases_registry__ = new std::vector<svutTestCaseBuilder *>;
	svUnitTest::__fake_svut_test_cases_registry__->push_back(&builder);
	return 0;
}

}

#endif

/*****************************************************
             PROJECT  : svUnitTest
             VERSION  : 0.3.0
             DATE     : 05/2011
             AUTHOR   : Valat Sébastien
             LICENSE  : CeCILL-C
*****************************************************/

#ifndef SVUT_FLAT_TEST_CASE_HEADER
#define SVUT_FLAT_TEST_CASE_HEADER

/********************  HEADERS  *********************/
//include "svutTestCase.h"
#include <vector>
#include <set>
#include <map>

/********************  NAMESPACE  *******************/
namespace svUnitTest
{

/********************  MACROS  **********************/
/**
 * Convert the given name to string.
 * @param value Define the value to convert to string.
 * @version 0.4.0
**/
#define SVUT_DECLARE_FLAT_TEST_STR(value) #value

/********************  MACROS  **********************/
/**
 * Register a flat test function.
 * @param testCaseName Define the name of virtual test case on which to link the flat test.
 * @param testName Define the name of your test (the name of function to declare).
 * @version 0.4.0
**/
#define SVUT_REGISTER_FLAT_TEST_INTERNAL(testCaseName,testName,method) \
	static bool ___SVUT_res_flat_case_method_registration_of_##testCaseName##_##testName##__ = \
		svUnitTest::registerFlatTestCaseMethod(SVUT_DECLARE_FLAT_TEST_STR(testCaseName),\
			SVUT_DECLARE_FLAT_TEST_STR(testName),method,SVUT_CODE_LOCATION); \

/********************  MACROS  **********************/
/**
 * Register a flat test function.
 * @param testCaseName Define the name of virtual test case on which to link the flat test.
 * @param testName Define the name of your test (the name of function to declare).
 * @version 0.4.0
**/
#define SVUT_REGISTER_FLAT_TEST(testCaseName,testName) SVUT_REGISTER_FLAT_TEST_INTERNAL(testCaseName,testName,testName)

/********************  MACROS  **********************/
/**
 * Declare a flat test function. It will declare a function of type "void name(void)", so place the test
 * body directly after this line. It will automatically declare it and call SVUT_REGISTER_FLAT_TEST
 * @param testCaseName Define the name of virtual test case on which to link the flat test.
 * @param testName Define the name of your test (the name of function to declare).
 * @version 0.4.0
**/
#define SVUT_DECLARE_FLAT_TEST(testCaseName,testName) \
	void testCaseName##_##testName(void);\
	SVUT_REGISTER_FLAT_TEST_INTERNAL(testCaseName,testName,testCaseName##_##testName);\
	void testCaseName##_##testName(void)

/********************  MACROS  **********************/
/**
 * Same than SVUT_REGISTER_FLAT_TEST, but to register the special setUp() function.
**/
#define SVUT_REGISTER_FLAT_SETUP(testCaseName,fctName) \
	SVUT_REGISTER_FLAT_TEST_INTERNAL(testCaseName,__SVUT_SPECIAL_TEST_CASE_SETUP__,fctName)

/********************  MACROS  **********************/
/**
 * Same than SVUT_DECLARE_FLAT_TEST, but to declare the special setUp() function.
**/
#define SVUT_DECLARE_FLAT_SETUP(testCaseName) \
	void testCaseName##_##setUp(void);\
	SVUT_REGISTER_FLAT_TEST_INTERNAL(testCaseName,__SVUT_SPECIAL_TEST_CASE_SETUP__,testCaseName##_##setUp);\
	void testCaseName##_##setUp(void)

/********************  MACROS  **********************/
/**
 * Same than SVUT_REGISTER_FLAT_TEST, but to register the special tearDown() function.
**/
#define SVUT_REGISTER_FLAT_TEAR_DOWN(testCaseName,fctName) \
	SVUT_REGISTER_FLAT_TEST_INTERNAL(testCaseName,__SVUT_SPECIAL_TEST_CASE_TEAR_DOWN__,fctName)

/********************  MACROS  **********************/
/**
 * Same than SVUT_DECLARE_FLAT_TEST, but to declare the special tearDown() function.
**/
#define SVUT_DECLARE_FLAT_TEAR_DOWN(testCaseName) \
	void testCaseName##_##tearDown(void);\
	SVUT_REGISTER_FLAT_TEST_INTERNAL(testCaseName,__SVUT_SPECIAL_TEST_CASE_TEAR_DOWN__,testCaseName##_##tearDown);\
	void testCaseName##_##tearDown(void)

/********************  TYPEDEF  *********************/
/**
 * Define the prototype of methodes for flat test mode (simple C functions).
 * @version 0.4.0
**/
typedef void (*svutTestMethodPtr)(void);

/*********************  STRUCT  *********************/
/**
 * Struct to define registry entry introduced by registerFlatTestCaseMethod() method.
 * @brief Simple structure of stage for flat test case global registry.
 * @version 0.4.0
**/
struct svutFlatRegistryEntry
{
	std::string testCaseName;
	std::string testName;
	svutCodeLocation location;
	svutTestMethodPtr methodPtr;
};

/*********************  CLASS  **********************/
/**
 * Builder to manage the creation of flat test cases by svutRunner if autoloading classes is enabled.
 * @brief Builder for flat test case.
 * @author Valat Sébastien
 * @version 0.4.0
**/
class svutFlatTestCaseBuilder : public svutTestCaseBuilder
{
	public:
	    inline svutFlatTestCaseBuilder(std::string testCaseName);
	    inline virtual svUnitTest::svutTestCase* build(void );
	protected:
		/** Name of flat test case to construct. **/
		std::string testCaseName;
};

/*********************  CLASS  **********************/
/**
 * Special test case used to automatically construct object test case from flat C functions.
 * @brief Test case made of static C functions.
 * @author Valat Sébastien
 * @version 0.4.0
**/
class svutFlatTestCase : public svutTestCase
{
	public:
	    inline svutFlatTestCase(std::string name = "Undefined");
	    inline virtual void testMethodsRegistration(void );
		inline void registerFlatTestMethod(std::string name,svutTestMethodPtr methodPtr,const svutCodeLocation & location);
	protected:
		inline virtual void setUp(void );
		inline virtual void tearDown(void );
	private:
		svutTestMethodPtr setUpPtr;
		svutTestMethodPtr tearDownPtr;
		std::vector<svutFlatRegistryEntry> tests;
};

/*******************  FUNCTION  *********************/
static bool registerFlatTestCaseMethod(const char* testCaseName, const char* functionName, svUnitTest::svutTestMethodPtr methodPtr, const svUnitTest::svutCodeLocation& location);
static std::set<class svutTestCaseBuilder *> getRegistredFlatTestCases(void);

/********************* GLOBALS **********************/
extern std::vector<svutFlatRegistryEntry> * __fake_svut_test_flat_test_registry__;

/*******************  FUNCTION  *********************/
/**
 * Function used to register a new methe by link time trick.
 * @param testCaseName Define the name of test case to create.
 * @param functionName Define the name of the function to use as test case.
 * @param methodPtr Define the pointer of C method to call.
 * @param location Define the location of the method.
 * @return Return always true, this is the trick to call the method at init time without depending
 * on compilers/linker keywords, see SVUT_REGISTER_FLAT_TEST macro.
**/
static inline bool registerFlatTestCaseMethod(const char* testCaseName, const char* functionName, svutTestMethodPtr methodPtr,const svutCodeLocation & location)
{
	if (svUnitTest::__fake_svut_test_flat_test_registry__ == NULL)
		svUnitTest::__fake_svut_test_flat_test_registry__ = new std::vector<svutFlatRegistryEntry>;
	svutFlatRegistryEntry entry;
	entry.testCaseName = testCaseName;
	entry.testName = functionName;
	entry.methodPtr = methodPtr;
	entry.location = location;
	svUnitTest::__fake_svut_test_flat_test_registry__->push_back(entry);
	return true;
}

/*******************  FUNCTION  *********************/
/**
 * Constructor of flat test case.
 * @param name Simply define the name of the flat test case. This name will be used by
 * testMethodsRegistration() to find the related tests.
**/
inline svutFlatTestCase::svutFlatTestCase(std::string name)
	: svutTestCase(name)
{
	setUpPtr = NULL;
	tearDownPtr = NULL;
}

/*******************  FUNCTION  *********************/
/**
 * Register a new test method to the current flat test case.
 * @param name Define the name of the test.
 * @param methodPtr Define the test function to use.
 * @param location Define the location of the function in source code.
**/
inline void svutFlatTestCase::registerFlatTestMethod(std::string name, svutTestMethodPtr methodPtr, const svUnitTest::svutCodeLocation& location)
{
	if (name == "__SVUT_SPECIAL_TEST_CASE_SETUP__")
	{
		this->setUpPtr = methodPtr;
	} else if (name == "__SVUT_SPECIAL_TEST_CASE_TEAR_DOWN__") {
		this->tearDownPtr = methodPtr;
	} else {
		svutFlatRegistryEntry entry;
		entry.location = location;
		entry.methodPtr = methodPtr;
		entry.testCaseName = this->getName();
		entry.testName = name;
		tests.push_back(entry);
	}
}

/*******************  FUNCTION  *********************/
/**
 * Search all corresponding tests by checking testCaseName and create entries in current test case.
**/
inline void svutFlatTestCase::testMethodsRegistration(void )
{
	if (svUnitTest::__fake_svut_test_flat_test_registry__ != NULL)
		for (std::vector<svutFlatRegistryEntry>::const_iterator it = svUnitTest::__fake_svut_test_flat_test_registry__->begin() ; it != svUnitTest::__fake_svut_test_flat_test_registry__->end() ; ++it)
			if (it->testCaseName == getName())
				this->registerFlatTestMethod(it->testName,it->methodPtr,it->location);

	//run all
	for (std::vector<svutFlatRegistryEntry>::iterator it = tests.begin() ; it != tests.end() ; ++it)
	{
		markStartTest(it->testName);
		SVUT_CAPTURE_ASSERT_EXCEPTIONS(setUp();it->methodPtr();tearDown(););
		markStatus();
	}
}

/*******************  FUNCTION  *********************/
/**
 * Call related tearDown method.
**/
inline void svutFlatTestCase::tearDown(void )
{
	if (tearDownPtr != NULL)
		tearDownPtr();
}

/*******************  FUNCTION  *********************/
/**
 * Call related setup method.
**/
inline void svutFlatTestCase::setUp(void )
{
	if (setUpPtr != NULL)
		setUpPtr();
}

/*******************  FUNCTION  *********************/
/**
 * Return the list of test case builder corresponding to all flat test case registred by
 * SVUT_REGISTER_FLAT_TEST macro.
**/
inline static std::set< svutTestCaseBuilder* > getRegistredFlatTestCases(void )
{
	std::set< svutTestCaseBuilder* > res;
	std::map<std::string,bool> filter;

	if (svUnitTest::__fake_svut_test_flat_test_registry__ != NULL)
	{
		for (std::vector<svutFlatRegistryEntry>::const_iterator it = svUnitTest::__fake_svut_test_flat_test_registry__->begin() ; it != svUnitTest::__fake_svut_test_flat_test_registry__->end() ; ++it)
		{
			if (filter.find(it->testCaseName) == filter.end())
			{
				filter[it->testCaseName] = true;
				res.insert(new svutFlatTestCaseBuilder(it->testCaseName));
			}
		}
	}

	return res;
}

/*******************  FUNCTION  *********************/
inline svutFlatTestCaseBuilder::svutFlatTestCaseBuilder(std::string testCaseName)
{
	this->testCaseName = testCaseName;
}

/*******************  FUNCTION  *********************/
inline svutTestCase* svutFlatTestCaseBuilder::build(void )
{
	return new svutFlatTestCase(testCaseName);
}

}

#endif // SVUTFLATTESTCASE_H

/*****************************************************
             PROJECT  : svUnitTest
             VERSION  : 0.3.0
             DATE     : 05/2011
             AUTHOR   : Valat Sébastien
             LICENSE  : CeCILL-C
*****************************************************/

#ifndef SVUT_DEFAULT_MAIN_HEADER
#define SVUT_DEFAULT_MAIN_HEADER

/********************  HEADERS  *********************/
#include <vector>
#include <cstdlib>
#include <iostream>
#include <ios>
//include "svutTestCase.h"
//include "svutFlatTestCase.h"

namespace svUnitTest
{

/********************  MACRO  ***********************/
/** Define all global variables (must be called one time only). **/
#define SVUT_FAKE_DECLARE_GLOBAL_VARS\
	std::vector<svutTestCaseBuilder *> * svUnitTest::__fake_svut_test_cases_registry__ = NULL;\
	std::vector<svutFlatRegistryEntry> * svUnitTest::__fake_svut_test_flat_test_registry__ = NULL;

/********************  MACROS  **********************/
#define SVUT_REGISTER_STANDELONE(className) \
	SVUT_FAKE_DECLARE_GLOBAL_VARS\
	int main(int argc,char * argv[]) { \
		svUnitTest::svutTestCaseBuilderGeneric<className> builder;\
		return uniqueStandeloneMain(argc,argv,builder);\
	}

/********************  MACROS  **********************/
#define SVUT_USE_DEFAULT_MAIN \
	SVUT_FAKE_DECLARE_GLOBAL_VARS\
	int main(int argc,char * argv[]) { \
		return defaultMain(argc,argv); \
	}
	
/********************  GLOBALS  **********************/
extern std::vector<svutTestCaseBuilder *> * __fake_svut_test_cases_registry__;

/*******************  FUNCTION  *********************/
static int defaultMain(int argc,char * argv[])
{
	bool final = true;

	std::set<svUnitTest::svutTestCaseBuilder *> lst = getRegistredFlatTestCases();
	if (__fake_svut_test_cases_registry__ != NULL)
		lst.insert(svUnitTest::__fake_svut_test_cases_registry__->begin(),svUnitTest::__fake_svut_test_cases_registry__->end());
	
	for (std::set<svutTestCaseBuilder *>::iterator it = lst.begin(); it != lst.end() ; ++it)
	{
		svutTestCase * test = (*it)->build();
		std::cout << "--------------";
		std::cout.width(30);
		std::cout.setf(std::ios::left);
		std::cout.fill('-');
		std::cout << test->getName();
		std::cout << "-------------- " << std::endl;
		if (test->runTestCase() == false)
			final = false;
		delete test;
	}

	if (__fake_svut_test_cases_registry__ != NULL)
		delete __fake_svut_test_cases_registry__;

	if (__fake_svut_test_flat_test_registry__ != NULL)
		delete __fake_svut_test_flat_test_registry__;

	if (final)
		return EXIT_SUCCESS;
	else
		return EXIT_FAILURE;
}

/*******************  FUNCTION  *********************/
static inline int uniqueStandeloneMain(int argc,char * argv[],svutTestCaseBuilder & builder)
{
	if (__fake_svut_test_cases_registry__ == NULL)
		__fake_svut_test_cases_registry__ = new std::vector<svutTestCaseBuilder *>;
	svUnitTest::__fake_svut_test_cases_registry__->push_back(&builder);
	return defaultMain(argc,argv);
}

}

#endif //SVUT_DEFAULT_MAIN_HEADER

