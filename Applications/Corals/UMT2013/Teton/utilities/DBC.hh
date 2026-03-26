#ifndef DBC_HH
#define DBC_HH
//---------------------------------------------------------------------------
//
// DBC.hh -- Design by Contract tools.
//
//---------------------------------------------------------------------------
#include <iostream>
#include <string>
#include <cmath>
#include "utilities/Process.hh"
#include "utilities/VERIFY.hh"

#include <sstream>

#ifdef TAU_INSTR_DBC

#include "TAU.h"

extern TauGroup_t var_tau_gr;
extern FunctionInfo *var_fi;
FunctionInfo* get_FI(char* name,char* filename,int lineNumber);

#endif

#ifndef DBC_FUNCTIONS_HH
#define DBC_FUNCTIONS_HH

// This a hack to keep people from using the C "abs" function which
// gives wrong answer when the argument is a floating point type.
// We don't have to do this if we're using g++; it doesn't have this problem.
#ifndef __GNUG__
template <class T>
int abs(T) { should use std::abs instead !!!! }
#endif

namespace dbc
{

// Lock for assertions
bool assertionLock (void);
void assertionUnLock(void);

// nearlyEqual returns true if x and y approximately equal
template<typename T>
inline bool nearlyEqual(const T& x, 
                 const T& y, 
                 double relativeTolerance = 1.0e-5,
                 double absoluteTolerance = 1.0e-12)
{
   using namespace std;
   T ref = max(abs((double)x), abs((double)y));  // g++ 4.3 error with abs(int) as being ambiguous, type cast for now
   return (abs((double)x-y) <=
           (ref * relativeTolerance + absoluteTolerance)
           );
}
} // namespace dbc

#endif // DBC_FUNCTIONS_HH

//----------------------------------------------------------------------------
//                         Clear any existing DBC compile flags.
//----------------------------------------------------------------------------

#ifdef DBC_USE_REQUIRE
#undef DBC_USE_REQUIRE
#endif

#ifdef DBC_USE_ENSURE
#undef DBC_USE_ENSURE
#endif

#ifdef DBC_USE_SCOPES
#undef DBC_USE_SCOPES
#endif

#ifdef DBC_USE_EXPENSIVE_SCOPES
#undef DBC_USE_EXPENSIVE_SCOPES
#endif

#ifdef DBC_USE_EXPENSIVE
#undef DBC_USE_EXPENSIVE
#endif

//----------------------------------------------------------------------------
//                         Work out the compilation modes.
//----------------------------------------------------------------------------

#ifdef DBC_COMPILE_EXPENSIVE
#define DBC_USE_REQUIRE
#define DBC_USE_ENSURE
#define DBC_USE_SCOPES
#define DBC_USE_EXPENSIVE
#define DBC_USE_EXPENSIVE_SCOPES
#endif

#ifdef DBC_COMPILE_ALL
#define DBC_USE_REQUIRE
#define DBC_USE_ENSURE
#define DBC_USE_SCOPES
#endif

#ifdef DBC_COMPILE_PRE
#define DBC_USE_REQUIRE
#define DBC_USE_SCOPES
#endif


//----------------------------------------------------------------------------
//                            REQUIRE -- Preconditions
//----------------------------------------------------------------------------


#ifndef TAU_INSTR_DBC   // normal use assertion definitions

#ifdef DBC_USE_REQUIRE
#define DBC_ASSERTION(x, msg, kind) \
if (dbc::assertionLock()) {\
   if (!(x)) { \
      std::ostringstream s; \
      s << kind << ": " << msg << std::endl; \
      s << "...at line " << __LINE__ << \
         " of file " << __FILE__ << "." << std::ends;\
      Process::haltAll(s.str());\
   }\
   dbc::assertionUnLock(); \
}
#define REQUIRE2(x, msg) DBC_ASSERTION(x, msg, "Precondition violated")
#define ASSERT2(x, msg) DBC_ASSERTION(x, msg, "Assertion violated")
#else
#define ASSERT2(x, msg)
#define REQUIRE2(x, msg)
#endif

#else  // TAU instrumented versions 

#ifdef DBC_USE_REQUIRE
#define DBC_ASSERTION(x, msg, kind)\
if (dbc::assertionLock()) {\
var_fi = get_FI("ASSERT/REQUIRE",__FILE__,__LINE__);              \
    if ( var_tau_gr & RtsLayer::TheProfileMask())                       \
        Tau_start_timer(var_fi, 0);                                     \
    if (!(x)) {                                                         \
        std::ostringstream s;                                           \
        s << kind << ": " << msg << std::endl;                          \
        s << "...at line " << __LINE__ <<                               \
            " of file " << __FILE__ << "." << std::ends;                \
        Process::haltAll(s.str());                                      \
    }                                                                   \
    Tau_stop_timer(var_fi);                                             \
    dbc::assertionUnLock();                                             \
}
#define REQUIRE2(x, msg) DBC_ASSERTION(x, msg, "Precondition violated")
#define ASSERT2(x, msg) DBC_ASSERTION(x, msg, "Assertion violated")
#else
#define ASSERT2(x, msg)
#define REQUIRE2(x, msg)
#endif

#endif
//----------------------------------------------------------------------------
//                            ENSURE -- Postconditions and Invariants
//----------------------------------------------------------------------------

//----------------------------------------------------------------------------
#ifdef DBC_USE_ENSURE
#define ENSURE2(x, msg) DBC_ASSERTION(x, msg,"Postcondition violated")
#define INVARIANT2(x, msg) DBC_ASSERTION(x, msg, "Invariant violated")
#else
#define ENSURE2(x, msg)
#define INVARIANT2(x, msg)
#endif
//----------------------------------------------------------------------------
//                               Contract scopes
// Use these to declare variables that are only used by contracts.
//----------------------------------------------------------------------------
#ifdef DBC_USE_SCOPES
#define BEGIN_CONTRACT_SCOPE 
#define END_CONTRACT_SCOPE 
#else
#define BEGIN_CONTRACT_SCOPE if (false) { while(false)
#define END_CONTRACT_SCOPE } while(false)
#endif

//----------------------------------------------------------------------------
//                               Expensive scopes.
//----------------------------------------------------------------------------
#ifdef DBC_USE_EXPENSIVE_SCOPES
#define BEGIN_EXPENSIVE_CONTRACT_SCOPE 
#define END_EXPENSIVE_CONTRACT_SCOPE 
#else
#define BEGIN_EXPENSIVE_CONTRACT_SCOPE if (false) { while(false)
#define END_EXPENSIVE_CONTRACT_SCOPE } while(false)
#endif
//----------------------------------------------------------------------------

//----------- Define one-argument forms
#ifdef ASSERT
#undef ASSERT
#endif
#define ASSERT(x) ASSERT2(x, #x)
#define REQUIRE(x) REQUIRE2(x, #x)
#define ENSURE(x) ENSURE2(x, #x)
#define INVARIANT(x) INVARIANT2(x, #x)

//----------- Define alternate forms
#define CHECK(x) ASSERT(x)
#define CHECK2(x, msg) ASSERT2(x, msg)

//----------- Define expensive forms.
#ifdef DBC_USE_EXPENSIVE
#define EXPENSIVE_REQUIRE(x) REQUIRE(x)
#define EXPENSIVE_REQUIRE2(x, msg) REQUIRE2(x, msg)
#define EXPENSIVE_ENSURE(x) ENSURE(x)
#define EXPENSIVE_ENSURE2(x, msg) ENSURE2(x, msg)
#define EXPENSIVE_ASSERT(x) ASSERT(x)
#define EXPENSIVE_ASSERT2(x, msg) ASSERT2(x, msg)
#define EXPENSIVE_CHECK(x) CHECK(x)
#define EXPENSIVE_CHECK2(x, msg) CHECK2(x, msg)
#define EXPENSIVE_INVARIANT(x) INVARIANT(x)
#define EXPENSIVE_INVARIANT2(x, msg) INVARIANT2(x, msg)
#else
#define EXPENSIVE_REQUIRE(x)
#define EXPENSIVE_REQUIRE2(x, msg) 
#define EXPENSIVE_ENSURE(x) 
#define EXPENSIVE_ENSURE2(x, msg) 
#define EXPENSIVE_ASSERT(x) 
#define EXPENSIVE_ASSERT2(x, msg) 
#define EXPENSIVE_CHECK(x) 
#define EXPENSIVE_CHECK2(x, msg) 
#define EXPENSIVE_INVARIANT(x) 
#define EXPENSIVE_INVARIANT2(x, msg) 
#endif


#define CONTRACT_VAR(x) if (0 && &x == &x);

#endif // DBC_HH
