#ifndef VERIFY_HH
#define VERIFY_HH
//---------------------------------------------------------------------------
//
// VERIFY.hh -- Run-time error checking tools.
//
// VERIFY, VERIFY2 macros:
// The VERIFY and VERIFY2 macros are designed to verify runtime input (file, user,
// other source, etc).
// Do not use them to check pre and post conditions, or assertions in code, as these
// macros will always run.  Instead, use the provided macros in DBC.hh for any design
// by contract functionality.

// Behavior:
// When VERIFY or VERIFY2 fails, a VERIFYError exception will be thrown which can be caught.
//
// Note: If exceptions are disabled (NO_EXCEPTIONS defined), VERIFY and VERIFY2
// will fall back to the design by contract macro DBC_ASSERTION. 
//---------------------------------------------------------------------------
#include <iostream>
#include <string>
#include <sstream>

#ifdef TAU_INSTR_DBC
#include "TAU.h"

extern TauGroup_t var_tau_gr;
extern FunctionInfo *var_fi;
FunctionInfo* get_FI(char* name,char* filename,int lineNumber);
#endif

// Exception-driven VERIFY.
#ifndef NO_EXCEPTIONS
#include <exception>
namespace dbc
{

// Exception thrown if VERIFY fails
class VERIFYError: public std::exception {
   public:

   // Constructor
   VERIFYError (const char* text);

   // Destructor
   ~VERIFYError() throw() {}

   // Description of the exception.
   const char* what() const throw() {
      return mReason.c_str();
   } // end what

   private:

   // Explanation of the failure.
   std::string mReason;
};

} // end namespace dbc

// And now.... the macros!

#ifndef TAU_INSTR_DBC  // don't TAU instrument the VERIFY macro

#define VERIFY2(x, msg) \
   if (!(x)) { \
      std::ostringstream s; \
      s << "Verification failed: " << msg << std::endl; \
      s << "...at line " << __LINE__ << \
         " of file " << __FILE__ << "." << std::ends;\
      dbc::VERIFYError reason(s.str().c_str());\
      throw reason;\
   }

#else  // TAU instrument the VERIFY macro

#define VERIFY2(x, msg) \
    { var_fi = get_FI("VERIFY",__FILE__,__LINE__);                  \
    if ( var_tau_gr & RtsLayer::TheProfileMask())                       \
        Tau_start_timer(var_fi, 0); \
    if (!(x)) {                            \
        std::ostringstream s;                           \
        s << "Verification failed: " << msg << std::endl; \
        s << "...at line " << __LINE__ <<                 \
            " of file " << __FILE__ << "." << std::ends;  \
        dbc::VERIFYError reason(s.str().c_str());         \
        throw reason;                                     \
    }                                                     \
    Tau_stop_timer(var_fi);                               \
    }

#endif

#define VERIFY(x) VERIFY2(x, #x)

// VERIFY sans exceptions.
#else
#include "utilities/DBC.hh"
#define VERIFY2(x, msg) DBC_ASSERTION(x, msg, "Verification failed");
#define VERIFY(x) VERIFY2(x, #x)
#endif

#endif // VERIFY_HH
