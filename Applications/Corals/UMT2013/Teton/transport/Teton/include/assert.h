!  Assertion checking include file for TETON

#ifdef ASSERT
#define require(bool,s) call assert(bool,__FILE__,__LINE__,s)
#define ensure(bool,s) call assert(bool,__FILE__,__LINE__,s)
#define check(bool,s) call assert(bool,__FILE__,__LINE__,s)
#else
#define require(bool,s)
#define ensure(bool,s)
#define check(bool,s)
#endif
