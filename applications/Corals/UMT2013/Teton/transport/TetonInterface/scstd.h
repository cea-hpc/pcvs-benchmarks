/*
 * fia.h - fortran interface
 */


#ifndef __FIA_H__
#define __FIA_H__


/* From scstd.h - an attempt to expand the interface to support f90 modules.
 *  Failed - sun mangles module procdure names as procedure.in.module_
 *  The .'s cause the C compiler grief.
 *
 * also simplified by using cpp token cating.
 * (i.e the trailing underscore is not explicit in the macro)
 */
/*--------------------------------------------------------------------------*/

/* F77_ID - attempt a uniform naming of FORTRAN 90 functions which 
 *        - gets around loader naming conventions
 *        -
 *        - F77_ID(foo_, foo, FOO)(x, y, z)
 */

/*--------------------------------------------------------------------------*/

#if (defined AIX) || (defined __blrts__) || (defined BGQ)
# define F77_ID(x_, x, X)  x
# define FCDTOCP(x)      ((char *) x)
# define CPTOFCD(x, len) (x)
# define FCDLENARG(var)  ,FIXNUM var
# define FCDLENDEF(var, x)
# define CPLENARG(var) ,(var)

typedef float F77_REAL4;
#define F77_REAL4_S "float"

typedef double F77_REAL8;
#define F77_REAL8_S "double"

#define F77_REAL16_S "unknown"

typedef float F77_REAL;
#define F77_REAL_S "float"

typedef signed char F77_INT1;
#define F77_INT1_S "signed char"

typedef short F77_INT2;
#define F77_INT2_S "short"

typedef int F77_INT4;
#define F77_INT4_S "int"

#define F77_INT8_S "unknown"

typedef int F77_INTEGER;
#define F77_INTEGER_S "int"

typedef unsigned int F77_LOGICAL;
#define F77_LOGICAL_S "int"

typedef struct _s_f90_complex4 {
  float real;
  float imag;
} F77_COMPLEX4;

typedef struct _s_f90_complex4 F77_COMPLEX;
/*
typedef struct _s_f90_complex4 F77_COMPLEX4;
*/

typedef struct _s_f90_complex8 {
  double real;
  double imag;
} F77_COMPLEX8;

/*
typedef struct _s_f90_complex8 F77_COMPLEX8;
*/


/*--------------------------------------------------------------------------*/

#elif (defined __alpha)
# define F77_ID(x_, x, X)  x ## _
# define FCDTOCP(x)      ((char *) x)
# define CPTOFCD(x, len) (x)
# define FCDLENARG(var)  ,FIXNUM var
# define FCDLENDEF(var, x)
# define CPLENARG(var) ,(var)

typedef float F77_REAL4;
#define F77_REAL4_S "float"

typedef double F77_REAL8;
#define F77_REAL8_S "double"

#define F77_REAL16_S "unknown"

typedef float F77_REAL;
#define F77_REAL_S "float"

typedef signed char F77_INT1;
#define F77_INT1_S "signed char"

typedef short F77_INT2;
#define F77_INT2_S "short"

typedef int F77_INT4;
#define F77_INT4_S "int"

typedef long F77_INT8;
#define F77_INT8_S "long"

typedef int F77_INTEGER;
#define F77_INTEGER_S "int"

typedef unsigned F77_LOGICAL;
#define F77_LOGICAL_S "int"

typedef struct _s_f90_complex4 {
  float real;
  float imag;
} F77_COMPLEX4;

typedef struct _s_f90_complex4 F77_COMPLEX;
/*
typedef struct _s_f90_complex4 F77_COMPLEX4;
*/

typedef struct _s_f90_complex8 {
  double real;
  double imag;
} F77_COMPLEX8;

/*
typedef struct _s_f90_complex8 F77_COMPLEX8;
*/



/*--------------------------------------------------------------------------*/

#elif (defined __linux)
# define F77_ID(x_, x, X)  x ## _
# define FCDTOCP(x)      ((char *) x)
# define CPTOFCD(x, len) (x)
# define FCDLENARG(var)  ,FIXNUM var
# define FCDLENDEF(var, x)
# define CPLENARG(var) ,(var)

typedef float F77_REAL4;
#define F77_REAL4_S "float"

typedef double F77_REAL8;
#define F77_REAL8_S "double"

#define F77_REAL16_S "unknown"

typedef float F77_REAL;
#define F77_REAL_S "float"

typedef signed char F77_INT1;
#define F77_INT1_S "signed char"

typedef short F77_INT2;
#define F77_INT2_S "short"

typedef int F77_INT4;
#define F77_INT4_S "int"

typedef long F77_INT8;
#define F77_INT8_S "long"

typedef int F77_INTEGER;
#define F77_INTEGER_S "int"

typedef unsigned int F77_LOGICAL;
#define F77_LOGICAL_S "int"


/*--------------------------------------------------------------------------*/

#elif (defined __sun)
# define F77_ID(x_, x, X)  x ## _
# define FCDTOCP(x)      ((char *) x)
# define CPTOFCD(x, len) (x)
# define FCDLENARG(var)  ,FIXNUM var
# define FCDLENDEF(var, x)
# define CPLENARG(var) ,(var)

/*--------------------------------------------------------------------------*/

#elif (defined UNICOS)
# define F77_ID(x_, x, X)  X
# define FCDTOCP(x)      (_fcdtocp(x))
# define CPTOFCD(x, len) (_cptofcd(x, len))
# define FCDLENARG(var)  
# define FCDLENDEF(var, x)  FIXNUM var = _fcdlen(x);
# define CPLENARG(var)
#endif

/*--------------------------------------------------------------------------*/

#endif /* __FIA__ */
