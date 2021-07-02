# -*- mode: shell-script-mode -*-
# Process this file with autoconf to produce a configure script.

AC_PREREQ([2.69])
AC_INIT([rma-mt-benchmarks], [1.0], [])
AC_CONFIG_SRCDIR([src/rmamt_options.c])
AC_CONFIG_HEADERS([src/config.h])
AC_CHECK_PROG(MPICC, [mpicc], [mpicc], [none])
AC_CONFIG_MACRO_DIR([m4])

if ( test -n "$XTOS_VERSION" || test -n "$CRAYPE_DIR" ) && (test -z $CC || test "$CC" = cc) ; then
    # Work around Cray idiosyncrasies
    # Don't let the Cray wrapper bring in extra crap
    PE_VERSION=`echo $XTOS_VERSION | sed 's/\..*//g'`
    # force the cray wrappers to act in a predictable way
    if test x$PE_VERSION = x4 ; then
        CC="cc -target=native -dynamic"
        CXX="CC -target=native -dynamic"
    else
        CC="cc --cray-bypass-pkgconfig -dynamic"
        CXX="CC --cray-bypass-pkgconfig -dynamic"
    fi
    # We may be cross-compiling so trick configure
    cross_compiling=yes
    if test ! -n "$LD" ; then
        LD=ld
    fi

    AC_MSG_CHECKING([for craypich])
    if test -n "$CRAY_MPICH2_DIR" ; then
        AC_MSG_RESULT([yes])
        CPPFLAGS="$CPPFLAGS -I$CRAY_MPICH2_DIR/include"
        LDFLAGS="$LDFLAGS -L$CRAY_MPICH2_DIR/lib"
        LIBS="$LIBS -lmpich"
    else
        AC_MSG_RESULT([no])
    fi
elif test ${MPICC} = "mpicc" ; then
    CC=${MPICC}
    CXX=${MPICXX}
fi

AC_GNU_SOURCE

AM_INIT_AUTOMAKE([serial-tests])
LT_INIT


AX_PTHREAD([],[AC_ERROR("The RMA-MT benchmarks require pthreads")])

RMAMT_CHECK_CVERSION

# Checks for libraries.
AC_CHECK_LIB([hwloc], [hwloc_topology_init])

# Checks for header files.
AC_CHECK_HEADERS([fcntl.h inttypes.h mach/mach.h string.h strings.h unistd.h])

# Checks for library functions.
AC_FUNC_MMAP

AC_CHECK_FUNC([clock_gettime], [], [AC_ERROR([RMA-MT requires the clock_gettime function])])

AC_CONFIG_FILES([Makefile src/Makefile scripts/Makefile])
AC_OUTPUT
