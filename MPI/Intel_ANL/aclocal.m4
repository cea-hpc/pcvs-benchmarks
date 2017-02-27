dnl
AC_DEFUN([PAC_COPY_ORIG_BACK],[
for file in \
	mpitestf.h mpitest_cfg.h mpitest_cfgf.h mpitest_def.h ; do
    if test -f include/$file.orig ; then
	mv -f include/$file.orig include/$file
    fi
done
])
dnl
dnl PAC_TRY_COMPILE_MPI(action-if-succeed,action-if-fail)
AC_DEFUN([PAC_TRY_COMPILE_MPI],[
AC_MSG_CHECKING([if can compile and link a simple MPI program with $MPITEST_CC])
if test -z "$MPITEST_CC" ; then
        AC_MSG_RESULT(no)
        ifelse([$2],,,[$2])
        AC_MSG_ERROR([No MPITEST_CC selected])
else
    rm -f conftest*
    cat > conftest.c <<EOF
#include "mpi.h"
   
int main(int argc, char **argv) 
{
    MPI_Init(&argc, &argv);
    MPI_Finalize();
    return 0;
}
EOF
    echo $MPITEST_CC conftest.c -o conftest $MPI_LIB 1>& AC_FD_CC
    $MPITEST_CC conftest.c -o conftest $MPI_LIB 1>& AC_FD_CC 2>& AC_FD_CC
    if test ! -x conftest ; then
        echo "Failed program was " 1>& AC_FD_CC
        cat conftest.c 1>& AC_FD_CC
        AC_MSG_RESULT(no)
        ifelse([$2],,,[$2])
        AC_MSG_ERROR([cannot compile and link a simple MPI program with $MPITEST_CC])
    else
        AC_MSG_RESULT(yes)
        ifelse([$1],,,[$1])
    fi
    rm -f conftest*
fi
])dnl
dnl
dnl PAC_TEST_FORTTYPES tests to see if the following fortran datatypes are
dnl supported: INTEGER1, INTEGER2, INTEGER4, REAL4, REAL8, DOUBLE_COMPLEX
dnl
dnl PAC_PROG_F77_TYPE checks for the given type(type,actioniffound,ifnot)
AC_DEFUN([PAC_PROG_F77_TYPE],[
rm -f conftest*
cat > conftest.f <<EOF
        subroutine conftest
        $1 a
        return 
        end
EOF
   AC_MSG_CHECKING([for $1])
   echo $MPITEST_F77 -c conftest.f 1>& AC_FD_CC
   cat conftest.f 1>& AC_FD_CC
   $MPITEST_F77 -c conftest.f 1>&AC_FD_CC 2>&AC_FD_CC
   if test -s conftest.o ; then
	AC_MSG_RESULT(yes)
	ifelse([$2],,,[$2])
   else
	AC_MSG_RESULT(no)
	ifelse([$3],,,[$3])
   fi
   rm -f conftest*
])

AC_DEFUN([PAC_TEST_FORTTYPES],[
FIX_FILE=0
FORT_INT1=1
FORT_INT2=1
FORT_INT4=1
FORT_REAL4=1
FORT_REAL8=1
FORT_DOUBLE_COMPLEX=1
COUNT=13
dnl Never support real*2
OPT_FREAL2="C "
AC_SUBST(OPT_FREAL2)
PAC_PROG_F77_TYPE([integer*1],,
                  [FIX_FILE=1;FORT_INT1=0;COUNT=`expr $COUNT - 1`])
PAC_PROG_F77_TYPE([integer*2],,
                  [FIX_FILE=1;FORT_INT2=0;COUNT=`expr $COUNT - 1`])
PAC_PROG_F77_TYPE([integer*4],,
                  [FIX_FILE=1;FORT_INT4=0;COUNT=`expr $COUNT - 1`])
PAC_PROG_F77_TYPE([real*4],,
                  [FIX_FILE=1;FORT_REAL4=0;COUNT=`expr $COUNT - 1`])
PAC_PROG_F77_TYPE([real*8],,
                  [FIX_FILE=1;FORT_REAL8=0;COUNT=`expr $COUNT - 1`])
PAC_PROG_F77_TYPE([double complex],,
                  [FIX_FILE=1;FORT_DOUBLE_COMPLEX=0;COUNT=`expr $COUNT - 1`])
   ])dnl
dnl
dnl
dnl PAC_FIX_MPITEST_FILES removes the Fortran Datatype that is not supported
dnl by the current machine.  It creates 1 files, mpitest_cfgf.fix 
dnl which will have the valid datatypes supported by the 
dnl current machine, and the NUMTOK3 variable will be changed to reflect the
dnl number of supported datatypes.
dnl PAC_RM_FTYPE removes the "type" from the configuration file
dnl if the type is not supported
dnl PAC_RM_FTYPE(shortname,longname)
dnl e.g., PAC_RM_FTYPE(INT1,INTEGER1)
dnl
dnl These delete the line from the file to ensure that we don't
dnl mess up the continuation marks
AC_DEFUN([PAC_RM_FTYPE],[
    if test "$enable_fortranoptionaltypes" = "no" ; then
        FORT_$1=0
    fi
    if test $FORT_$1 = 0; then
	AC_SUBST(OPT_F$1)
        OPT_F$1="C "
	rm -f temp.fix
	sed -e "/MPITEST_$2,/d" mpitest_cfgf.h > temp.fix
        mv -f temp.fix mpitest_cfgf.h
    fi
])
AC_DEFUN([PAC_FIX_MPITEST_FILES],
   [cd include
    # VPATH copies
    if test ! -s mpitest_cfgf.h -a -s $MPITEST_SRCDIR/include/mpitest_cfgf.h ; then
	cp $MPITEST_SRCDIR/include/mpitest_cfgf.h mpitest_cfgf.h
    fi

    if test ! -s mpitest_cfgf.h.orig ; then
        cp mpitest_cfgf.h mpitest_cfgf.h.orig
    fi
dnl
    if test "$enable_fortranoptionaltypes" = "no" ; then
        COUNT=7
    fi
    if test $COUNT != 13; then
 	rm -f temp.fix
        sed -e "s/NUMTOK3 = 13/NUMTOK3 = $COUNT/g" mpitest_cfgf.h > temp.fix
        mv -f temp.fix mpitest_cfgf.h
    fi        
dnl
    PAC_RM_FTYPE(INT1,INTEGER1)
    PAC_RM_FTYPE(INT2,INTEGER2)
    PAC_RM_FTYPE(INT4,INTEGER4)
    PAC_RM_FTYPE(REAL4,REAL4)
    PAC_RM_FTYPE(REAL8,REAL8)
    PAC_RM_FTYPE(DOUBLE_COMPLEX,DOUBLE_COMPLEX)
    rm -f temp.fix
    sed -e "/^[ ]*\&[ ]*$/d" mpitest_cfgf.h > temp.fix
    mv -f temp.fix mpitest_cfgf.h
    cd ..
   ])dnl
dnl
dnl
dnl PAC_FIX_DEF changes '#define MPITEST_long_double_def' to 0 in mpitest_def.h
dnl This routine is called when the MPI_LONG_DOUBLE is incompatible with
dnl the current compiler's long double 
define(PAC_FIX_DEF,dnl
    [cd include
     if test ! -s mpitest_def.h.orig ; then
         cp mpitest_def.h mpitest_def.h.orig
     fi
     rm -f temp.fix
     sed -e "s/#define MPITEST_long_double_def 1/#define MPITEST_long_double_def 0/g" mpitest_def.h > temp.fix
     mv -f temp.fix mpitest_def.h
     cd ..
dnl
    ])dnl
dnl
dnl
dnl PAC_FIX_CFG(datatype) removes the datattype ($1) from mpitest_cfg.h
dnl This routine is called when the MPI_datatype is incompatible with the 
dnl current compiler's datatype, or if fortran-optional-datatypes is disabled.
define(PAC_FIX_CFG,dnl
    [cd include
     if test ! -s mpitest_cfg.h.orig ; then
         cp mpitest_cfg.h mpitest_cfg.h.orig
     fi
     rm -f temp.fix
     sed -e "s/$1,//g" mpitest_cfg.h > temp.fix
     mv -f temp.fix mpitest_cfg.h
     cd ../
dnl
   ])dnl
dnl
dnl
dnl PAC_TEST_DATATYPES first creates a C program called 'testtypes' which 
dnl checks to see if all the MPI_Datatypes are compatible with the current
dnl compiler's corresponding datatype.  It then compiles and executes this
dnl program.  If there are any incompatible datatypes, then 'testtypes' will
dnl write them out to an output file.  Any datatypes in this output file will
dnl be subsequently removed from mpitest_cfg.h and if it is a long double,
dnl then mpitest_def.h will also be affected,
define(PAC_TEST_DATATYPES,dnl
    [cat > testtypes.c <<EOF
#include "mpi.h"
#include <stdio.h>

int main(int argc, char **argv)
     {
         int m_size, c_size;

         FILE *f=fopen("test.out","w");
         if (!f) exit(1); 
         MPI_Init(&argc, &argv);

         MPI_Type_size(MPI_INT, &m_size);
         c_size = sizeof(int);
         if (m_size != c_size)  
             fprintf( f, "%s\n", "FIX_INT");        

         MPI_Type_size(MPI_SHORT, &m_size);
         c_size = sizeof(short);
         if (m_size != c_size)  
             fprintf( f, "%s\n", "FIX_SHORT");        

         MPI_Type_size(MPI_LONG, &m_size);
         c_size = sizeof(long);
         if (m_size != c_size)  
             fprintf( f, "%s\n", "FIX_LONGS");

         MPI_Type_size(MPI_UNSIGNED_SHORT, &m_size);
         c_size = sizeof(unsigned short);
         if (m_size != c_size)  
             fprintf( f, "%s\n", "FIX_UN_SHORT");

         MPI_Type_size(MPI_UNSIGNED, &m_size);
         c_size = sizeof(unsigned int);
         if (m_size != c_size)  
             fprintf( f, "%s\n", "FIX_UN_INT");

         MPI_Type_size(MPI_UNSIGNED_LONG, &m_size);
         c_size = sizeof(unsigned long);
         if (m_size != c_size)  
             fprintf( f, "%s\n", "FIX_UN_LONG");

         MPI_Type_size(MPI_FLOAT, &m_size);
         c_size = sizeof(float);
         if (m_size != c_size)  
             fprintf( f, "%s\n", "FIX_FLOAT");

         MPI_Type_size(MPI_DOUBLE, &m_size);
         c_size = sizeof(double);
         if (m_size != c_size)  
             fprintf( f, "%s\n", "FIX_DOUBLE");

         MPI_Type_size(MPI_CHAR, &m_size);
         c_size = sizeof(signed char);
         if (m_size != c_size)  
             fprintf( f, "%s\n", "FIX_CHAR");

         MPI_Type_size(MPI_UNSIGNED_CHAR, &m_size);
         c_size = sizeof(unsigned char);
         if (m_size != c_size)  
             fprintf( f, "%s\n", "FIX_UN_CHAR");

         MPI_Type_size(MPI_LONG_DOUBLE, &m_size);
         c_size = sizeof(long double);
         if (m_size != c_size)  
             fprintf( f, "%s\n", "FIX_LONG_DOUBLE");

         MPI_Finalize(); 
         return 0;
      }
EOF
 
dnl
       $MPITEST_CC testtypes.c -o testtypes $MPI_LIB   

dnl
      if test ! -x testtypes ; then
	AC_MSG_WARN([Unable to compile (or execute) C program (in order to test for incompatible data types)])
      else
          $MPIRUN -np 1 ./testtypes
      fi
dnl
      if test -f test.out ; then
	if test -s test.out ; then
          FIX_CFG=1
          AC_MSG_CHECKING(if MPI_INT is compatible with current compiler's integer)
          cat test.out | grep FIX_INT > temp
	  if test -s temp ; then
            AC_MSG_RESULT(no)
	    PAC_FIX_CFG(MPITEST_int)          
          else
	    AC_MSG_RESULT(yes)
          fi
	  /bin/rm temp
dnl
          AC_MSG_CHECKING(if MPI_SHORT is compatible with current compiler's short)
	  cat test.out | grep FIX_SHORT > temp
	  if test -s temp ; then
            AC_MSG_RESULT(no)
	    PAC_FIX_CFG(MPITEST_short_int)          
          else
            AC_MSG_RESULT(yes)            
          fi
          /bin/rm temp
dnl
          AC_MSG_CHECKING(if MPI_LONG is compatible with current compiler's long integer)
          cat test.out | grep FIX_LONGS > temp
	  if test -s temp ; then
            AC_MSG_RESULT(no)
	    PAC_FIX_CFG(MPITEST_long)	  
          else
	    AC_MSG_RESULT(yes)
	  fi
	  /bin/rm temp
dnl
          AC_MSG_CHECKING(if MPI_UNSIGNED_SHORT is compatible with current compiler's unsigned short integer)
          cat test.out | grep FIX_UN_SHORT > temp
	  if test -s temp ; then
            AC_MSG_RESULT(no)
	    PAC_FIX_CFG(MPITEST_unsigned_short)	  
          else
            AC_MSG_RESULT(yes)
          fi
	  /bin/rm temp
dnl
          AC_MSG_CHECKING(if MPI_UNSIGNED_INTEGER is compatible with current compiler's unsigned integer)
	  cat test.out | grep FIX_UN_INT > temp
	  if test -s temp ; then
            AC_MSG_RESULT(no)
	    PAC_FIX_CFG(MPITEST_unsigned)	  
          else
            AC_MSG_RESULT(yes)
          fi
	  /bin/rm temp
dnl
          AC_MSG_CHECKING(if MPI_UNSIGNED_LONG is compatible with current compiler's unsigned long integer)
          cat test.out | grep FIX_UN_LONG > temp
          if test -s temp ; then
            AC_MSG_RESULT(no)
	    PAC_FIX_CFG(MPITEST_unsigned_long)	  
          else
            AC_MSG_RESULT(yes)
          fi
	  /bin/rm temp
dnl
          AC_MSG_CHECKING(if MPI_FLOAT is compatible with current compiler's float)
	  cat test.out | grep FIX_FLOAT > temp
          if test -s temp ; then
            AC_MSG_RESULT(no)
	    PAC_FIX_CFG(MPITEST_float)	  
	  else
            AC_MSG_RESULT(yes)
          fi
	  /bin/rm temp
dnl  
          AC_MSG_CHECKING(if MPI_DOUBLE is compatible with current compiler's double)
          cat test.out | grep FIX_DOUBLE > temp
          if test -s temp ; then
            AC_MSG_RESULT(no)
	    PAC_FIX_CFG(MPITEST_double)	  
	  else
            AC_MSG_RESULT(yes)
          fi
	  /bin/rm temp
dnl
          AC_MSG_CHECKING(if MPI_CHAR is compatible with current compiler's char)
          cat test.out | grep FIX_CHAR > temp
          if test -s temp ; then
            AC_MSG_RESULT(no)
	    PAC_FIX_CFG(MPITEST_char)
	  else
            AC_MSG_RESULT(yes)	  
          fi
	  /bin/rm temp
dnl
          AC_MSG_CHECKING(if MPI_UNSIGNED_CHAR is compatible with current compiler's unsigned char)
          cat test.out | grep FIX_UN_CHAR > temp
          if test -s temp ; then
            AC_MSG_RESULT(no)
	    PAC_FIX_CFG(MPITEST_unsigned_char)	  
	  else
            AC_MSG_RESULT(yes)
          fi
	  /bin/rm temp
dnl
          AC_MSG_CHECKING(if MPI_LONG_DOUBLE is compatible with current compiler's long double)
          cat test.out | grep FIX_LONG_DOUBLE > temp
          if test -s temp ; then
            AC_MSG_RESULT(no)
	    PAC_FIX_CFG(MPITEST_long_double)
            FIX_DEF=1
            PAC_FIX_DEF
	  else
            AC_MSG_RESULT(yes)
          fi
	  /bin/rm temp
dnl
        fi
      fi
      if test -f testtypes.c ; then
          /bin/rm testtypes.c
       fi
       if test -f testtypes.o ; then
	  /bin/rm testtypes.o
       fi
       if test -f testtypes ; then
           /bin/rm testtypes
       fi
       if test -f test.out ; then
           /bin/rm test.out
        fi
   ])dnl
dnl
dnl
dnl
dnl PAC_INFO_ERROR generates a PERL script, which when executed, runs on all
dnl fortran tests to ensure that ERRORSTRING is not written out any longer
dnl than size (the size allocated by MPI_ERROR_STRING.
define(PAC_INFO_ERROR,dnl
[
if test -z "$PERL" ; then
    AC_PATH_PROG(PERL,perl)
fi
changequote(`,')dnl
    cat > info_error.pl <<EOF
#! $PERL
    \$fortrandir = \$ARGV[0];

    $|=1;
    opendir(FORTRAN, \$fortrandir) ||
        die("Unable to open directory\n");
    @fortrandir = grep(!/^\./, readdir(FORTRAN));

    foreach \$testdir (@fortrandir) {
        \$test_workdir = \$ARGV[0] . "/" . \$testdir;
        opendir(TEST_WORKDIR, \$test_workdir) ||
            die("Unable to open directory\n");
        print ".";  
  
        @err_func_dirs = grep(!/^\./, readdir(TEST_WORKDIR));
        foreach \$error_func (@err_func_dirs) {
	    \$error_func_dir = \$test_workdir . "/" . \$error_func;
            opendir(DIR, \$error_func_dir) ||
	        die("Unable to open directory\n");
            print ".";  

            @filelist = grep(!/^\./, grep(/^MPI/, readdir(DIR)));

            \$cntline = 5;
            foreach \$file (@filelist) {   # begin foreach \$file
            \$newdir = \$error_func_dir . "/" . \$file . "/";
            opendir(WORKDIR, "\$newdir") ||
	        die("Unable to open \$file directory\n");
            chdir(\$newdir);
	    \$cntline--; if (\$cntline == 0) { print "."; \$cntline = 5; }

            @node_file = grep (/node.F/, readdir(WORKDIR));
            \$length_node = @node_file;
            if (\$length_node == 1) {   # begin if \$length_node
	        (\$node_file) = @node_file;
	        unless (open(OLDNODE, "\$node_file") ) {
	            print ("Cannot open \$node_file\n");
	        }

	        unless (open(NEWNODE, ">temp.f") ) {
	            print ("Cannot open temp.f\n");
	        }

	        while (\$node_line = <OLDNODE>) {   # begin while \$node_line
	            \$flag = 0;
                    \$result_error = \$node_line =~ /CALL MPI_ERROR_STRING/;
	            print NEWNODE (\$node_line);
                    if (\$result_error) {   # begin if \$result_error
	                \$position = `index'(\$node_line, "CALL");
                        \$sub_node = `substr'(\$node_line, \$position);
		        chop(\$sub_node);
		        @node_error = split(/,/, \$sub_node);
		        \$length = @node_error;
		        if (\$length < 3) {   # begin if \$length
		            \$node_line = <OLDNODE>;
		            \$result_size = \$node_line =~ /SIZE/;
		            if (\$result_size) {
			        \$flag = 1;
		            }
		            print NEWNODE (\$node_line);
		        }  # end if \$length
                        elsif ( (\$node_error[2] eq "SIZE") || (\$node_error[2] eq " SIZE") ) {   # begin double if
		            \$flag = 1;
		        }  # end double if
		        if (\$flag) {  # begin if \$flag
		            \$node_line = <OLDNODE>;
                            \$result_dollar = \$node_line =~ /\\$/;
		            if  (\$result_dollar) {   # begin if \$result_dollar
			        print NEWNODE (\$node_line);
			        \$node_line = <OLDNODE>;
		            }   # end if \$result_dollar
                            \$result_blank = \$node_line !~ /[A-Z]/;
		            if (\$result_blank) {   # begin if \$result_blank
			        print NEWNODE (\$node_line);
			        \$node_line = <OLDNODE>;
		            }   # end if \$result_blank
		            \$result_info = \$node_line =~ /WRITE\(INFOBUF,/;
                            if (\$result_info) {   # begin if \$result_info
		                \$position = `index'(\$node_line, "WRITE");
		                \$sub_node = `substr'(\$node_line, \$position);
		                chop(\$sub_node);
			        chop(\$node_line);
		                @node_write = split(/ /, \$sub_node);
		                if ( (\$node_write[2] eq "ERRORSTRING") || (\$node_write[1] eq "ERRORSTRING") )  {   # begin if \$node_write[2]    
			            \$newline = \$node_line . "(1:SIZE)\n";
			            print NEWNODE (\$newline);
		                }   # end if \$node_write[2]
			        else {   # begin else
			            print NEWNODE ("\$node_line\n");
			        }   # end else
                             }   # end if \$result_info
		             else {   # begin else
			         print NEWNODE ("\$node_line\n");
	                     }   # end else
		        }   # end if flag
	            }   # end if \$result_error
	        }   #end while \$node_line
                \$mv_str = "mv temp.f node.F";
                system(\$mv_str);
              }   #end if \$length_node
            }   # foreach \$file
        }  # foreach err_func
     }  #foreach \$testdir
EOF
changequote([,])dnl
    ])dnl
define(PAC_FIX_START_FILE,dnl
[changequote(!,!)dnl
    cd bin
dnl
    PWD=pwd
    Q=\`
    cat > startjob.${MPITEST_ARCH} <<EOF
    #!/bin/sh

    if test -z "\$!2!"
    then
       echo "ERROR: \$!0! \"-appname\" option: argument required"
       exit 201
    else
       if test -x "\$!2!"
       then
           # run test 
           dir=$Q$PWD$Q
           cd \$dir
           \${MPIRUN} -np \$MPITEST_NPROCS \$!2!
       else
          echo "ERROR: \$!0! \"-appname\" option: \$!2! does not exist or is not executable"
          exit 201
       fi
    fi
EOF
changequote([,])
dnl
if test ! -x "startjob.${MPITEST_ARCH}" ; then
    chmod +x startjob.${MPITEST_ARCH}
fi
dnl
cd ../
dnl
    ])dnl
dnl
