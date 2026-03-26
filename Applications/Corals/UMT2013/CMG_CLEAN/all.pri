# -*- makefile -*-

SILO = $$(SILO)
PACT = $$(PACT)


#Always compile with debug (this adds -g automatically)
CONFIG += debug

#Remove qt which is defined by default
CONFIG -= qt 

#Common defines for the whole project
DEFINES = CMGDEBUG \

QMAKE_MAKEFILE = Makefile.$${TARGET}.$$(SYS_TYPE)
MAKEFILE = Makefile.$${TARGET}.$$(SYS_TYPE)

##Immediately evaluate the SYS_TYPE environment variable
SYS_TYPE = $$(SYS_TYPE)
#Try running the systype utility to get the SYS_TYPE
isEmpty( SYS_TYPE ) { SYS_TYPE = $$system('/usr/gapps/axdot/systype') }
#Give up
isEmpty( SYS_TYPE ) { error('Environment variable SYS_TYPE must be set.') }
#else { message( Detected sys type $${SYS_TYPE} ) }

THREADED_SYS_TYPES = rhe_3_ia32 fedora_3_ia32 chaos_3_x86_elan3
LINUX_SYS_TYPES = redhat_9_ia32 rhe_3_ia32 chaos_2_ia32_elan3 \
		  fedora_3_ia32 chaos_3_x86_elan3
AIX_SYS_TYPES   = aix_5_ll aix_5_64_fed
AIX_SYS_TYPES_64 = aix_5_64_fed
MAC_SYS_TYPES   = macos_10a

#Now add the sys type and generic OS type
CONFIG += $${SYS_TYPE}
contains( LINUX_SYS_TYPES, $${SYS_TYPE} ) { CONFIG += linux }
contains( AIX_SYS_TYPES, $${SYS_TYPE} ) { CONFIG += aix }
contains( MAC_SYS_TYPES, $${SYS_TYPE} ) { CONFIG += macx }
contains( AIX_SYS_TYPES_64, $${SYS_TYPE} ) {CONFIG+= aix_64_fed }

#IF enterprise 3-based, use multithreaded qt
contains( THREADED_SYS_TYPES, $${SYS_TYPE} ) { CONFIG += thread }

aix{
CC = cc
}
else{
CC = gcc
}

aix_64_fed{
DEFINES+=aix_64
}

F77 = f77

CFLAGS = -ansi

QMAKE_CFLAGS_DEBUG += $${CFLAGS}
QMAKE_CFLAGS_RELEASE += $${CFLAGS}

QMAKE_CC = $${CC} 

isEmpty( PACT ) { 
  aix { 
    aix_5_ll { PACT = /usr/apps/pact/32 }
    aix_5_64_fed { PACT = /usr/apps/pact/64 }
 }
  else { PACT = /usr/gapps/pmesh/pact/$${SYS_TYPE} } 
}
else { OVERRIDES += PACT }

isEmpty( SILO ) {
  aix {
    aix_5_ll { SILO = /usr/gapps/silo/current/aix_4 }
    aix_5_64_fed { SILO = /usr/gapps/silo/4.4.3/aix_5_64_fed }
 }
  else { SILO = /usr/gapps/pmesh/silo/$${SYS_TYPE} } 
}
else { OVERRIDES += SILO }

!isEmpty( OVERRIDES ) {
message ("Overriding default values for: $${OVERRIDES}")
}

#  PACT
PACT_INCLS = $${PACT}/include
PACT_LIBS = -L$${PACT}/lib -lpdb -lpml -lscore

#  SILO
SILO_INCLS = $${SILO}/include
SILO_LIBS = -L$${SILO}/lib -lsilo 
!aix { 
  !macx {
    SILO_LIBS += -lnsl
  }
}
