#! /usr/bin/env python

import sys,expr_mgmt

try:
  ppns = expr_mgmt.config_option_value("ppn")

except KeyError, data:
  sys.stdout.write( str( data ) + ' is a nonexistent system.\n' )

else:
  sys.stdout.write( 'PPN for default system is "' + str( ppns ) + '".\n' ) 
