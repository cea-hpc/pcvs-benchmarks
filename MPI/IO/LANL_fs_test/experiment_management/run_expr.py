#! /usr/bin/env python

import sys,re,os,imp
from lib import expr_mgmt

version = "%prog 0.1"

# a wrapper pretty much for expr_mgmt.opt_parse
# defines usage, description, and checks required args
def get_args(argv):
  usage     = "usage: %prog [options] /path/to/experiment.py" 
  description = """Use this script to run an experiment in the 
           experiments directory.  The experiment must
           have a .py extension and must define a
           get_commands() function which returns a list
           of mpirun (or aprun or srun) commands. The experiments directory has
           several example experiments and the README 
           has more information about creating these as well."""
  return expr_mgmt.options_parse( argv=argv, usage=usage,
    description=description, required=1, version=version )

def get_commands( module_file , expr_mgmt_options):
  # first try to import it as a module and call get_commands, otherwise treat
  # it as simple test
  try: 
    commands = get_module(module_file).get_commands(expr_mgmt_options=expr_mgmt_options)
  except AttributeError, e:
    commands = []
    fp = open(module_file)
    for line in fp.readlines(): 
      print "line " + line
      if "mpirun" in line or "aprun" in line and line[0] != "#": 
        commands.append(line.rstrip(os.linesep))
  return commands


# the main routine, marshall args, get commands, dispatch them
def main(argv=None):
  if argv == None:
      argv = sys.argv
  options,args = get_args(argv)
  if options == None:
      return -1
  commands = get_commands( module_file=args[0], expr_mgmt_options=options)
  if commands == None:
      return -1
  result = expr_mgmt.dispatch_commands( commands=commands, options=options ) 

  # If this script is being called directly from the command line, we are
  # unable to just return the last job id because anything over 127 is just
  # converted to a 1, which usually means some sort of error. So, if this
  # script is being run from a command line, return acceptable values. But,
  # if it is being run from an existing python session, return the last job
  # id so that output doesn't have to be parsed to find it.
  if __name__ == "__main__":
      if result > 0:
          return 0
      else:
          return result
  return result


# this takes a path to a module file and returns a function pointer to
# a function inside that module
def get_module( module_path ):
  # this block just breaks the passed path into the path for the parent dir
  # and the name of the file itself (without any trailing extension), and 
  # then imports the module
  path_components = module_path.split('/') 
  module_file = path_components.pop()   
  try: module_file = re.compile('(.*)\.\S+$').match(module_file).group(1)
  except AttributeError: pass
  parent = "/".join(path_components)    
  (fp, path, desc) = imp.find_module(module_file, [parent])
  module = imp.load_module(module_file, fp, path, desc)
  fp.close()
  return module


if __name__ == "__main__": sys.exit(main())
