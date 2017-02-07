import sys
sys.path += [ './lib', '../lib' ]
import re
import expr_mgmt
# some routines specific to the fs_test experiment

def get_commands( mpi_options, mpi_program, program_options, expr_mgmt_options, 
                    n1_strided=True, n1_segmented=True, nn=True, 
                    auto_cw=False, mpirun=None):
  commands = []
  if n1_strided is True or n1_segmented is True:
    ensure_n1(program_options,auto_cw,strided=n1_strided,segmented=n1_segmented)
    commands += expr_mgmt.get_commands( mpi_options=mpi_options,
                      mpi_program=mpi_program,
                      program_options=program_options, mpirun=mpirun,
                      expr_mgmt_options=expr_mgmt_options )  
  if nn is True:
    ensure_nn(program_options,auto_cw)
    commands += expr_mgmt.get_commands( mpi_options=mpi_options,
                      mpi_program=mpi_program,
                      program_options=program_options, mpirun=mpirun,
                      expr_mgmt_options=expr_mgmt_options )  
  return commands

# this currently only works for hints that are integers....
# (notice the \d+ in the re.sub line)
def set_hint(key,value,program_options):
  try: hints = program_options['hints']
  except KeyError: hints = [ '' ]
  incoming = hints
  outgoing = []
  for hint in incoming:
    if len(hint):
      if key in hint:
        hint = re.sub(r'%s=\d+'%key,'%s=%s'%(key,value),hint)
      else:
        hint = '%s,%s=%s' % (hint,key,value)
    else:
      hint = '%s=%s' % (key,value)
    outgoing.append(hint) 
  program_options['hints'] = outgoing

# make program_options work for N-1 
def ensure_n1( program_options, auto_cw, strided, segmented ):
  for p in ( 'strided' ): # rip out strided argument and add it explicitly
    if p in program_options: 
      del program_options[p]
  program_options["type"] = [ 2 ] 
  program_options["strided"] = [ ]
  if strided   is True: program_options["strided"].append(1)
  if segmented is True: 
    program_options["strided"].append(0)
    if 'time' in program_options and 'supersize' not in program_options:
      program_options['supersize'] = [ 4096 ] # set a default if not specified
  for i in range(len(program_options["target"])):
    program_options["target"][i] \
          = program_options["target"][i].replace(".%r","")
  if auto_cw: set_hint('panfs_concurrent_write',1,program_options)

# make program_options work for N-N 
def ensure_nn( program_options, auto_cw ):
  for p in ( 'strided' ):
    if p in program_options: del program_options[p]
  program_options["type"] = [ 1 ] 
  for i in range(len(program_options["target"])):
    program_options["target"][i] += ".%r"
  if auto_cw: set_hint('panfs_concurrent_write',0,program_options)
