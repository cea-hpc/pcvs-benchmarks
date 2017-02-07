import sys,os,time
sys.path += [ './lib', '../lib' ]
import expr_mgmt

# just edit these parameters, the rest of the script
# adjusts accordingly to produce N-N, N-1 strided and N-1 segmented
data_per_proc = 4 * 1024 * 1024
io_sizes = [ 1024 ]
nps   = range(4,5,2)
wrapper = True

if wrapper == True:
  ior_top = "%s/Testing/Benchmarks/IOR" % os.getenv("HOME")
  mpirun = "%s/ior_wrapper.py mpirun" % ior_top 
  program_arguments = [
    [ "--desc rrz.scaling" ],
    [ "--env_to_db %s/env_to_db.tcsh" % ior_top ],
  ]
else:
  program_arguments = None
  mpirun = "mpirun"

mpi_program = "%s/Testing/IOR/src/C/IOR" % os.getenv("HOME")  

panfs    = expr_mgmt.system_default("filesystem") 
program_options = {
  "i" : [ 1 ],
  "o" : [ "%s/%s/ior.out" % ( panfs, os.getenv('USER') )],
  "a" : [ 'POSIX' ],
}

mpi_options = {
  'np' : nps 
}

#############################################################################
# typical use of this framework won't require modification beyond this point
#############################################################################

def get_commands( expr_mgmt_options ):

    # helper utility
  def make_commands():
    return expr_mgmt.get_commands(
      mpi_options=mpi_options,
      mpi_program=mpi_program,
      mpirun=mpirun,
      program_arguments=program_arguments,
      program_options=program_options,
      expr_mgmt_options=expr_mgmt_options )

  commands = []

  # N-1 strided
  for size in io_sizes:
    program_options['s'] = [ data_per_proc/size ]
    program_options['b'] = [ size ]
    program_options['t'] = [ size ]
    commands += make_commands()

  # N-1 segmented
  program_options['s'] = [ 1 ]
  program_options['b'] = [ data_per_proc ]
  program_options['t'] = io_sizes 
  commands += make_commands() 

  # N-N 
  program_options['F'] = [ '' ]
  commands += make_commands() 

  return commands
