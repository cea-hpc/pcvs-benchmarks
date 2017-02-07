import sys,os,time,getpass
sys.path += [ './lib', '../lib' ]
import expr_mgmt

user        = getpass.getuser()
home        = os.getenv( "HOME" )
my_mpi_host = os.getenv( "MY_MPI_HOST" )

# the mdtest_wrapper actually calls mpirun so use a non-default mpirun command
mdtest_top = "/usr/projects/ioteam/%s/mdtest-1.8.5/" % ( my_mpi_host )
mpirun = "%s/scripts/mdtest_wrapper.py aprun" % ( mdtest_top )

# if you want to use this to run a new test, use the current time,
# if you want to use this to complete an already started test, use that time
ts = int( time.time() )
#ts = 1269097554
#
# We cannot have a constant file count divided by the number of processes
# in the experiment by defining "n" here, but we do need the variable,
# mpi_options, to exist. We will then use a custom "make_commands" below.
#
mpi_options = {
  "np"   : [ 2, 4 ], 
#
# "n" is used on the Crays.
#
#  "n"    : [ 2, 4 ],
}

mpi_program = ( "%s/mdtest" % ( mdtest_top )) 

target = ( "/lustre/lscratch/%s/mdtest" % ( user ))

program_options = {
#
# Branching factor of hierarchical directory structure. See README. It's
# complicated.
#
#  "b" : [ 2 ],
#
# Each task times itself. Have noticed bugs where the experiment will
# fail when using this parameter. Generally, don't use it.
#
#  "B" : [ '' ],
#
# Collective creates: task 0 does all the creates and deletes.
#
#  "c" : [ '' ],
#
# Only create files/dirs.
#
  "C" : [ '' ],
#
# The directory for the files.
#
  "d" : [ "%s/mdtest.%d/" % ( target, ts ) ], 
#
# Perform tests on directories only (no files).
#
#  "D" : [ '' ],
#
# Number of bytes to read from each file.
#
#  "e" : [ 64 ],
#
# Only read files.
#
#  "E" : [ '' ],
#
# First number of tasks on which test will run. I don't understand this and
# there is no additional explanation in the README.
#
#  "f" : [ <integer>?? ],
#
# Perform the tests on the files only (no directories).
#
  "F" : [ '' ],
#
# Number of iterations in the experiment. Default is 1.
#
#  "i" : [ 10 ],
#
# "n" and "I" are mutually exclusive. In general, we prefer "n" because it
# creates, stats, and deletes the files in the directory the caller
# specifies.
#
# "I" specifies the number of items per tree node. Use this in conjunction
# with "b" and "z".
#
#  "I" : [ 1, 10 ],
#
# Last  number of tasks on which test will run. I don't understand this and
# there is no additional explanation in the README.
#
#  "l" : [ <integer>?? ],
#
# Create files and directories at the "leaf" level only.
#
#  "L" : [ '' ],
#
# "n" and "I" are mutually exclusive. In general, we prefer "n" because it
# creates, stats, and deletes the files in the directory the caller
# specifies.
#
# Every task will create/stat/delete number of files/dirs specified per tree.
#
# We cannot have a constant file count divided by the number of processes
# in the experiment by defining "n" here. We will use a custom
# "make_commands" below.
#
  "n" : [ 1024 ],
#
# Stride number between neighbor tasks for file/dir stat. Make this the number
# of processes that will run on a node.
#
  "N" : [ 16 ],
#
# Pre-iteration delay in seconds.
#
#  "p" : [ 10 ],
#
# Only remove files/dirs.
#
  "r" : [ '' ],
#
# Randomly stat files/dirs (optional seed can be provided).
#
#  "R" : [ '' ],
#
# Stride between the number of tasks for each test. No other explanation
# is given. I'm not sure what this does.
#
#  "s" : [ 16 ],
#
# Shared file access (file only, no directories). For N-1 where all
# processes operate on the same file.
#
#  "S" : [ '' ],
#
# Time unique working directory overhead. No other explanation is given.
# I'm not sure what this does.
#
#  "t" : [ '' ],
#
# Only stat files/dirs.
#
  "T" : [ '' ],
#
# This parameter has each process use its own directory. If you don't
# use it, all processes use the same, shared, directory.
#
#  "u" : [ '' ],
#
# Verbosity (each instance of option increments by one). So, you can have:
# -v, -vv, -vvv, etc.
#
#  "v" : [ '' ],
#
# Verbosity value. Instead of lots of small "v"s, put an integer after
# this to specify the verbosity value.
#
#  "V" : [ 1 ],
#
# Number of bytes to write to each file.
#
#  "w" : [ 64 ],
#
# Sync file after write completion.
# 
#  "y" : [ '' ],
#
# Depth of hierarchical directory structure. See README. It's complicated.
#
#  "z" : [ 1, 2 ],
}

# the wrapper looks for these args at the end of the args and splices them
# off before calling IOR, these args are used by the wrapper to get more
# data into the sql insert
#program_arguments = [
#  [ "--desc ./mdtest.%d" % int(time.time()) ]
#]

#############################################################################
# typical use of this framework won't require modification beyond this point
#############################################################################

def get_commands( expr_mgmt_options ):
  global mpi_options,program_options,program_arguments,mpirun
#
# Uncomment this section when using mpi_options and "n" from above.
#
  commands = expr_mgmt.get_commands( 
      mpi_options=mpi_options,
      mpi_program=mpi_program,
#      program_arguments=program_arguments,
      mpirun=mpirun,
      program_options=program_options,
      expr_mgmt_options=expr_mgmt_options )
  return commands
#
# Comment this section when using mpi_options and "n" from above.
#
  #def make_commands():
  #  return expr_mgmt.get_commands(
  #      mpi_options=mpi_options,
  #      mpi_program=mpi_program,
  ##      program_arguments=program_arguments,
  #      mpirun=mpirun,
  #      program_options=program_options,
  #      expr_mgmt_options=expr_mgmt_options )

  #commands = []

  #for exponent in range ( 0, 11 ):
  ##for exponent in range ( 6, 7 ):
  #  np = 2**exponent
  #  mpi_options['n'] = [ np ]
  #  program_options['n'] = [ 102400/np ]
  #  commands += make_commands()

  #return commands
