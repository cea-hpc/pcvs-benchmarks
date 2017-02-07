#! /usr/bin/env python

import os,sys,re,datetime,math,random,ConfigParser
from subprocess import Popen,PIPE
from optparse import make_option,OptionParser,OptionGroup

class configError(Exception):
    """An error class for when there is a problem in the configuration."""
    def __init__(self, msg):
        self.msg = msg
    def __str__(self):
        return str(self.msg)

def options_parse(argv=None, usage=None, description=None, version=None,
    required=0):
    """Parse the config file and the command line options. Creates a unified dictionary
    containing all options.

    Returns a dictionary that will have an entry for every one of the input
    parameters that experiment_management can accept.
    """
    # A list of all options that experiment_management can deal with.
    expr_mgmt_options = [ "runcommand", "dispatch", "iterations", "random",
        "limit", "quiet", "outdir", "ppn", "msub", "walltime", "lastjob",
        "chain", "prescript", "postscript", "nprocs" ]

    # Create the options dictionary that will be used throughout the rest
    # of experiment_management. Initialize all options to None.
    options = {}
    for opt in expr_mgmt_options:
        options[opt] = None

    # Set program defaults that shouldn't be None
    options["runcommand"] = "mpirun"
    options["dispatch"] = "list"
    options["iterations"] = 1
    options["random"] = False
    options["quiet"] = False
   
    try:
    
        # Parse the config file
        cf_options = conf_parse()
        
        for k, v in cf_options.iteritems():
            options[k] = v
        
        # Parse command line parameters
        cl_options, args = clp_parse(argv=argv, usage=usage,
            description=description, version=version, defaults=options,
            required=required)

        # cl_options is of type optparse.Values, so vars(cl_options) is needed to
        # get at key, value pairs.
        for k, v in vars(cl_options).iteritems():
            options[k] = v

        # We now have a complete set of experiment_management options. We need to
        # do some tests to make sure the needed options are here for the internal
        # workings of experiment_managment
        # Must have a runcommand
        if options["runcommand"] == None:
            raise configError("runcommand must be specified")
            
        # If dispatch is msub or qsub, must have ppn and outdir
        if options["dispatch"] == "msub" or options["dispatch"] == "qsub":
            if options["ppn"] == None or options["outdir"] == None:
                raise configError("with dispatch=msub or qsub, ppn and outdir "
                    + "must be specified")

    except configError, detail:
        print("Error in experiment_management configuration: " + str(detail))
        return None, None
    else:
        return options, args

def get_rc_file():
    # Check the environment variable EXPRMGMTRC
    cf = os.getenv('EXPRMGMTRC')
    if cf == None:
        # If that isn't set, look for ~/.exprmgmtrc
        cf = str(os.getenv('HOME')) + "/.exprmgmtrc"
    else:
        if os.path.exists(cf) != True:
            print ("EXPRMGMTRC is set to " + str(cf) + " but that is not a "
                + "valid file.")
    return cf

def get_all_section_options(section, config):
    return_options = {}
    try:
        options = config.options(section)
    except ConfigParser.NoSectionError:
        # It's ok if there is no section. It is still possible to run
        # experiment_management without putting anything in a conf file.
        pass
    else:
        for option in options:
            try:
                return_options[option] = config.get(section, option)
                # If it is true or false, convert them to python True/False
                if return_options[option] == "True":
                    return_options[option] = True
                if return_options[option] == "False":
                    return_options[option] = False
            except:
                print ("exception on %s." % option)
                return_options[option] = None
    return return_options
    
def conf_parse():
    """Parse experiment_management's config file.

    Returns a dictionary of the options for the machine experiment_management
    is running on.
    """
    conf_options = {}
    cf = get_rc_file()
    Config = ConfigParser.ConfigParser()
    # result will be an empty string if the file is not there. There will be
    # no error.
    result = Config.read(cf)
    if result != []:
        # Check the common section first
        common_options = get_all_section_options(section="expr_mgmt_common",
            config=Config)
        for k, v in common_options.iteritems():
            conf_options[k] = v
        # Now get the section for this host
        host = os.getenv('MY_MPI_HOST')
        host_options = get_all_section_options(section=host, config=Config)
        for k, v in host_options.iteritems():
            conf_options[k] = v
    return conf_options

def clp_parse( argv=None, usage=None, description=None, version=None,
  defaults={}, required=0):
  """Wrapper for optparse; gets the command line parameters

  Returns an object of type optparse.Values and a list
  """
  try: description = re.sub( "\s+", " ", description ) # clean out extra spaces
  except TypeError: pass  # description is None

  if argv == None:
      argv = sys.argv

  option_list = [
    make_option("-d", "--dispatch", metavar="D",
      help="what to do with the generated commands "
         "(msub|qsub|serial|list) [default=%default]",
      action="store", default=defaults["dispatch"]),
    make_option("-r", "--runcommand", metavar="COM",
      help="what run command to use for running parallel programs.",
      action="store", default=defaults["runcommand"]),
    make_option("-i", "--iterations", metavar="I",
      help="dispatch I copies of each command [default=%default]",
      type="int", default=defaults["iterations"]),
    make_option("-a", "--random", 
      help="shuffle the commands before dispatching",
      action="store_true", default=defaults["random"]),
    make_option("-l", "--limit", metavar="L",
      help="dispatch no more than L commands [default=%default]",
      type="int", default=defaults["limit"]),
    make_option("-q", "--quiet", 
      help="suppress printing the commands [default=%default]",
      action="store_true", default=defaults["quiet"]),
    ]

  msub_opts = [
    make_option("-o", "--outdir", metavar="O",
      help="set output directory [default=%default]",
      default = defaults["outdir"] ), 
    make_option("-p", "--ppn", metavar="N",
      help="set procs_per_node [default=%default]", type="int", 
      default = defaults["ppn"] ), 
    make_option("-M", "--msub", metavar="M",
      help="set msub options [default=%default]",
      default = defaults["msub"] ),
    make_option("-w", "--walltime", metavar="W",
      help="how much walltime to request [default=%default]",
      type="string", default=defaults["walltime"] ),
    make_option("-L", "--lastjob", metavar="L",
      help="make first command depend on an existing qsub job "
         "[default=%default]", type="int", default=defaults["lastjob"] ),
    make_option("-c", "--chain", 
      help="make each command depend on the previous [default=%default]",
      action="store_true", default=defaults["chain"] ),
    make_option("--pre", default=defaults["prescript"], dest="prescript",
      help="run a pre-script before each command [default=%default]" ),
    make_option("--post", default=defaults["postscript"], dest="postscript",
      help="run a post-script before each command [default=%default]" ),
    make_option("-n", "--nprocs", metavar="N",
      help="Specify number of processors to request. Used to calculate "
      "number of nodes to request. If not specified, the calculation will "
      "be done based on the generated mpi run command (default).",
      type="int", default = defaults["nprocs"]),
  ]

  parser = OptionParser( usage=usage, version=version, option_list=option_list,
                         description=description )
  extra_groups = [ [msub_opts, "MSUB"] ]
  for extra_group in extra_groups: 
    group = OptionGroup( parser, "%s options" % extra_group[1] )
    group.add_options( extra_group[0] )
    parser.add_option_group(group)
  options, args = parser.parse_args(argv[1:])
  # Check non-optional command line args
  if len(args) < required:
    raise configError("Not all required arguments provided.")
  elif len(args) > required:
    raise configError("Unknown extra arguments: " + str(args[1:]))
  return options, args

def config_option_value(key):
    """Pull a single option from experiment_management's config file and return it

    Returns a string with the option's value if it is found, None otherwise.
    """
    Config = ConfigParser.ConfigParser()
    cf = get_rc_file()
    
    result = Config.read(cf)
    if result != []:
        # Check the common section
        try:
            ret_str = Config.get("expr_mgmt_common", key)
        except:
            ret_str = None
        section = os.getenv('MY_MPI_HOST')
        # Check the host section
        try:
            ret_str = Config.get(section, key)
        except:
            # If it isn't found, just pass. ret_str is either a string or None
            # after checking the common section. It will get over-written if
            # it does exist in the host section.
            #print("Error finding option " + str(key) + " in section " 
            #    + str(section) + " in file " + str(cf))
            pass
    else:
        #print("Problem reading " + str(cf))
        ret_str = None
    return ret_str

def dispatch_commands( commands, options ):
  orig_dir = os.getcwd()
 
  if options["iterations"] > 1:
    tmp = commands[:]   # make it a deep copy
    for i in range(1,options["iterations"]):
      tmp += commands[:]
    commands = tmp 
  if options["random"] is True: random.shuffle(commands)

  if options["dispatch"] == "msub" or options["dispatch"] == "qsub":
    outdir = options["outdir"] + "/" + str(datetime.date.today())
    if not os.path.isdir( outdir ): os.makedirs( outdir, 0777 )
    os.chdir( outdir )

  dispatched = 0
  # The following code is needed in order to set aprun -N option from 
  # default ppn or ppn passed in with -p option.  The change was needed
  # here because of -list option
  for command in commands:
    if options["runcommand"] == 'aprun' and options["nprocs"] == None:
      np_cnt = re.compile('.*aprun\s+-n\s+(\d*)').match(command).group(1)
      pes = float(np_cnt)
      if options["ppn"] > pes:
          ppn = pes
      else:
          ppn = options["ppn"]
      aprun_opt = "%s -n %d -N %d " % ( "aprun", pes, ppn)
      command = re.sub('aprun\s+-n\s+\d*', aprun_opt, command)
    if   options["quiet"] is not True:    print command
    if   options["dispatch"] == 'list':   pass 
    elif options["dispatch"] == "serial": runSerial(command) 
    elif options["dispatch"] == "msub"  or options["dispatch"] == "qsub":
        options["lastjob"] = submit(command,options)
        if options["lastjob"] == -1:
            return -1
    else: raise SyntaxError, "Usage: unknown dispatch %s" % options["dispatch"]
    dispatched = dispatched+1
    if options["limit"] is not None and dispatched >= options["limit"]: break
  if options["quiet"] is not True: print # separate commands from status
  print "# %d jobs dispatched by %s." % (dispatched, options["dispatch"])

  if options["dispatch"] == "msub" or options["dispatch"] == "qsub": 
    os.chdir( orig_dir )  
    print "# Output in %s." % outdir
    return options["lastjob"]
  else:
    return 0


def runSerial( command ):
  try:
    retcode = Popen(command, shell=True).wait()
    if retcode < 0:
      print >>sys.stderr, "Child was terminated by signal", -retcode
  except OSError, e:
    print >>sys.stderr, "Execution failed:", e
  return None


def submit( command, options ): 
  # this is ugly bec it has to parse the command to figure out the
  # aprun -n N, mpirun -np N, or srun -n N in order to pass than on
  # to msub.  blech.
  # then add in the rest of the m_opts
  if options["nprocs"] == None:
    # Get the number of processes from the one of these commands:
    #   aprun
    #   mpirun
    #   srun
    try:
      np = re.compile('.*aprun\s+-n\s+(\d*)').match(command).group(1)
    except AttributeError:
      try:
        np = re.compile('.*mpirun\s+-np?\s+(\d*)').match(command).group(1)
      except AttributeError:
        try:
          np = re.compile('.*srun\s+-n\s+(\d*)').match(command).group(1)
        except AttributeError:
          print ("Error: Unable to get number of processes from the command "
            + str(command) + ".")
          return -1
  else:
    np = options["nprocs"]
  
  pe = float(np)
  nodes = math.ceil(float(np) / float(options["ppn"]))
  if options["dispatch"] == "msub":
    m_opts = "-l nodes=%d:ppn=%d" % ( nodes, options["ppn"] ) 
  elif options["dispatch"] == "qsub":
    m_opts = "-l mppwidth=%d,mppnppn=%d" % ( pe, options["ppn"] )
  if options["msub"] is not None: 
    m_opts += " %s" % options["msub"]
  if options["chain"] is True and options["lastjob"] is not None: 
    m_opts += " -l depend=%s" % options["lastjob"]
  if options["walltime"] is not None: 
    m_opts += " -l walltime=%s" % options["walltime"]

  # get the set of commands to run in the msub script from the possible
  # prescript, the definite command, and the possible postscript
  mcommands = [] 
  for mcommand in [ options["prescript"], command, options["postscript"] ]:
    if mcommand is not None:
      mcommands.append( "echo \"# Running %s\"" % mcommand )
      mcommands.append( mcommand )
  mcommand = "\n".join(mcommands)

  # now open pipes to submit it, to get the jobid, and to check for error 
  try:
    print "options: " + m_opts 
    problem = False
    ch = Popen(["%s %s" % ( options["dispatch"],m_opts )],shell=True,stdin=PIPE,stdout=PIPE,stderr=PIPE)
    ch.stdin.write(mcommand)
    ch.stdin.close()
    ch.wait()
    out = ch.stdout.read().strip()
    try: 
#    if options["dispatch"] == "msub": jobid = int(out)
#    else: jobid=out
     jobid=out
    except (TypeError, ValueError, OverflowError):
      print "Problem with msub/qsub output: " + out
      problem = True
    err = ch.stderr.read()
    if len(err): 
      print "Problem with msub/qsub: " + err
      problem = True
    if problem is True: return -1
  except Exception, e:
    print >> sys.stderr, "Execution failed:", e
    return -1
#  print "  # MSUB ID: %d (%s)" % ( jobid, m_opts )
  print "  # MSUB/QSUB ID: %s (%s)" % ( jobid, m_opts )
  return jobid


# takes two dictionarys of params : [ args ]
# one for args to mpirun, the other for args to the actual program
# and takes the path to the program
# returns all the commandlines from all the permutations
# e.g.
# mpi_params = { "np" : [ 1, 2 ] }
# exec = "/bin/hostname"
# prog_params = { "s" : [ '' ] }
# get_commands( mpi_params, exec, prog_params )
# output is:
#   mpirun -np 1 /bin/hostname -s
#   mpirun -np 2 /bin/hostname -s 
def get_commands( mpi_program, expr_mgmt_options, program_arguments=None,
          mpi_options=None, program_options=None, mpirun=None ): 

  if mpirun == None:
      mpirun = expr_mgmt_options["runcommand"]
  # takes an array of arrays
  # returns all possible permutations
  def args_to_list(array_of_args):
    commands = [ "" ]
    if array_of_args is None: return commands
    for args in array_of_args:
      num = len(commands)
      for i in range( 0, num ):
        command = commands.pop(0)
        for arg in args: commands.append( "%s %s" % ( command, arg ) )
    return commands

  # takes a dictionary of options : [ args ]  
  # returns a list of all possible permutations 
  def opts_to_list(params):
    commands = [ "" ]
    if params is None: return commands
    for param in params:
      num = len(commands)
      for i in range( 0, num ):
        command = commands.pop(0)
        if ( type(params[param]) != type(list()) ):
          raise TypeError( "Problem: param %s needs to be a list not a %s" 
                  % ( param, type(params[param] ) ) )
        for arg in params[param]:
          commands.append( "%s -%s %s" % (command, param, str(arg)) )
    return commands

  def executable(fpath):
    return os.path.exists(fpath) and os.access(fpath, os.X_OK)

  def die(s):
    print >> sys.stderr, "FATAL: " + s

  if not executable(mpi_program):
    die( mpi_program + " is not a valid executable." )
    return

  commands = [ ]
  for mpi_opt in opts_to_list(mpi_options):
    for pro_opt in opts_to_list(program_options):
      for pro_arg in args_to_list(program_arguments):
        command = "%s %s %s %s %s" % \
                    ( mpirun, mpi_opt, mpi_program, pro_opt, pro_arg )
        commands.append( re.sub( "\s+", " ", command ).strip() ) # trim space 
  return commands

##
## The following function is no longer in use but the preserved code is to
## document the database options that were in use at one time. The database
## capabilities are no longer included in experiment_management. Machine
## specifications have been moved to a config file.
#
# this function is a way to define cluster specific arguments for msub
# on your cluster, set MY_MPI_HOST env var to 'key', then add 'key' in here
#def system_default( spec ):
#  systems = {
#    "cielos"  :
#      { "ppn"        : 16,
#        "nodes"      : 6704,
#        "msub"       : '-j oe',
#        "filesystem" : '/panfs/scratch4/',
#        "runcommand" : 'aprun',
#        "chain"      : False,
#        "walltime"   : None,
#        "outdir"     : "output",
#        "host"       : "phpmyadmin.lanl.gov",
#        "passwd"     : "hpciopwd",
#        "user"       : "cron",
#        "db"         : "mpi_io_test_pro",
#        "table"      : "experiment",
#      }, 
##    None    : 
##      { "ppn"        : 2, 
##        "nodes"      : 1,   
##        "chain"      : False,
##        "msub"       : '-j oe',
##        "filesystem" : '/tmp',
##        "walltime"   : None,
##        "outdir"     : "output",
##        "host"       : "phpmyadmin.lanl.gov",
##        "passwd"     : "hpciopwd",
##        "user"       : "cron",
##        "db"         : "mpi_io_test_pro",
##        "table"      : "experiment",
##      },
#  }

##
## The following function is no longer used but is kept here to preserve the
## code. It had to do with database logging of the commands run by
## experiment_management.
#
## where to stash info about how to query databases in order to recreate
## previously run commands, also how to figure out how much walltime they used
#def get_database_field( table, item ):
#  tables = {
#    "experiment" :
#      { "np"       : "num_hosts",
#        "command"  : "full_args",
#        "walltime" : "write_total_time_max + read_total_time_max",
#      },
#    "mdtest" :
#      { "np"       : "num_tasks",
#        "command"  : "command_line",
#        "walltime" : "walltime",
#       },
#    "ior" :
#      { "np"       : "num_tasks",
#        "command"  : "command_line",
#        "walltime" : "write_time_mean+read_time_mean",
#      },
#  }
#  try: return tables[table][item]
#  except KeyError: return None

