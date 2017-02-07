#! /bin/tcsh 
# whatever this spits out in the form of # key val
# can be parsed by the fs_test and will be
# injected into the DB (so long as the key exists in the schema)
# the format is key [space] val
# currently the val can't have spaces in it...
# to pull this into the DB through the fs_test, set the
# FS_TEST_EXTRA environment variable to point to this file
#
# This script uses the tpf_panfs.x application to gather panfs-specific
# parameters, such as components, stripe-size, stripe-width, etc.
# Because people may have the source tree in different locations, we
# use the environment varioable TPF_ROOT to define the directory path
# down to the "tpf" directory of the code repository. Under "tpf"
# the actual application is in tpf, so $TPF_ROOT/tpf/tpf_panfs.x


# set up
set target = $1
if ("X$target" == "X" ) then
  echo "error bad_args_no_target_specified"
  exit 1
endif

# if the user specified an fs:/ notation for MPI-IO, then strip it
set target = `echo $target | sed 's/.*://g'`
set target_dir = $target:h
set target_mnt = `echo $target | tr "/" " " | awk '{print $2}'`

set tpf = $TPF_ROOT/tpf/tpf_panfs.${MY_MPI_HOST}.x

set panfs_trace1 = /usr/sbin/panfs_trace
set panfs_trace2 = /usr/local/sbin/panfs_trace

# grab the ionode list
if ( -x /sbin/ip ) then
  set ionodes = `/sbin/ip route | awk '/nexthop/ {print $3}' | sort | uniq`
  set num_ionodes = `echo $ionodes | wc -w`
  set ionodes = `echo "$ionodes" | tr ' ' ','`
  echo "ionodes $ionodes"
  echo "num_ionodes $num_ionodes"
endif

# grab the panfs mount options
# if panfs has multiple mounts, this might get the wrong one....
set panfs_mnt = `mount -t panfs | tr '\n' '|' | tr ' ' '_'`
echo "panfs_mnt $panfs_mnt"

# get panfs client version
if (-x $panfs_trace1 ) then
    set client_version = `$panfs_trace1 --version $target_dir | awk '{print $4$5}' | head -1`
    echo "panfs $client_version"
else if (-x $panfs_trace2 ) then
    set client_version = `$panfs_trace2 --version $target_dir | awk '{print $4$5}' | head -1`
    echo "panfs $client_version"
else 
    # no error if panfs can't be discovered
    #echo "error couldnt_discover_panfs_version"
endif

# find great-grandparent pid (mpirun->app->shell->this)
set ppid = `ps -o user,pid,ppid ax | egrep "$USER\s+$$" | awk '{print $3}'`
set gpid = `ps -o user,pid,ppid ax | egrep "$USER\s+$ppid" | awk '{print $3}'`
set ggpid = `ps -o user,pid,ppid ax | egrep "$USER\s+$gpid" | awk '{print $3}'`

# count children of great-grandparent
# TOSS/slurm machines need to grep -v srun first
set children = `ps -o ppid,args | grep -v srun | awk '{print $1}' | grep $ggpid | wc -w`
echo "procs_per_node $children" 

# get job id 
set job_id = `env | grep JOBID | sed 's/=/ /' | awk '/PBS_JOBID/ {print $2}'`
echo "jobid $job_id"

# get thread count
set thread_count = `ps auxw | grep pan | grep kpanfs_thpool | grep -v grep | wc -l`
echo "panfs_threads $thread_count"

# get some plfs parameters if possible
plfs_version >& ./version.out
set plfs_ver = `cat ./version.out | grep -A 1 'PLFS library' | grep Built | awk '{print $2}' | sed -e 's/"//'`
rm -f ./version.out
echo "plfs_version $plfs_ver"

foreach file_plfsrc ( $HOME/.plfsrc /etc/plfsrc ) 
  if ( -e $file_plfsrc ) then
    set file = $file_plfsrc
    # compare target directory to plfs directoy and deterime if plfsrc has 
    # include statements
    set match_line = `grep $target_mnt $file`
    set check_include = `echo $match_line | awk '{print $1}'`
    if ( $check_include == "include" ) then
      set file = `echo $match_line | awk '{print $2}'`
    endif

    set plfs_mnt = `awk '/^mount_point/ {print $2}' $file | tail -1`
    #set plfs_mnt = `grep 'mount_point' $file | awk '{print $2}'`
    awk \
      '/^num_hostdirs/ {print "plfs_hostdirs "$2} \
       /^threadpool_size/ {print "plfs_threads "$2} \
       /^backends/ {print "plfs_backends "$2} \
       /^mount_point/ {print "plfs_mnt "$2} \
       /^map/ {print "plfs_map "$2} \
      ' $file
    break
  endif
end

# some memory numbers
set committed = `grep Committed_AS /proc/meminfo | awk '{print $2}'`
echo committed_memory $committed

# get df and memory numbers
#echo "# target_dir is $target_dir"
if ( -d $target_dir ) then
  set df_perc = `df $target_dir -P | tail -1 | awk '{print $5}' | sed s/%//`
  set df_tot  = `df $target_dir -P | tail -1 | awk '{print $2}'`
  echo "df_perc_before $df_perc"
  echo "df_tot_before $df_tot"
  if ( -x $tpf ) then
    $tpf default $target_dir |& awk \
      '/Components/ {print "panfs_comps "$5} \
      /RAID width/ {print "panfs_width "$3} \
      /Depth/      {print "panfs_depth "$2} \
      /Stride/     {print "panfs_stripe "$3} \
      /Layout Policy/ {print "panfs_visit "$3} \
      /Layout Type/     {print "panfs_type "$3}  \
      '
    else 
      #echo "error no_valid_tpf_executable"
    endif
else
  # possible that we're using a plfs path on a machine with plfs-adio but
  # not a plfs mount point
  # if so, don't error out
  echo $target_dir | grep -q plfs
  if ( $? == 1 ) then
    echo "error no_valid_target_dir_$target_dir"
  endif
endif
