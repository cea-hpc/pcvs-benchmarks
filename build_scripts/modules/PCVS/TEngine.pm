package PCVS::TEngine;

use strict;
use warnings;
use 5.010;

use Exporter;
use PCVS::Helper;                     # PCVS Helper (paths...)
use Algorithm::Loops qw(NestedLoops); # Used for building combinations from iterators
use Module::Load qw(load autoload);   # Dynamic module loading
use XML::Writer;                      # XML parser (output)
use YAML qw(LoadFile DumpFile);       # YAML parser (input)
use POSIX;                            # Used for rounding (maths)
use Data::Dumper;                     # Used for debug, printing data structures
use vars qw(@ISA @EXPORT @EXPORT_OK);

@ISA = 'Exporter';
@EXPORT = qw(engine_init engine_fini engine_unfold_file);
@EXPORT_OK = qw();

my $sysconf;                 # system configuration (set at init())
my $loaded_mod;              # reference to the runtime validation module
my %sys_iterlist = ();       # system iterator list (set at init())
my %sys_limits = ();         # system iterator limits (set at init())
my @sys_iterlist_names = (); # system iterator names
my $debug_file;              # debug file, where *.yml.log are written

###########################################################################
# write parameter string into the current debug file
# Args:
#    - $str : the string to flush
sub engine_debug
{
	my ($str) = @_;
	print $debug_file $str if $debug_file;
}

###########################################################################
# Initialize the engine
# Args:
#   - $sysconf: a reference to the system configuration
sub engine_init
{
	($sysconf) = @_;

	#load runtime-specific Perl module to validate combinations
	$loaded_mod = "PCVS::Validate::".($sysconf->{'runtime'}{'module'} || "Default");
	load($loaded_mod);

	#first, remove iterators not used by the configuration.
	foreach (keys $sysconf->{'iterators'})
	{
		#if iterator does not exist or not defined by the runtime, the iterator is skipped
		next if(! exists $sysconf->{"runtime"}{$_}{'key'} or !$sysconf->{"runtime"}{$_}{'key'});
		push @sys_iterlist_names, $_;
	}

	#create an hash to associate the iterator with the 
	#once we cleaned up, let the runtime remember the iterator sequence
	$loaded_mod->runtime_init($sysconf);
	
	#... and get back system definition for each iterator
	foreach my $iter_name(@sys_iterlist_names)
	{
		if(defined $sysconf->{'iterators'}{$iter_name})
		{
			# build the sequence for the iterator
			my @sys_list = engine_unfold_iterator($iter_name,@{ $sysconf->{'iterators'}{$iter_name}});
			# register iterator limits (for boundary checking)
			$sys_limits{$iter_name} = [$sys_list[0], $sys_list[$#sys_list]];
			push @{ $sys_iterlist{$iter_name}}, @sys_list;
		}
		else
		{
			#iterator explicitly disabled by user configuration
			$sys_iterlist{$iter_name} = undef;
		}
	}
}

###########################################################################
# Un-roll a specific iterator: For each values of the iterator, convert any
# pattern into a sequence of values (ex: convert "1:4:*2" to [1, 2, 4]).
# Three kind of transformation which can be done here are:
#   1. [ ]
#   2. [ A ], where A can be anything you want (even non-numeric values)
#   3. [ "A-B" ]
#   4. [ "A:B:xC"], where x is one of { '', '+', '*', '^'}
#   5. [ "yA" ], where y is one of {'>', '>=', '<=', '<'}
# Once sequence is generated, it is sorted and cleaned from duplicates
#
# Args:
#   - $iter_name: name of the iterator currently parsed.
#   - @iter_list: the value list for this iterator
sub engine_unfold_iterator
{
	# vars
	my ($iter_name, @iter_list) = @_;
	my (@list_values) = ();

	# for each element in the iterator -> unroll it
	foreach my $el(@iter_list )
	{
		# if value is matching 4.
		if($el =~ /^([0-9]+):([0-9]+):([+*^])?([0-9]+)*$/)
		{
			#$1, $2, $3, $4 are patterns mathing to atoms above
			my ($min, $max, $op, $step, $prev);
			$min = $1;
			$max  = $2;
			$op = (defined $3) ? $3 : "+";
			$step = (defined $4) ? $4 : "1";
			
			#special case, if we compute nth-power
			if($op eq "^")
			{
				# we have troubles here with rounding. Becasue of computing
				# the nth root is based on the power of (1/nth), we create an approximation
				# In some case, this can lead to skip the final value.
				# For example cubic_root(64) = 4. but floor() returns 3 because (64 ** (1/3)) returns 63.99999...
				# This should be fixed. For now, we recommend to increase by one the final value to avoid this case (bad)
				$min = ceil($min ** (1/$step));
				$max = floor($max ** (1.0/$step));

				# push all nth-power for each integer between root(min) and root(max)
				push @list_values, map {$_ ** $step } $min..$max;
			}
			else
			{
				# push all values between min and max which satisfy the step
				while($min <= $max) { push @list_values, $min; $min=eval("$min $op $step"); }
			}
		}
		
		# ELSE if value is matching 3. 
		elsif($el =~ /^([0-9]+)(-([0-9]+))*$/)
		{
			my ($min, $max);
			$min = $1; $max = (defined $2) ? $3 : $min;
			#Push the sequence
			push @list_values, $min..$max;
		}
		
		# ELSE if value is matching 5. 
		elsif($el =~ /^(<|<=|>|>=) *([0-9]+)$/)
		{
			my $op = $1;
			my $bound = $2;
			#create the list only with value maching '$value (>=|>|<=|<) limit'
			my @ev = grep { eval "$_ $op $bound" } @{ $sys_iterlist{$iter_name}};
			push @list_values, @ev;
		}

		# ELSE (value matching 2.
		else
		{
			push @list_values, $el;
		}
	}

	# if no values are stored, it means we found no intersection between system and user-defined sets. we returns an empty array
	return () if( scalar @list_values == 0);

	#remove duplicate and sort the array (no sensible overhead, small arrays here...)
	# may be optimized to avoid distinctions between numerical and non-numerical arrays
	if($iter_name =~ /^n_/)
	{
		@list_values = helper_uniq(sort { $a <=> $b } @list_values );
	}
	else
	{
		@list_values = helper_uniq(sort @list_values);
	}
	return @list_values;
}

###########################################################################
# Build the test name associated to the combination
# Args: 
#   - $keys: reference to iterator name array
#   - @combination: the combination
#
# Returns
# The test name
sub engine_build_testname
{
	my ($keys, @combination) = @_;
	my $name;

	#for each element 
	foreach (0..$#combination)
	{
		#retrieve how the user wants to prefix this element
		$name .= "_".$sysconf->{'naming'}{$keys->[$_]}.$combination[$_];
	}
	return $name;
}

###########################################################################
# Critical function: It aims to construct all combinations between valid iterator for a specific TE
# Args: 
#    - $tn: TE name from YAML
#    - $te: TE node from YAML
#
# Returns
# a tuple (\@a, \@b) where @a is the final iterator namelist and @b is list of combinations (array of arrays)
# This function can also return (undef, undef) if the current TE has been unregistered due to no valid combinations
sub engine_TE_combinations
{
	my ($tn, $te) = @_;

	my $cur = $te;
	my %local_iterlist;       # HASH storing the iterator list for each encountered iterator names
	my @local_iterlist_names; # list of iterator names
	my $nparent = 0;          # parent-level (used for debug)
	
	engine_debug("\n######################\n");
	engine_debug("Processing $tn:\n");

	#Browsing the TE
	while(defined $cur)
	{
		# for each existing iterator
		foreach(@sys_iterlist_names)
		{
			# if the current level does not define the iterator, skip it
			next if (!exists $cur->{$_});

			#if the iterator exists but set to undef, it means the iterator is disabled
			if(!defined $cur->{$_})
			{
				if(defined $local_iterlist{$_})
				{
					engine_debug("\t($nparent-degree) '$_' previously disabled by parent !\n");
					die("Found a 'enabled/disabled' iterator '$_' of $tn ($nparent-degree). Please refer to the documentation for this special case");
				}
				else
				{
					engine_debug("\t($nparent-degree) '$_' disabled\n");
					$local_iterlist{$_} = undef;
				}
			}

			# ELSE, the iterator is defined at this level
			else
			{
				#check the node provides an array
				die("Iterators should be array constructs (err with $tn)") if(ref($cur->{$_} ne "ARRAY"));
				# check the node does not contain an empty array
				die("What did you want to do by providing empty array ? Please refer to the doc !") if(!@{$cur->{$_}});

				#if we don't stored a list for this entry yet
				if(!exists $local_iterlist{$_})
				{
					
					engine_debug("\t($nparent-degree) '$_' set: [".join(", ", @{ $cur->{$_} })."]\n");
					$local_iterlist{$_} = $cur->{$_};
				}
				#ELSE, just ignore and log this entry for debug
				else
				{
					engine_debug("\t($nparent-degree) '$_' ignored: [".join(", ", @{ $cur->{$_}})."]\n");
				}
			}
		}
		
		#check if there are a parent for the current node
		$cur = $cur->{herit} || undef;
		$nparent++;
	}

	# from this point, we have all iterator list, but not unfolded. We have to convert them into value sequence and remove bad values

	engine_debug("\nFinal values:\n");
	
	my (@tmp, @local_combinatory, $n) = ();
	#We iterate over iterators
	foreach my $name(@sys_iterlist_names)
	{
		#If the TE does not define one of them, set it to default
		# If system configuration disables an iterator, disable it here too
		($local_iterlist{$name} = $sys_iterlist{$name}) if(!exists $local_iterlist{$name} || !defined $sys_iterlist{$name});
		
		# If, after that, the iterator is not defined (and so disabled), do not consider this iterator
		if(!defined $local_iterlist{$name})
		{
			delete $local_iterlist{$name};
			next;
		}

		#From this point, the iterator has to exist and provide at least one combination.
		#If not, it means that this TE cannot be generated according to the system configuration.

		#construct the iterator list --> coonvert sequence and generators into a value list
		my @val_sequence = engine_unfold_iterator($name, @{$local_iterlist{$name}});
		
		#find intersection betweeen user and system iterator values.
		#Be aware that engine_unfold_iterator() already restricts the list with the 'greater/lower' construct
		if ($name =~ /^n_/)
		{
			@val_sequence = grep {$_ >= $sys_limits{$name}[0] and $_ <= $sys_limits{$name}[1] } @val_sequence;
		}
		else
		{
			# ~~ means 'contains' (only >= perl 5.10 !)
			# SmartMatch is not bug-free (according to PerlMonks). This should probably be replaced.
			@val_sequence = grep { $_ ~~ @{$sys_iterlist{$name}} } @val_sequence;
		}
		
		#if the array is empty here, it means that the intersection between system and user configuration
		#provides no possible combinations ==> do not unfold this TE anymore
		if(@val_sequence eq 0)
		{
			engine_debug("===> $name raise: No intersection between system and [".join(", ", @{$local_iterlist{$name}})."] configuration.\n");
			return (undef, undef);
		}

		engine_debug("\t$name:\t [".join(", ", @val_sequence)."]\n");
		
		#Finally, tag this iterator as usable
		push @tmp, \@val_sequence;
		push @local_iterlist_names, $name;
	}

	# From this point, we own all unfolded iterator lists. Now, we have to build the combination.
	# This will be constructed thanks to @tmp, which is an array of arrays (constructed in iterator name order).
	# The next call will create an another array with combinations.
	# For example, if @tmp contains  : [ [1, 2, 3, 4], [A, B] ],
	# @local_combinatory will contain: [ [1, A], [1, B], [2, A], [2, B], [3, A], [3, B], [4, A], [4, B]]
	$n = 0;
	NestedLoops(\@tmp, sub { $n++; push @local_combinatory, [ @_ ] if($loaded_mod->runtime_valid(\@local_iterlist_names, @_));} );
	engine_debug("===> ".scalar @local_combinatory." combinations kept (over $n)\n");
	
	#return two things:
	# the list of iterator names (the keys) mapping each combination value to the associated iterator
	# the array of combinations (array of arrays)
	return (\@local_iterlist_names, \@local_combinatory);

}

###########################################################################
# Convert the combination sequence into a valid LINUX command
# Args: 
#   - $keys: list or iterator names for the combination
#   - @c : the combination
#
# Returns:
# Two elements: The string containing env vars to load and the string containing runtime-formated arguments list
sub engine_convert_to_cmd
{
	my ($keys, @c) = @_;
	my ($pre_env, $post_args) = ();
	# for each combination element
	foreach (0..$#c)
	{
		my $param_name = $keys->[$_];
		my $prefix_name = $sysconf->{runtime}{$param_name}{key} || undef;
		my $trad_name = $sysconf->{'runtime'}{$param_name}{"$c[$_]_val"} || undef;
		my $value = $prefix_name;

		#if the value have a key name '$value_val' then use this traductio instead of the initial value.
		#for example, we use 'shmem' keyword but OpenMPI only accept 'shm'.
		#The runtime configuration then defines: 'net: { shmem_val: 'shm'}.
		$value .= (defined $trad_name) ? $trad_name : $c[$_];
		$value .= " ";

		#if the system requires this element to be used as an environnment variable
		if(lc($sysconf->{'runtime'}{$param_name}{'usage'}) eq "environment")
		{
			$pre_env .= $value;
		}
		#else, by default, it is an command line argument
		else
		{
			$post_args .= $value;
		}
	}
	return ($pre_env || "", $post_args || "");
}

###########################################################################
# Build an XML test entry
# Args:
#   - $xml : the open FD to XML file (through XML::Writer)
#   - $name : test naem
#   - $command: test command
#   - $rc: test expected return code
#   - $time: expected time
#   - $delta: tolerance time limit
#   - $constraint: Is a compilation test ?
#   - @deps: list of dependencies for this test (can be undef)
sub engine_gen_test
{
	my ($xml, $name, $command, $rc, $time, $delta, $constraint, @deps) = @_;
	$xml->startTag("job");
	$xml->dataElement("name", "$name");
	$xml->dataElement("command", $command);
	$xml->dataElement("rc", $rc);
	$xml->dataElement("time", $time) if (defined $time);
	$xml->dataElement("delta", $delta) if(defined $delta);

	$xml->startTag("constraints");
	$xml->dataElement("constraint", $constraint) if(defined $constraint);
	$xml->endTag("constraints");

	$xml->startTag("deps");
	$xml->dataElement("dep", $_) foreach(@deps);
	$xml->endTag("deps");

	$xml->endTag("job");
}

###########################################################################
# Look for any value associated to the given key for the current node or one of its parents
# Args:
#    - $node : the node where we start to look for
#    - $key : the key to find
#
# Returns:
# the value associated to the key if exist, undef otherwise.
sub engine_get_value_ifdef
{
	my ($node, $key) = @_;

	# Look for local field
	my $value = $node->{$key};
	my $parent = $node->{'herit'};

	#Check if current node has it
	return $value if(defined $value);
	#then check recursively if parent has it
	return engine_get_value_ifdef($parent, $key) if( defined $parent);

	#default value
	return undef;
}

###########################################################################
# This function check if given system meets requirement to execute this TE
# Args:
#    - $tname: TE name
#    - $ttypoe: TE type
#    - $omp: TE OpenMP support
#    - $tbb: TE TBB Support
#
# Returns:
# 1 if TE would be executed, 0 otherwise
sub engine_check_foldable
{
	my ($tname, $ttype, $omp, $tbb, $accl) = @_;
	
	if(!($ttype =~ /^(build|run|complete)$/))
	{
		print ("       Skipping $tname: bad type '$ttype'\n");
		return 0;
	}
	
	if(!$sysconf->{compiler}{openmp} and $omp eq "true")
	{
		print ("       Skipping $tname: OpenMP not enabled for $sysconf->{compiler}{target}\n");
		return 0;
	}
	
	if(!$sysconf->{compiler}{tbb} and $tbb eq "true")
	{
		print ("       Skipping $tname:  TBB not supported for $sysconf->{compiler}{target}\n");
		return 0;
	}
	
	if(!$sysconf->{compiler}{accl} and $accl eq "true")
	{
		print ("       Skipping $tname:  Accelerators not supported for $sysconf->{compiler}{target}\n");
		return 0;
	}
	return 1;
}

###########################################################################
# Unfold One TE and write the associated test cases into XML files
# Args:
#    - $xml : the XML reference where data will go
#    - $tname : TE name (key)
#    - $tvalue : TE node (value)
#    - $bpath : Current build path
sub engine_unfold_test_expr
{
	#params
	my  ($xml, $tname,  $tvalue, $bpath) = @_;
	#global var for a test_expr
	my ($name, $bin, $command, $args, $arg_omp, $arg_tbb, $arg_accl, $rc, $time, $delta, $constraint, $timeout, @deps) = ();
	#other vars
	my $ttype = lc(engine_get_value_ifdef($tvalue, 'type') || "run");

	#common params, whatever the TE type
	$time    = engine_get_value_ifdef($tvalue, 'limit') || undef;
	$delta   = engine_get_value_ifdef($tvalue, 'tolerance' ) || undef;
	$rc   = engine_get_value_ifdef($tvalue, 'returns' ) || 0;
	@deps    = @{ engine_get_value_ifdef($tvalue, 'deps') || [] };
	$bin = "$bpath/".(engine_get_value_ifdef($tvalue, 'bin') || $tname);
	$timeout = engine_get_value_ifdef($tvalue, "timeout") || $sysconf->{validation}{timeout} || "";
	$arg_omp = engine_get_value_ifdef($tvalue, 'openmp') || "false"; 
	$arg_tbb = engine_get_value_ifdef($tvalue, 'tbb') || "false"; 
	$arg_accl= engine_get_value_ifdef($tvalue, 'accl') || "false"; 

	#check if the TE is usable within the current configuration
	return if (!engine_check_foldable($tname, $ttype, $arg_omp, $arg_tbb, $arg_accl));

	# if the current should be compiled
	if($ttype =~ m/^(build|complete)$/)
	{
		# tag it as a compilation
		$constraint = "compilation";

		#retrieve some params
		my $target = engine_get_value_ifdef($tvalue, 'target');
		my $files = engine_get_value_ifdef($tvalue, 'files') || $tname;
		my $cflags = $sysconf->{compiler}{cflags} || "";
		$args = engine_get_value_ifdef($tvalue, 'cargs') || "";

		$args .= " $sysconf->{compiler}{openmp}" if($arg_omp eq 'true');
		$args .= " $sysconf->{compiler}{tbb}" if($arg_tbb eq 'true');
		$args .= " $sysconf->{compiler}{accl}" if($arg_accl eq 'true');

		# if it should be a makefile
		if(defined $target)
		{
			die("'files' field not found for $tname ! (Makefile)") if(! defined $files);

			(my $makepath = $files) =~ s,/[^/]*$,,;
			(my $makefile = $files) =~ s/^$makepath\///;
			$command = "make -f $makefile -C $makepath $target PCVS_CC=\"$sysconf->{compiler}{c}\" PCVS_CXX=\"$sysconf->{compiler}{cxx}\" PCVS_CU=\"$sysconf->{compiler}{cu}\" PCVS_FC=\"$sysconf->{compiler}{f77}\" PCVS_CFLAGS=\"$cflags $args\"";
		}
		#else, simple compilation
		else
		{
			#look for input source file and try to detect the good compiler
			my $comp_name = helper_detect_compiler($files);
			die("Unable to find a valid compiler for $tname !") if (!$comp_name);
			die("No compiler found for $comp_name !") if(!exists $sysconf->{compiler}{$comp_name});

			#build the command
			$command = "$sysconf->{compiler}{$comp_name} $cflags $args -o $bin $files";
		}
		#generate the XML entry
		engine_gen_test($xml, $tname, $command, $rc, $time, $delta, $constraint, @deps);
	}

	# if the current should be run
	if($ttype =~ m/^(run|complete)$/)
	{
		#retrieve some TE infos
		$constraint = undef;
		my $args = engine_get_value_ifdef($tvalue, 'args') || "";
		my $launcher = $sysconf->{'runtime'}{'cmd'} || "";
		my $extra_args = $sysconf->{'runtime'}{'args'} || "";
		
		#disable timeout if not defined or not supported by runtime
		$timeout = ($timeout and exists $sysconf->{runtime}{'timeout-prefix'} and $sysconf->{runtime}{'timeout-prefix'} ne "") ? "$sysconf->{runtime}{'timeout-prefix'}$timeout" : "";
			
		#special case : if type is 'complete' -> autocreate the dependency between compilation and exec
		push @deps, $tname if($ttype =~ /^complete$/);

		#do the job...
		my ($it_keys, $it_comb) = engine_TE_combinations($tname, $tvalue);

		#no possible combinations found --> skip the TE
		#TODO: also remove the previously added compilation
		return if (!defined $it_comb);

		# for each selected combinations
		foreach(0..(scalar @{$it_comb} -1 ))
		{
			# build the test name
			$name    = "$tname".engine_build_testname($it_keys, @{ $it_comb->[$_]} );
			#parse arguments and options depending on runtime
			my ($pre_env, $post_args) = engine_convert_to_cmd($it_keys, @{ $it_comb->[$_] });
			$command = "$pre_env $launcher $post_args $timeout $extra_args $bin $args";
			#push the test into XML file
			engine_gen_test($xml, $name, $command, $rc, $time, $delta, $constraint, @deps);
		}
	}
}

###########################################################################
# Parse one YAML file. This function could be implemented in thread-safe mode (warning about global vars)
# Args:
#    - $bpath: current build path, where XML file will be built
#    - $ftree: package name for the current XML file (=directory tree) 
#    - $filepath: path to the YAML file
sub engine_unfold_file
{

	my ($bpath, $ftree, $filepath) = @_;
	my $filestream = LoadFile("$filepath");

	#ready to write list_of_tests
	open(my $xml_file, ">", "$bpath/list_of_tests.xml");

	#if engine-debug mode
	if($sysconf->{'engine-debug'})
	{
		open($debug_file, ">", "$filepath.log") or die ("Debug file !");
		print $debug_file "##################\nSystem combinations:\n";
		foreach(keys %sys_iterlist)
		{
			print $debug_file "\t$_:\t"; 
			print $debug_file ((defined $sys_iterlist{$_}) ? "[".join(", ", @{ $sys_iterlist{$_} })."]" : "disabled")."\n";
		}

	}

	my $xmlwriter = XML::Writer->new(OUTPUT => $xml_file, NEWLINES => 0);
	$xmlwriter->startTag("jobSuite", "package" => grep {s/\//\./g} $ftree);

	# parse each test
	foreach my $test(keys %{ $filestream })
	{
		# skip test template
		next if($test =~ m/^pcvs.*$/);
		engine_unfold_test_expr($xmlwriter, $test, $filestream->{$test}, $bpath);
	}

	#close the file
	$xmlwriter->endTag("jobSuite");
	$xmlwriter->end();
	close($xml_file);
	close($debug_file) if($debug_file);
}

###########################################################################
# Unload the engine. Free the runtime module
sub engine_fini
{
	$loaded_mod->runtime_fini();
}

1;
