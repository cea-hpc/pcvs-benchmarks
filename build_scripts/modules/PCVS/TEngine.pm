package PCVS::TEngine;

use strict;
use warnings;

use Exporter;
use Algorithm::Loops qw(NestedLoops); # time saver
use Module::Load qw(load autoload); #dynamic module loading
use Data::Dumper; #used for debug
use XML::Writer; #XML parser
use PCVS::Helper;
use POSIX; # for maths
use YAML qw(LoadFile DumpFile); # YAML parser
#$YAML::numify = 1;
use vars qw(@ISA @EXPORT @EXPORT_OK);

@ISA = 'Exporter';
@EXPORT = qw(engine_init engine_fini engine_unfold_file);
@EXPORT_OK = qw();

my $sysconf;
my $loaded_mod;
my %sys_iterlist = ();
my %sys_limits = ();
my @sys_iterlist_names = (); # list of iterator names
my $debug_file;

# utility to remove duplicate from an array
sub uniq {
	my %seen;
	return grep { !$seen{$_}++ } @_;
}

sub engine_debug
{
	my ($str) = @_;
	print $debug_file $str if $debug_file;
}

sub engine_unfold_iterator
{
	my ($iter_name, @iter_list) = @_;
	my (@list_values) = ();

	foreach my $el(@iter_list )
	{
		# if matching a:b:[+*^]c pattern
		if($el =~ /^([0-9]+):([0-9]+):([+*^])?([0-9]+)*$/)
		{
			my ($min, $max, $op, $step, $prev);
			$min = $1;
			$max  = $2;
			$op = (defined $3) ? $3 : "+";
			$step = (defined $4) ? $4 : "1";
			
			if($op eq "^")
			{
				$min = ceil($min ** (1/$step));
				#troubles with computer precision here...
				$max = floor($max ** (1.0/$step));
				push @list_values, map {$_ ** $step } $min..$max;
			}
			else
			{
				while($min <= $max) { push @list_values, $min; $min=eval("$min $op $step"); }
			}
		}
		#else if matching numeric or a-b pattern
		elsif($el =~ /^([0-9]+)(-([0-9]+))*$/)
		{
			my ($min, $max);
			$min = $1; $max = (defined $2) ? $3 : $min;
			while($min <= $max)
			{
				push @list_values, $min;
			} continue {$min++;}
		}
		#system conf is loaded with a boundary
		elsif($el =~ /^(<|<=|>|>=) *([0-9]+)$/)
		{
			my $op = $1;
			my $bound = $2;
			my @ev = grep { eval "$_ $op $bound" } @{ $sys_iterlist{$iter_name}};
			push @list_values, @ev;
		}
		else
		{
			push @list_values, $el;
		}
	}

	return () if( scalar @list_values == 0);
	#remove duplicate and sort the array (no sensible overhead, small arrays here...)
	# may be optimized to avoid distinctions between numerical and non-numerical arrays
	if($iter_name =~ /^n_/)
	{
		@list_values = uniq(sort { $a <=> $b } @list_values );
	}
	else
	{
		@list_values = uniq(sort @list_values);
	}
	return @list_values;
}

sub engine_init
{
	($sysconf) = @_;

	#very important : load runtime module validator.
	die("Unable to find a valid Module !") if(!exists $sysconf->{'runtime'}{'module'});
	$loaded_mod = "PCVS::Validate::$sysconf->{'runtime'}{'module'}";
	load($loaded_mod);

	#first, remove iterators not used by the configuration
	my @tmp;
	foreach (keys $sysconf->{'iterators'})
	{
		#if iterator does not exist or not defined by the runtime, the iterator is skipped
		next if(! exists $sysconf->{"runtime"}{$_}{'key'} or !$sysconf->{"runtime"}{$_}{'key'});
		push @sys_iterlist_names, $_;
	}

	#create an hashmap to associate the iterator with the 
	#once we cleaned up, let the runtime remember the iterator sequence
	$loaded_mod->runtime_init($sysconf);
	
	#... and get back system definition for each iterator
	foreach my $iter_name(@sys_iterlist_names)
	{
		my @sys_list = engine_unfold_iterator($iter_name,@{ $sysconf->{'iterators'}{$iter_name}});
		$sys_limits{$iter_name} = [$sys_list[0], $sys_list[$#sys_list]];
		push @{ $sys_iterlist{$iter_name}}, @sys_list;
	}
}

sub engine_build_testname
{
	my ($keys, @combination) = @_;
	my $name;
	foreach (0..$#combination)
	{
		$name .= "_".$sysconf->{'naming'}{$keys->[$_]}.$combination[$_];
	}
	return $name;
}

sub engine_TE_combinations
{
	my ($tn, $te) = @_;

	my $cur = $te;
	my %local_iterlist;
	my @local_iterlist_names;
	my $nparent = 0;
	
	engine_debug("\n######################\n");
	engine_debug("Processing $tn:\n");

	#first we look for final state of each iterator
	while(defined $cur)
	{
		foreach(@sys_iterlist_names)
		{
			# current level does not define the iterator
			next if (!exists $cur->{$_});

			#if exists but set to undef -> disabled iterator
			if(!defined $cur->{$_})
			{
				engine_debug("\t($nparent-degree) '$_' disabled\n");
				$local_iterlist{$_} = undef;
			}
			# exists and is defined
			else
			{
				die("Iterators should be array constructs (err with $tn)") if(ref($cur->{$_} ne "ARRAY"));
				die("What did you want to do by providing empty array ? Please refer to the doc !") if(!@{$cur->{$_}});
				if(!exists $local_iterlist{$_})
				{
					
					engine_debug("\t($nparent-degree) '$_' set: [".join(", ", @{ $cur->{$_} })."]\n");
					$local_iterlist{$_} = $cur->{$_};
				}
				elsif(!defined $local_iterlist{$_})
				{
					engine_debug("\t($nparent-degree) '$_' previously disabled by parent !\n");
					die("Found a 'enabled/disabled' iterator '$_' of $tn ($nparent-degree). Please refer to the documentation for this special case");
				}
				else
				{
					engine_debug("\t($nparent-degree) '$_' ignored: [".join(", ", @{ $cur->{$_}})."]\n");
				}
			}
		}
		
		$cur = $cur->{herit} || undef;
		$nparent++;
	}

	engine_debug("\nFinal values:\n");
	
	my (@tmp, @local_combinatory, $n) = ();
	#now, we set user-undefined iterators with system ones. If an iterator is empty ('[]'), it will be removed
	foreach my $name(@sys_iterlist_names)
	{
		#set to default if it does not exist
		($local_iterlist{$name} = $sys_iterlist{$name}) if(!exists $local_iterlist{$name});
		
		# skip this iterator if has been disabled
		if(!defined $local_iterlist{$name})
		{
			delete $local_iterlist{$name};
			next;
		}

		#from this point, the iterator has to exist.
		#If, at any moment, the iterator becomes empty, we have to skip this TE because
		#there are no intersection between what the user wants and the system requires

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
		
		#save this value list into the "array of iterators" (used w/ NestedLoops)
		push @tmp, \@val_sequence;
		push @local_iterlist_names, $name;
	}

	#build all the combinations
	NestedLoops(\@tmp, sub { $n++; push @local_combinatory, [ @_ ] if($loaded_mod->runtime_valid(\@local_iterlist_names, @_));} );
	engine_debug("===> ".scalar @local_combinatory." combinations kept (over $n)\n");
	
	#return two things:
	# the list of iterator names (the keys) mapping each combination value to the associated iterator
	# the array of combinations (array of arrays)
	return (\@local_iterlist_names, \@local_combinatory);

}

sub engine_convert_to_cmd
{
	my ($keys, @c) = @_;
	my ($pre_env, $post_args) = ();
	foreach (0..$#c)
	{
		my $param_name = $keys->[$_];
		my $prefix_name = $sysconf->{runtime}{$param_name}{key} || undef;
		my $trad_name = $sysconf->{'runtime'}{$param_name}{"$c[$_]_val"} || undef;
		my $value = $prefix_name;

		$value .= (defined $trad_name) ? $trad_name : $c[$_];
		$value .= " ";

		if(lc($sysconf->{'runtime'}{$param_name}{'usage'}) eq "environment")
		{
			$pre_env .= $value;
		}
		else
		{
			$post_args .= $value;
		}
	}
	return ($pre_env || "", $post_args || "");
}

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

sub engine_get_value_ifdef
{
	my ($node, $key) = @_;

	# Look for local field
	my $value = $node->{$key};
	my $parent = $node->{'herit'};


	return $value if(defined $value);
	#check recursively if parent has it
	return engine_get_value_ifdef($parent, $key) if( defined $parent);

	#default value
	return undef;
}

sub engine_check_foldable
{
	my ($tname, $ttype, $omp, $tbb) = @_;
	
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
	return 1;
}

sub engine_unfold_test_expr
{
	#params
	my  ($xml, $tname,  $tvalue, $bpath) = @_;
	#global var for a test_expr
	my ($name, $bin, $command, $args, $arg_omp, $arg_tbb, $rc, $time, $delta, $constraint, $timeout, @deps) = ();
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
	
	return if (!engine_check_foldable($tname, $ttype, $arg_omp, $arg_tbb));

	if($ttype =~ m/^(build|complete)$/)
	{
		$constraint = "compilation";
		my $target = engine_get_value_ifdef($tvalue, 'target');
		my $files = engine_get_value_ifdef($tvalue, 'files') || $tname;
		my $cflags = $sysconf->{compiler}{cflags} || "";
		$args = engine_get_value_ifdef($tvalue, 'cargs') || "";

		$args .= " $sysconf->{compiler}{openmp}" if($arg_omp eq 'true');
		$args .= " $sysconf->{compiler}{tbb}" if($arg_tbb eq 'true');
		
		if(defined $target) # if makefile
		{
			die("'files' field not found for $tname !") if(! defined $files);

			(my $makepath = $files) =~ s,/[^/]*$,,;
			(my $makefile = $files) =~ s/^$makepath\///;
			$command = "make -f $makefile -C $makepath $target PCVS_CC=\"$sysconf->{compiler}{c}\" PCVS_CXX=\"$sysconf->{compiler}{cxx}\" PCVS_FC=\"$sysconf->{compiler}{f77}\" PCVS_CFLAGS=\"$cflags $args\"";
		}
		else
		{
			my $comp_name = helper_detect_compiler($files);
			die("Unable to find a valid compiler for $tname !") if (!$comp_name);
			die("No compiler found for $comp_name !") if(!exists $sysconf->{compiler}{$comp_name});

			$command = "$sysconf->{compiler}{$comp_name} $cflags $args -o $bin $files";
		}
		engine_gen_test($xml, $tname, $command, $rc, $time, $delta, $constraint, @deps);
	}

	if($ttype =~ m/^(run|complete)$/)
	{
		my $args = engine_get_value_ifdef($tvalue, 'args') || "";
		my $launcher = $sysconf->{'runtime'}{'cmd'} || "";
		my $extra_args = $sysconf->{'runtime'}{'args'} || "";
		$timeout = "$sysconf->{runtime}{'timeout-prefix'}$timeout" if($timeout and exists $sysconf->{runtime}{'timeout-prefix'} and $sysconf->{runtime}{'timeout-prefix'} ne "");
			
		#special case : complete -> auto-create the dependency between compilation and exec
		push @deps, $tname if($ttype =~ /^complete$/);

		$constraint = undef;

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

sub engine_unfold_file
{

	my ($bpath, $ftree, $filepath) = @_;
	my $filestream = LoadFile("$filepath");

	#ready to write list_of_tests
	open(my $xml_file, ">", "$bpath/list_of_tests.xml");

	if($sysconf->{'engine-debug'})
	{
		open($debug_file, ">", "$filepath.log") or die ("Debug file !");
		print $debug_file "##################\nSystem combinations:\n";
		foreach(keys %sys_iterlist)
		{
			print $debug_file "\t$_:\t[".join(", ", @{ $sys_iterlist{$_} })."]\n";
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


sub engine_fini
{
	$loaded_mod->runtime_fini();
}

1;
