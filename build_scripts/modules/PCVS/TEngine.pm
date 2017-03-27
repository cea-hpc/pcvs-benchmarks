package PCVS::TEngine;

use strict;
use warnings;

use Exporter;
use Algorithm::Loops qw(NestedLoops); # time saver
use Module::Load qw(load autoload); #dynamic module loading
use Data::Dumper; #used for debug
use XML::Writer; #XML parser
use PCVS::Helper;
use YAML qw(LoadFile); # YAML parser
$YAML::numify = 1;
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
	my (@list_values);

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
				$min = int($min ** (1/$step));
				$prev = $min;
			}

			while($min <= $max)
			{
				push @list_values, $min;
			} 
			continue 
			{
				#switch-case does not natively exist w/ Perl
				if   ($op eq "+") {$min+=$step;}
				elsif($op eq "*") {$min*=$step;}
				elsif($op eq "^") {$prev+=1; $min=$prev**$step;}
				else {die("$iter_name: Unknown operator '$_'");}
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
		# else if a numeric iter and not fallen in previous conditions --> abort
		elsif($iter_name =~ /^n_/)
		{
			die("$iter_name only takes numeric, 'a:b:c' or 'a-b' interval values !!");
		}
		else
		{
			push @list_values, $el;
		}
	}

	return [] if( scalar @list_values == 0);
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
			next if (!defined $cur->{$_});

			# if the iterator is encountered for the first time
			if(!exists $local_iterlist{$_})
			{
				engine_debug("\t($nparent-degree) '$_' set [".join(", ", @{ $cur->{$_} })."]\n");
				$local_iterlist{$_} = $cur->{$_};
			}
			#if the iterator is already registered and the current one is empty -> special case
			elsif(scalar @{$cur->{$_}} == 0 and scalar @{$local_iterlist{$_}})
			{
				engine_debug("\t($nparent-degree) '$_' set: empty list !\n");
				die("Found an empty parent for '$_' of $tn ($nparent-degree). Please refer to the documentation for this special case");
			}
			else # just override it
			{
				engine_debug("\t($nparent-degree) '$_' ignored: [".join(", ", @{ $cur->{$_}})."]\n");
			}
			
		}
		
		$cur = $cur->{herit} || undef;
		$nparent++;
	}

	engine_debug("\nFinal iterator value override:\n");
	
	my (@tmp, @local_combinatory, $n) = ();
	#now, we set user-undefined iterators with system ones. If an iterator is empty ('[]'), it will be removed
	foreach my $name(@sys_iterlist_names)
	{
		#set to default if undefined
		$local_iterlist{$name} = $sys_iterlist{$name} if(!exists $local_iterlist{$name});
		
		#"unfold" the iterator to get generated list: parse each element and build the value list
		my @seq = engine_unfold_iterator($name, @{$local_iterlist{$name}});
		(@seq = grep {$_ > $sys_limits{$name}[0] and $_ < $sys_limits{$name}[1] } @seq) if ($name =~ /^n_/); 
		
		#remove this iterator if empty
		(delete $local_iterlist{$name} and next ) if(!@seq);


		engine_debug("\t$name:\t [".join(", ", @seq)."]\n");
		
		#save this value list into the "array of iterators" (used w/ NestedLoops)
		push @tmp, \@seq;
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

sub engine_unfold_test_expr
{
	#params
	my  ($xml, $tname,  $tvalue, $bpath) = @_;
	#global var for a test_expr
	my ($name, $bin, $command, $args, $rc, $time, $delta, $constraint, @deps) = ();
	#other vars
	my $ttype = lc(engine_get_value_ifdef($tvalue, 'type') || "run");

	#common params, whatever the TE type
	$time    = engine_get_value_ifdef($tvalue, 'limit') || undef;
	$delta   = engine_get_value_ifdef($tvalue, 'tolerance' ) || undef;
	$rc   = engine_get_value_ifdef($tvalue, 'returns' ) || 0;
	@deps    = @{ engine_get_value_ifdef($tvalue, 'deps') || [] };
	$args = engine_get_value_ifdef($tvalue, 'args') || "";
	$bin = "$bpath/".(engine_get_value_ifdef($tvalue, 'bin') || $tname);

	if($ttype =~ m/^(build|complete)$/)
	{
		$constraint = "compilation";
		my $target = engine_get_value_ifdef($tvalue, 'target');
		my $files = engine_get_value_ifdef($tvalue, 'files') || $tname;
		my $cflags = $sysconf->{compiler}{cflags} || "";
		if(defined $target) # if makefile
		{
			die("'files' field not found for $tname !") if(! defined $files);

			(my $makepath = $files) =~ s,/[^/]*$,,;
			(my $makefile = $files) =~ s/^$makepath\///;
			$command = "make $args -f $makefile -C $makepath $target";
		}
		else
		{
			my $comp_name = helper_detect_compiler($files);
			die("Unable to find a valid compiler for $tname !") if (!$comp_name);

			$command = "$sysconf->{compiler}{$comp_name} $cflags $args -o $bin $files";
		}
		engine_gen_test($xml, $tname, $command, $rc, $time, $delta, $constraint, @deps);
	}

	if($ttype =~ m/^(run|complete)$/)
	{
		my $launcher = $sysconf->{'runtime'}{'cmd'} || "";
		my $extra_args = $sysconf->{'runtime'}{'args'} || "";
		$constraint = undef;

		#do the job...
		my ($it_keys, $it_comb) = engine_TE_combinations($tname, $tvalue);

		# for each selected combinations
		foreach(0..(scalar @{$it_comb} -1 ))
		{
			# build the test name
			$name    = "$tname".engine_build_testname($it_keys, @{ $it_comb->[$_]} );
			#parse arguments and options depending on runtime
			my ($pre_env, $post_args) = engine_convert_to_cmd($it_keys, @{ $it_comb->[$_] });
			$command = "$pre_env $launcher $post_args $extra_args $bin $args";
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
		next if($test =~ m/^pcvst_.*$/);
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
