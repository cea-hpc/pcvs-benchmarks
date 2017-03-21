package PCVS::TEngine;

use strict;
use warnings;

use Exporter;
use Algorithm::Loops qw(NestedLoops); # time saver
use Module::Load qw(load autoload); #dynamic module loading
use Data::Dumper; #used for debug
use XML::Writer; #XML parser
use YAML qw(LoadFile); # YAML parser
$YAML::numify = 1;
use vars qw(@ISA @EXPORT @EXPORT_OK);

@ISA = 'Exporter';
@EXPORT = qw(engine_init engine_fini engine_unfold_file);
@EXPORT_OK = qw();

my $sysconf;
my @iter_namelist = ("n_node", "n_proc", "n_mpi", "n_omp", "n_core", "net", "sched");
my %iter_name_map = ("n_node" => "N", "n_proc" => "P", "n_mpi" => "np", "n_omp" => "omp", "n_core" => "c", "net" => "", "sched" => ""); # just used to build test name
my @iter_prefix;
my @iter_combinations;

# utility to remove duplicate from an array
sub uniq {
	my %seen;
	return grep { !$seen{$_}++ } @_;
}

#retrieve from global configuration, the given field
sub get_if_exists
{
	my ($p) = @_;

	if(exists $sysconf->{'tests'}{$p})
	{
		return @{ $sysconf->{'tests'}{$p} };
	}
	else
	{
		return;
	}
}

sub engine_init
{
	($sysconf) = @_;

	#very important : load runtime module validator.
	die("Unable to find a valid Module !") if(!exists $sysconf->{'runtime'}{'validate_module'});
	my $loaded_module = "PCVS::Validate::$sysconf->{'runtime'}{'validate_module'}";
	load($loaded_module);

	#first, remove iterators not used by the configuration
	my @tmp;
	foreach my $iter(@iter_namelist)
	{
		my @current_obj = get_if_exists($iter);

		#if iterator does not exist or not defined by the runtime, the iterator is skipped
		next if(scalar @current_obj eq 0 or ! exists $sysconf->{"runtime"}{$iter}{'prefix'} or !$sysconf->{"runtime"}{$iter}{'prefix'});

		push @tmp, $iter;
	}
	@iter_namelist = @tmp; 

	#compute prefix along with each iterator
	@iter_prefix = map { $sysconf->{runtime}{$_}{prefix} } @iter_namelist;

	#create an hashmap to associate the iterator with the 
	#once we cleaned up, let the runtime remember the iterator sequence
	$loaded_module->runtime_init($sysconf, @iter_namelist);


	#list of iterator sequences
	my @itseq_list;

	#... and iterate over the iterators to build possible sequences for each
	foreach my $iter_name(@iter_namelist)
	{
		my @iter_list = get_if_exists($iter_name);
		# final list of values
		my @list_values;
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

		push @itseq_list, \@list_values;
	}
	my $n = 0;
	NestedLoops(\@itseq_list, sub { $n++; push @iter_combinations, [ @_ ] if($loaded_module->runtime_valid(@_));} );
	print " * Creating a set of ".scalar @iter_combinations." combinations per test (over $n defined)\n";

	$loaded_module->runtime_fini();
}

sub engine_build_testname
{
	my @c = @_;
	my $name;
	foreach (0..$#c)
	{
		#$name .= "_".$iter_namelist[$_].$c[$_];
		$name .= "_".$iter_name_map{$iter_namelist[$_]}.$c[$_];
	}
	return $name;
}

sub engine_convert_to_cmd
{
	my @c = @_;
	my ($pre_env, $post_args) = ();
	foreach (0..$#c)
	{
		my $param_name = $iter_namelist[$_];
		my $prefix_name = $iter_prefix[$_];
		my $trad_name = $sysconf->{'runtime'}{$param_name}{"$c[$_]_opt"};
		my $value = $prefix_name;
		
		$value .= (defined $trad_name) ? $trad_name : $c[$_];
		$value .= " ";

		if(lc($sysconf->{'runtime'}{$param_name}{'usage'}) eq "env")
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
	my ($xml, $name, $command, $time, $delta, $constraint, @deps) = @_;
	$xml->startTag("job");
	$xml->dataElement("name", "$name");
	$xml->dataElement("command", $command);
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

sub engine_unfold_test_expr
{
	#params
	my  ($xml, $tname,  $tvalue, $bpath) = @_;
	#global var for a test_expr
	my ($name, $command, $time, $delta, $constraint, @deps) = ();
	#other vars
	my $ttype = lc($tvalue->{'type'} || "run");
	
	#common params, whatever the TE type
	$time    = $tvalue->{'limit'} || $tvalue->{'herit'}{'limit'} || undef;
	$delta   = $tvalue->{'tolerance'} || $tvalue->{'herit'}{'tolerance'} or undef;
	@deps    = @{$tvalue->{'deps'} || $tvalue->{'herit'}{'deps'} || [] };
	
	if($ttype =~ m/^(build|complete)$/)
	{
		$constraint = "compilation";
		if(exists $tvalue->{'target'}) # if makefile
		{
			(my $makepath = $tvalue->{'files'}) =~ s,/[^/]*$,,;
			(my $makefile = $tvalue->{'files'}) =~ s/^$makepath\///;
			$command = "make -f $makefile -C $makepath $tvalue->{'target'}";
		}
		else
		{
			$command = "echo 'Not implemented yet !' && return 1";
		}
		engine_gen_test($xml, $tname, $command, $time, $delta, $constraint, @deps);
	}
	
	if($ttype =~ m/^(run|complete)$/)
	{
		my $launcher = $sysconf->{'runtime'}{'mpi_cmd'} || "";
		my $extra_args = $sysconf->{'runtime'}{'extra_args'} || "";
		my $bin = "$bpath/".($tvalue->{'bin'} || $tvalue->{'herit'}{'bin'} || $tname);
		my $args = $tvalue->{'args'} || $tvalue->{'herit'}{'args'} || "";
		foreach(@iter_combinations)
		{
			$name    = "$tname".engine_build_testname(@{$_});
			my ($pre_env, $post_args) = engine_convert_to_cmd(@{$_});
			$command = "$pre_env $launcher $post_args $extra_args $bin $args";
			$constraint = undef;
			engine_gen_test($xml, $name, $command, $time, $delta, $constraint, @deps);
		}
	}
}

sub engine_unfold_file
{

	my ($bpath, $ftree, $filepath) = @_;
	my $filestream = LoadFile("$filepath");

	#ready to write list_of_tests
	open(my $xml_file, ">", "$bpath/list_of_tests.xml");
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
}


sub engine_fini
{
}

1;
