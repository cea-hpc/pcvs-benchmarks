package PCVS::TEngine;
use strict;
use warnings;
use 5.010;
use Exporter;
use Module::Load "autoload";
use Data::Dumper;
use XML::Writer;
use YAML qw(LoadFile DumpFile); 
use Algorithm::Loops qw(NestedLoops); # time saver
$YAML::numify = 1;
use vars qw(@ISA @EXPORT @EXPORT_OK);

@ISA = 'Exporter';
@EXPORT = qw(engine_init engine_fini engine_unfold_file);
@EXPORT_OK = qw();

my $sysconf;
my @combinations;

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
	print Dumper($sysconf);
	die("Unable to find a valid Module !") if(!exists $sysconf->{'runtime'}{'validate_module'});
	my $loaded_module = "PCVS::Validate::$sysconf->{'runtime'}{'validate_module'}";
	load($loaded_module);

	my @system_fields = ("n_node", "n_proc", "n_mpi", "n_omp", "n_core", "net", "sched");
	my @system_objs;
	my @comb_arrays;

	#first, remove iterators not used by the configuration
	foreach my $field(@system_fields)
	{
		my @current_obj = get_if_exists($field);
		next if(scalar @current_obj eq 0 or ! exists $sysconf->{"runtime"}{$field}{'prefix'} or !$sysconf->{"runtime"}{$field}{'prefix'});
		
		push @system_objs, $field;
	}

	#once we cleaned up, let the runtime remember the iterator sequence
	$loaded_module->runtime_init($sysconf, @system_objs);

	#... and iterate over the iterators to build possible sequences for each
	foreach my $field(@system_objs)
	{
		my @current_obj = get_if_exists($field);
		# final list of values
		my @list_values;
		foreach my $el(@current_obj )
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
					else {die("$field: Unknown operator '$_'");}
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
			# else if a numeric field and not fallen in previous conditions --> abort
			elsif($field =~ /^n_/)
			{
				die("$field only takes numeric, 'a:b:c' or 'a-b' interval values !!");
			}
			else
			{
				push @list_values, $el;
			}
		}
		#remove duplicate and sort the array (no sensible overhead, small arrays here...)
		# may be optimized to avoid distinctions between numerical and non-numerical arrays
		if($field =~ /^n_/)
		{
			@list_values = uniq(sort { $a <=> $b } @list_values );
		}
		else
		{
			@list_values = uniq(sort @list_values);
		}

		push @comb_arrays, \@list_values;
	}
	my $n = 0;
	NestedLoops(\@comb_arrays, sub { $n++; push @combinations, [ @_ ] if($loaded_module->runtime_valid(@_));} );
	print " * Creating a set of ".scalar @combinations." combinations per test (over $n)\n";
}

sub engine_unfold_test
{
	my  ($xml, $ht,  $fstream, $bpath) = @_;

	my $command;
	if(exists $fstream->{$ht}{'type'} and lc($fstream->{$ht}{'type'}) eq "build")
	{
		$xml->startTag("job");
		$xml->dataElement("name", "$ht");
		if(exists $fstream->{$ht}{'target'}) # makefile
		{
			(my $makepath = $fstream->{$ht}{'files'}) =~ s,/[^/]*$,,;
			(my $makefile = $fstream->{$ht}{'files'}) =~ s/^$makepath\///;
			$command = "make -f $makefile -C $makepath $fstream->{$ht}{'target'}";
		}
		else
		{
			$command = "echo Hello, World && return 1";
		}
		$xml->dataElement("command", $command);
		$xml->startTag("constraints");
		$xml->dataElement("constraint", "compilation");
		$xml->endTag("constraints");
		$xml->endTag("job");
	}
	else
	{
		foreach my $cel(@combinations)
		{
			$xml->startTag("job");
			$xml->dataElement("name", "$ht".join("_", @{ $cel }));
			$command="LAUNCHER ".(Dumper($cel))."  $bpath/$fstream->{$ht}{'herit'}{'bin'} $fstream->{$ht}{'args'}";


			if(exists $fstream->{$ht}{'herit'}{'deps'})
			{
				$xml->startTag("deps");
				$xml->dataElement("dep", $fstream->{$ht}{'herit'}{'deps'}[0]);
				$xml->endTag("deps");
			}


			$xml->dataElement("command", $command);
			#$xml->dataElement("rc", "$fstream->{$ht}{'returns'}");
			$xml->endTag("job");
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
		engine_unfold_test($xmlwriter, $test, $filestream, $bpath);
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
