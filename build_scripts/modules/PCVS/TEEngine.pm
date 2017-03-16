package PCVS::TEEngine;
use strict;
use warnings;
use Exporter;
use Data::Dumper;
use XML::Writer;
use YAML qw(LoadFile DumpFile); 
$YAML::numify = 1;
use vars qw(@ISA @EXPORT @EXPORT_OK);

@ISA = 'Exporter';
@EXPORT = qw(engine_init engine_fini engine_unfold_file);
@EXPORT_OK = qw();

my $sysconf;
my %testconf;

sub get_if_exists
{
	my ($p) = @_;

	if(exists $sysconf->{'tests'}{$p})
	{
		return $sysconf->{'tests'}{$p};
	}
	else
	{
		return undef;
	}
}

sub engine_init
{
	($sysconf) = @_;

	my @system_fields = ("n_node", "n_proc", "n_mpi", "n_omp", "n_core", "net", "sched");

	foreach my $field(@system_fields)
	{
		$testconf{$field} = get_if_exists($field);
	
		if(defined $testconf{$field})
		{
			$testconf{$field} = undef if(defined $testconf{$field} and scalar $testconf{$field} eq 0);
			my @list_values;
			foreach my $el(@{ $testconf{$field} } )
			{
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
				elsif($el =~ /^([0-9]+)(-([0-9]+))*$/)
				{
					my ($min, $max);
					$min = $1; $max = (defined $2) ? $3 : $min;
					while($min <= $max)
					{
						push @list_values, $min;
					} continue {$min++;}
				}
				elsif($field =~ /^n_/) #iterators prefixed with 'n_' should fall into previous conditinos
				{
					die("$field only takes numeric, 'a:b:c' or 'a-b' interval values !!");
				}
				else
				{
					push @list_values, $el;
				}
			}
			$testconf{$field} = \@list_values;
		}
	}
	print Dumper(\%testconf);
}

sub engine_unfold_test
{
	my  ($xml, $ht,  $fstream, $bpath) = @_;
	$xml->startTag("job");
	$xml->dataElement("name", "$ht");

	my $command;
	if(exists $fstream->{$ht}{'type'} and lc($fstream->{$ht}{'type'}) eq "build")
	{
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
		$xml->startTag("constraints");
		$xml->dataElement("constraint", "compilation");
		$xml->endTag("constraints");
	}
	else
	{
		$command="mpirun -np 2 $bpath/$fstream->{$ht}{'herit'}{'bin'} $fstream->{$ht}{'args'}";
	}

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

sub engine_unfold_file
{

	my ($bpath, $ftree, $filepath) = @_;
	my $filestream = LoadFile("$filepath");

	#ready to write list_of_tests
	open(my $xml_file, ">", "$bpath/list_of_tests.xml");
	my $xmlwriter = XML::Writer->new(OUTPUT => $xml_file, NEWLINES => 1);
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
