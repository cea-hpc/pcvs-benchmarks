package PCVS::TEEngine;
use strict;
use warnings;
use Exporter;
use Data::Dumper;
use XML::Writer;
use YAML qw(LoadFile DumpFile); 
use vars qw(@ISA @EXPORT @EXPORT_OK);

@ISA = 'Exporter';
@EXPORT = qw(engine_init engine_fini engine_unfold_file);
@EXPORT_OK = qw();

my $myconf;

sub engine_init
{
	($myconf) = @_;
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
