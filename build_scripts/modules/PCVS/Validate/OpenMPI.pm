#please dont edit this file...
package PCVS::Validate::OpenMPI;
use strict;
use warnings;
use Exporter;
use Data::Dumper;
use vars qw(@ISA @EXPORT);

@ISA = 'Exporter';
@EXPORT = qw(runtime_valid);

my $gconf;
my %syshash;
my $max_cores;
my $max_nodes;
sub runtime_init
{
	(my $self, $gconf, my @sysfields) = @_;
	%syshash = map { $sysfields[$_] => $_ } 0..$#sysfields;
}

sub get_ifdef
{
	my ($arg, @arr)= @_;
	if(exists $syshash{$arg})
	{
		return $arr[ $syshash{$arg} ];
	}
	else
	{
		return undef;
	}
}

sub runtime_valid
{
	my ($self, @args) = @_;
	my ($node, $proc, $mpi, $omp, $core, $net, $sched) = ();

	$node  = get_ifdef('n_node',@args);
	$proc  = get_ifdef('n_proc',@args);
	$mpi   = get_ifdef('n_mpi' ,@args); 
	$omp   = get_ifdef('n_omp' ,@args);
	$core  = get_ifdef('n_core',@args);
	$net   = get_ifdef('net' ,@args); 
	$sched = get_ifdef('sched',@args);

	# please be sure to check if the iterator exist because the system emits
	# an undef when the iterator won't be unfolded for the current configuration
	
	
	#print "Keep N=".(defined $node ? $node : "X").
		  #" p=".(defined $proc ? $proc : "X").
		  #" t=".(defined $mpi ? $mpi : "X").
		  #" o=".(defined $omp ? $omp : "X").
		  #" c=".(defined $core ? $core : "X"). 
		  #" n=".(defined $net ? $net : "X").
		  #" m=".(defined $sched ? $sched : "X")."\n";	
	return 1;
}
1;

