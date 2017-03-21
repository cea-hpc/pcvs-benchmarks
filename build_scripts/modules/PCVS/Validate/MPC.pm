#please dont edit this file...
package PCVS::Validate::MPC;
use strict;
use warnings;
use Exporter;
use Data::Dumper;
use vars qw(@ISA @EXPORT);

@ISA = 'Exporter';
@EXPORT = qw();

my $gconf;
my @iter_list;
my $max_cores;
my $max_nodes;
sub runtime_init
{
	(my $self, $gconf, @iter_list) = @_;
	$max_cores = $gconf->{cluster}{max_cores_per_node};
	$max_nodes = $gconf->{cluster}{max_nodes};
}

sub runtime_fini
{
}

sub get_ifdef
{
	my ($arg, @c)= @_;
	foreach (0..$#c)
	{
		return $c[$_] if($iter_list[$_] eq $arg);
	}

	return undef;
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
	return 0 if(defined $node and defined $max_nodes and $node > $max_nodes); 
	return 0 if(defined $node and defined $proc      and $node > $proc); 
	return 0 if(defined $proc and defined $mpi       and $proc > $mpi); 
	return 0 if(defined $proc and defined $mpi       and $proc > $mpi); 
	return 0 if(defined $proc and defined $core and defined $node and $core > ($max_cores * $node) / $proc and $core ne 1);
	return 0 if(defined $node and defined $sched and $node > 1 and $net eq "shmem");
	
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

