#   Parallel Computing -- Validation Suite (PCVS)
#
#   Copyright (C) 2017 
#   Commissariat à l'Énergie Atomique et aux Énergies Alternatives (CEA)
#   
#   This program is free software: you can redistribute it and/or modify
#   it under the terms of the GNU General Public License as published by
#   the Free Software Foundation, either version 3 of the License, or
#   (at your option) any later version.
#
#   This program is distributed in the hope that it will be useful,
#   but WITHOUT ANY WARRANTY; without even the implied warranty of
#   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
#   GNU General Public License for more details.
#
#   You should have received a copy of the GNU General Public License
#   along with this program.  If not, see <http://www.gnu.org/licenses/>.
#
#please dont edit this file...
package PCVS::Validate::Default;
use strict;
use warnings;
use Exporter;
use Data::Dumper;
use vars qw(@ISA @EXPORT);

@ISA = 'Exporter';
@EXPORT = qw();

my $gconf;
my $max_cores;
my $max_nodes;

sub runtime_init
{
	(my $self, $gconf) = @_;
	#to be faster...
	$max_cores = $gconf->{cluster}{max_cores_per_node};
	$max_nodes = $gconf->{cluster}{max_nodes};
}

sub runtime_fini
{
}

###########################################################################
# Retrieve an element of the combination with the associated key, if exists 
# Args:
#  - $arg: the key to find
#  - $k: the array of keys, currenly accepted by the configuration
#  - @c: the combination array
#
# Returns:
# the value associated to the key or undef
sub get_ifdef
{
	my ($arg, $k, @c)= @_;
	foreach (0..$#c)
	{
		return $c[$_] if($k->[$_] eq $arg);
	}

	return undef;
}

###########################################################################
# Function called for each generated combination to evaluated whether the combination if valid.
# In this function, it is important to check if the key exist it the configuration
# before using it. This is because we set an iterator to 'undef' when the configuration
# disabled it
# Args:
#  - $self: the object reference
#  - $keys: the array of keys handled by the configuration
#  - @args: the combination
#
# Returns:
# 1 if combination is valid, 0 otherwise
sub runtime_valid
{
	my ($self, $keys, @args) = @_;
	my ($node, $proc, $mpi, $omp, $core, $net, $sched) = ();

	$node  = get_ifdef('n_node',$keys,@args);
	$proc  = get_ifdef('n_proc',$keys,@args);
	$mpi   = get_ifdef('n_mpi' ,$keys,@args); 
	$omp   = get_ifdef('n_omp' ,$keys,@args);
	$core  = get_ifdef('n_core',$keys,@args);
	$net   = get_ifdef('net' ,$keys,@args); 
	$sched = get_ifdef('sched',$keys,@args);

	# please be sure to check if the iterator exist because the system emits
	# an undef when the iterator won't be unfolded for the current configuration
	
	return 1;
}

1;

