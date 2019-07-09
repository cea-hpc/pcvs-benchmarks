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
package PCVS::SpackConnect;
use strict;
use warnings;
use Exporter;
use Env::Modulecmd;
use File::chdir;
use Data::Dumper;

our @ISA = 'Exporter';
our @EXPORT = qw(spack_init spack_load);
our @EXPORT_OK = qw();

our $gconf;
our ($buildir, $internaldir, $srcdir, $rundir);

my $prepare_done = 0;

sub spack_die
{
	my ($str) = @_;
	print STDERR "$gconf->{colorcode}{rb}";
	foreach my $line (split /\n/, $str)
	{
		print STDERR "ERROR (Spack): $line\n";
	}
	print STDERR "$gconf->{colorcode}{d}";
	die "ERROR (Spack) : Abort due to error(s) above";
}

sub spack_init
{
	($gconf) = @_;
	$rundir = $CWD;
	#shortcuts
	$buildir = $gconf->{'build'};
	$srcdir = $gconf->{src};
	$internaldir = "$srcdir/build_scripts";
}

sub spack_build_with_generator
{
	my (%node) = @_;
	my $str = "";
	if(exists $node{build_with} and defined $node{build_with}{compiler})
	{
		$str .= " %$node{build_with}{compiler}";
		$str .= "\@$node{build_with}{version}" if (defined $node{build_with}{version});
	}
	return $str;
}

sub spack_prepare
{
	if(not $prepare_done)
	{
		my $str = `which spack 2>&1`;
		if($? ne 0)
		{
			spack_die("Spack is used somewhere in the configuration,\nbut unable to find the 'spack' command in the environment.");
		}
		
		spack_die("PCVS only support Spack with Modules-tcl support") if($ENV{"MODULESHOME"} eq "");
		spack_die("Are you sure \$MODULESHOME is pointing to Modules setup directory ?") if(! -d "$ENV{MODULESHOME}/init" );
		my $perl_script = `ls $ENV{MODULESHOME}/init/*perl*`; chomp $perl_script;
		require "$perl_script";

	}
	$prepare_done = 1;
}

sub spack_load
{
	my (%node) = @_;
	if(exists $node{spackage})
	{
		spack_prepare();
		print "   - Found ! Using it\n";
		my %spackrun = %{$node{spackage}};
		my $spackage = $spackrun{name} || spack_die("The field 'name' should be provided for runtime definition");

		$spackage .= "\@$spackrun{version}" if(defined $spackrun{version});
		$spackage .= " ".join("", @{$spackrun{variants}}) if(defined $spackrun{variants});
		$spackage .= spack_build_with_generator(%spackrun);

		foreach my $depname(keys %{$spackrun{deps}})
		{
			my %depnode = %{$spackrun{deps}{$depname}};
			$spackage .= " ^".$depname;
			$spackage .= "\@$depnode{version}" if(defined $depnode{version});
			$spackage .= spack_build_with_generator(%depnode);
			$spackage .= " ".join("", @{$depnode{variants}}) if defined($depnode{variants});

		}
		
		my $spack_out = `spack find $spackage 2>&1`;
		chomp $spack_out;

		if($spack_out !~ /([0-9]+) installed package/)
		{
			if($spackrun{build_if_missing})
			{
				system("echo spack install $spackage");
			}
			else
			{
				spack_die("Failed to load $spackrun{name} according to spec and not allowed to build it by ourselves\n$spack_out");
			}
		}
		elsif($1 gt 1)
		{
			spack_die("We did not handle multiple matches yet :( Please see below :\n$spack_out");
		}

		my $module_name = `spack module tcl loads --input-only $spackage`;
		chomp $module_name;
		#TODO: hack...
		#Env::Modulecmd::load($module_name);
		module("load $module_name");
	} 
	else
	{
		print "   - Spack not requested, hope for the best through the environment.\n";
	}
}

1;
