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
our @EXPORT = qw(spack_init spack_env_load spack_test_load);
our @EXPORT_OK = qw();

our $gconf;
our ($buildir, $internaldir, $srcdir, $rundir);

my $prepare_done = 0;
my $glob_tobuild = undef;

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
	
	if(exists $gconf->{"spack-install"})
	{
		$glob_tobuild = $gconf->{"spack-install"};
	}

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
		my $str = qx(which spack 2>&1);
		if($? ne 0)
		{
			spack_die("Spack is used somewhere in the configuration,\nbut unable to find the 'spack' command in the environment.");
		}
		
		spack_die("PCVS only support Spack with Modules-tcl support") if($ENV{"MODULESHOME"} eq "");
		spack_die("Are you sure \$MODULESHOME is pointing to Modules setup directory ?") if(! -d "$ENV{MODULESHOME}/init" );
		my $perl_script = qx(ls $ENV{MODULESHOME}/init/*perl*); chomp $perl_script;
		require "$perl_script";

	}
	$prepare_done = 1;
}

sub spack_find_module_from_package
{
	my ($spackage) = @_;
	my $name = qx(spack module tcl loads --input-only $spackage);
	chomp $name;
	return $name;
}


sub spack_build_package_name
{
	my ($spackrun) = @_;
	my $spackage = undef; 
	$spackage = $spackrun->{name} || return undef;

	$spackage .= "\@$spackrun->{version}" if(defined $spackrun->{version});
	$spackage .= " ".join("", @{$spackrun->{variants}}) if(defined $spackrun->{variants});
	$spackage .= spack_build_with_generator(%{$spackrun});

	foreach my $depname(keys %{$spackrun->{deps}})
	{
		my %depnode = %{$spackrun->{deps}{$depname}};
		$spackage .= " ^".$depname;
		$spackage .= "\@$depnode{version}" if(defined $depnode{version});
		$spackage .= spack_build_with_generator(%depnode);
		$spackage .= " ".join("", @{$depnode{variants}}) if defined($depnode{variants});
	}
	return $spackage;
}

sub spack_detect_package
{
	my ($override, $yaml_node) = @_;
	my @commands;
	my $spackage = undef;

	$spackage = $override if (defined $override);
	$spackage = spack_build_package_name($yaml_node) if(exists $yaml_node->{name} and not defined $spackage);

	if(defined $spackage) # a Spack spec should be loaded
	{
		spack_prepare();
		$yaml_node->{gen_spackname} = $spackage;
		$yaml_node->{gen_modname} = undef;
		$yaml_node->{build_if_missing} = 0;
		
		$yaml_node->{build_if_missing} = $yaml_node->{build_if_missing} if(exists $yaml_node->{build_if_missing});
		$yaml_node->{build_if_missing} = $glob_tobuild if(defined $glob_tobuild);
		
		my $pattern = qx(spack location --install-dir $spackage 2>&1);
		chomp $pattern;
		
		# if something seems to be found
		if($pattern =~ /no installed package/)
		{
			# can be either :
			#  - valid package name but not installed or,
			#  - invalid package name
			# Up to the caller to decide what to do.
			return 1;
		}
		elsif($pattern =~ /Use a more specific spec/)
		{
			#ambiguous name, cannot identify one unique entry.
			return 2;
		}
		else
		{
			# Everything goes well, package is already installed -> retrieve the module name
			$yaml_node->{gen_modname} = spack_find_module_from_package($spackage);
			return 0;

		}
	}
	return -1;
}

sub spack_load_package
{
	my ($need_install, %node) = @_;
	my $module_name = undef ;

	if($need_install)
	{
		spack_die("Package '$node{spackage}{gen_spackname}' cannot be found nor installed (see --spack-install)") if(! $node{spackage}{build_if_missing});
		system("spack install $node{spackage}{gen_spackname}");
		spack_die("An error occured when installed the package. See above") if ($? ne 0);
		$module_name = spack_find_module_from_package($node{spackage}{gen_spackname});
		$node{spackage}{gen_modname} = $module_name;
	}
	else
	{
		$module_name = "$node{spackage}{gen_modname}";
	}

	module("load $module_name");
	return $module_name;
}

sub spack_env_load
{
	my ($user_conf, %node) = @_;
	my ($ret, $tobuild, $spackage, $module_name) = (undef, undef, undef, undef);


	$ret = spack_detect_package($user_conf, $node{spackage});

	if($ret eq -1) # no spack-definition
	{
		$module_name = "$gconf->{colorcode}{r}None$gconf->{colorcode}{d}";
	}
	elsif($ret eq 2) # ambiguous
	{
		spack_die("Unable to identify one *UNIQUE* package to load :\n".qx(spack location --install-dir $node{spackage}{gen_spackname} 2>&1));
	}
	else
	{
		$module_name = spack_load_package($ret, %node);
	}
	printf "    - Module loaded: $gconf->{colorcode}{g}$module_name$gconf->{colorcode}{d}\n";
}

sub spack_test_load
{
	my ($user_conf, $node) = @_;
	
	my ($spack_set,$ret,$rc) = (0, 0, 0);
	my @msgs;

	if(defined $node)
	{
		$ret = spack_detect_package(undef, $node);
		if($ret eq -1) # No spack definition
		{
			$rc+=1;
			push @msgs, "No Spack identification for this node...";
		}
		elsif($ret eq 2) # ambiguous
		{
			push @msgs, "Ambiguous Spack definition '$node->{gen_spackname}'";
			$rc+=1;
		}
		else
		{
			push @msgs, "Spack will be loaded : $node->{gen_spackname}";
			$spack_set=1;

		}
	}

	return ($rc, $spack_set, @msgs);
}

1;
