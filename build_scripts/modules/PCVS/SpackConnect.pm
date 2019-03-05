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


# die function, to print Spack-specific messages, will
# abort after being called.
# take a single string as argument
sub spack_die
{
	my ($str) = @_;
	print STDERR "$gconf->{colorcode}{rb}";
	foreach my $line (split /\n/, $str)
	{
		print STDERR " ERROR (Spack): $line\n";
	}
	print STDERR "$gconf->{colorcode}{d}";
	die "ERROR (Spack) : Abort due to error(s) above";
}

# same as above but do not abort after being called.
# Take a single string as argument
sub spack_warn
{
	my ($str) = @_;
	print STDERR "$gconf->{colorcode}{yb}";
	foreach my $line (split /\n/, $str)
	{
		print STDERR " WARNING (Spack): $line\n";
	}
	print STDERR "$gconf->{colorcode}{d}";
}

# module initialisation with global config (CLI, files...)
sub spack_init
{
	($gconf) = @_;
	$rundir = $CWD;
	#shortcuts
	$buildir = $gconf->{'build'};
	$srcdir = $gconf->{src};
	$internaldir = "$srcdir/build_scripts";
	
	# if the user overrides 'build_if_missing' for all packages from CL, cache it
	if(exists $gconf->{"spack-install"})
	{
		$glob_tobuild = $gconf->{"spack-install"};
	}

}

# extract compiler (in spack sense) pacakge name to provide the right Spack string (starting with '%')
# It will convert YAML spec into a string like '%mycompiler@version'
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

# if at least one Spack package is set, initialise all Spack relative
sub spack_prepare
{
	if(not $prepare_done)
	{
		# this is an ptimisation we are willing to have in PCVS, where
		# a system Spack installation could be reused to avoid installing duplicates
		# But because we had some issues, for now, only one instance of Spack can be
		# loaded at a time. If a previous environment is loaded, it will be used in place.
		# We just warn the user that its Spack environment can be altered with the addition/removal
		# of packages related to PCVS. To avoid that, the best solution for now is to run 
		# the script without pre-loading any Spack environmetn beforehand (see env -i or sh -l for example)
		spack_warn(
"DISCLAIMER : Two Spack installations should not exist at the same time when running PCVS (to be fixed).
In the meantime, be aware that if a Spack environment is present before running the test-suite, 
it will be modified by the installation/removal of packages during benchmark processing."
		);
		# IF there is muliple Spack installation, one can switch from the desired one to avoid conflict
		if(defined $gconf->{'spack-root'})
		{
			print " * INIT: Reusing Spack installation: $gconf->{'spack-root'}\n";
			spack_die("Unable to find Spack in designated folder '$gconf->{'spack-root'}'") if(! -f "$gconf->{'spack-root'}/bin/spack");
			# be aware that just setting PATH is not sufficient for a complete Spack integration
			# We need to source setup-ev.sh each time we want to load a package (it measns, per geneated test)
			$ENV{'PATH'} = "$gconf->{'spack-root'}/bin:".$ENV{'PATH'};
		}

		#check if we have the required commands in PATH
		my $str = qx(type spack spack-python 2>&1);
		if($str =~ /not found/)
		{
			# if not available, deploy our own version (won't conflict)
			if(! -d "$buildir/spack")
			{
				print " * INIT: Deploying local Spack into $buildir/spack/\n";
				system("tar xf $internaldir/resources/spack.tgz -C $buildir/");
			}
			$gconf->{'spack-root'} = "$buildir/spack";
			$ENV{'PATH'} = "$gconf->{'spack-root'}/bin:".$ENV{'PATH'};
		}
		else
		{
			$gconf->{'spack-root'} = qx(spack location -r);
			chomp $gconf->{'spack-root'};
		}
		#print "ROOT = $gconf->{'spack-root'}\n";
		
		#does it still have a sense to keep the TCL adherence while we are relying now fully on top of Spack ?
		#I think yes because PCVS is still using that approach to load the compiler. Spack is stilling missing 
		#Perl bindings anyway
		spack_die("PCVS only support Spack with Modules-tcl support") if($ENV{"MODULESHOME"} eq "");
		spack_die("Are you sure \$MODULESHOME is pointing to Modules setup directory ?") if(! -d "$ENV{MODULESHOME}/init" );
		my $perl_script = qx(ls $ENV{MODULESHOME}/init/*perl*); chomp $perl_script;
		require "$perl_script";
	}
	$prepare_done = 1;
}

#from a Spack package name, return the module (TCL) name and its location on fhe FS
#The called script is writing on stdout two lines : 
#	1 - The module name
#	2 - the location
sub spack_find_module_from_package
{
	my ($spackage) = @_;
	#{print "spack-python $internaldir/generation/concretizer.py $spackage\n";
	my $out = qx(spack-python $internaldir/generation/concretizer.py $spackage);
	chomp $out;
	my @tmp = split /\n/, $out;
	#print "spack loads = $name\n";
	return @tmp;
}

# from YAML specs, build the string that Spack will use to identify the 'UNIQUE' package we want to load/install
# the YAML node is the argument
sub spack_build_package_name
{
	my ($spackrun) = @_;
	my $spackage = undef;

	#first, keep the name, this is mandatory
	$spackage = $spackrun->{name} || return undef;

	#if specified, add the version and any variants
	$spackage .= "\@$spackrun->{version}" if(defined $spackrun->{version});
	$spackage .= " ".join("", @{$spackrun->{variants}}) if(defined $spackrun->{variants});

	#if specified add the compiler 'specified with '%'
	$spackage .= spack_build_with_generator(%{$spackrun});

	#for each defined dep, some cherks are performed
	#then, their name are expanded in the same way as above and added to the current package with a '^'
	foreach my $depname(keys %{$spackrun->{deps}})
	{
		my %depnode = %{$spackrun->{deps}{$depname}};

		# if the YAML spec requires to add the compiler as a dependency AND we actually found a 
		# spack configuration for the comiler, add it
		# Consider packages specifing rules over the mpi package without explicitly mentioning an implementation.
		# This let TEs to add filters to loaded compiler in a dynamic way
		if($depnode{upstream} eq "compiler" and defined $gconf->{compiler}{spackage}{gen_spackname})
		{
			#There is a tricky case for compilers. If the compiler is a bare one, we have to use the '%' as
			#Spack is probablky knowing it as a compiler and not a regular dependecy.
			#But then, consider MPI compilers, which are atually virtual packages for Spack, actually compiling code
			#but Spack is expecting them as regular dependencies.
			#To deal with it, a good Spack comiler configuration should set a 'bare_compiler' field if it needs to be loaded
			#as a regulard compiler (prefix with '%'). By default, it is set to false as most of our use cases are MPI-based
			#compilers
			$spackage .= ($gconf->{compiler}{spackage}{bare_compiler} ? "%" : "^"). $gconf->{compiler}{spackage}{gen_spackname};
		}
		#same as above, but for runtime Spack configuration
		elsif($depnode{upstream} eq "runtime" and defined $gconf->{runtime}{spackage}{gen_spackname})
		{
			$spackage .= "^$gconf->{runtime}{spackage}{gen_spackname}";
		}
		else
		{
			$spackage .= "^".$depname;
			$spackage .= "\@$depnode{version}" if(defined $depnode{version});
			$spackage .= spack_build_with_generator(%depnode);
			$spackage .= join("", @{$depnode{variants}}) if defined($depnode{variants});
		}
	}
	return $spackage;
}

#Try do tectect if a package is already installed, can be installed or not
#This function will return :
#  * zero if the package is installed
#  - 1 if the package is not installed or not existing (will be discovered at runtime unfortunately)
#  - 2 there is multiple matches for this spec
sub spack_detect_package
{
	my ($override, $yaml_node) = @_;
	my @commands;
	my $spackage = undef;

	$spackage = $override if (defined $override);
	$spackage = spack_build_package_name($yaml_node) if(exists $yaml_node->{name} and not defined $spackage);
	#print "spack name = $spackage\n";

	if(defined $spackage) # a Spack spec should be loaded
	{
		spack_prepare();
		#initialize default values for this node
		$yaml_node->{gen_spackname} = $spackage;
		$yaml_node->{gen_modname} = undef;
		$yaml_node->{gen_modpath} = undef;

		# priority : from the most to the least specialized definition : 
		#  - explicitly defined in YAML
		#  - then override from the command line
		#  - 0 by default
		$yaml_node->{build_if_missing} = $glob_tobuild if(defined $glob_tobuild and not exists $yaml_node->{build_if_missing});
		$yaml_node->{build_if_missing} = 0 if(not exists $yaml_node->{build_if_missing});
		
		# by trying to find a location, we can assess if the package is already installed
		# This is a limitation, probably the reason why we cannot handle multiple Spack installations
		# for now, but currently the best way to have it on best effort
		my $pattern = qx(spack location --install-dir $spackage 2>&1);
		chomp $pattern;
		#print "spack location = $pattern\n";
		
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
			($yaml_node->{gen_modname}, $yaml_node->{gen_modpath}) = spack_find_module_from_package($spackage);
			return 0;

		}
	}
	return -1;
}

#effectively load a Spack definition if any.
#This is mainly used by compiler/runtime configuration to be loaded when pre-processing TEs
sub spack_load_package
{
	my ($need_install, %node) = @_;
	my ($module_name, $module_path) ;

	#if package is not found but is allowed to be built, build it
	if($need_install)
	{
		spack_die("Package '$node{spackage}{gen_spackname}' cannot be found nor installed (see --spack-install)") if(! $node{spackage}{build_if_missing});
		system("spack install $node{spackage}{gen_spackname}");
		spack_die("An error occured when installed the package. See above") if ($? ne 0);
		($module_name, $module_path) = spack_find_module_from_package($node{spackage}{gen_spackname});
		$node{spackage}{gen_modname} = $module_name;
		$node{spackage}{gen_modpath} = $module_path;
	}
	else
	{
		$module_name = "$node{spackage}{gen_modname}";
	}

	# use the Perl bindings for module, working pretty well for now...
	module("load $module_name");
	return $module_name;
}

# load the environment, compiler/runtime mainly
sub spack_env_load
{
	my ($user_conf, %node) = @_;
	my ($ret, $tobuild, $spackage, $module_name) = (undef, undef, undef, undef);

	#The function below is gonna fill this node up with Spack configuration.
	#The node is given by reference to let the calling function set the actual values.
	#There is two cases :
	# - A Spack definition is present in the YAML file, the node will exist and everything will be fine as the existing
	# 	 key will be passed by reference
	# - There is no Spack definition BUT the user set an override through the CL. in that case, no pre-existing node
	# has been created and 'undef' is forwarded. For this particular case, we create an empty node beforhand.
	#
	# The distinction between these two cases is kept by the miss of the 'name' key in the Spack node.
	# This should not impact TE spack configuration because CL override is not possible.
	#
	$node{spackage} = {} if (! exists $node{spackage});
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
	print "    - Module loaded: $gconf->{colorcode}{g}$module_name$gconf->{colorcode}{d}\n";
	printf Dumper(\%node);
}

#create the Spack specs for a test, if any
sub spack_test_load
{
	my ($user_conf, $node) = @_;
	
	my ($spack_set,$ret,$rc) = (0, 0, 0);
	my @msgs;

	if(defined $node) # if there is a Spack definition
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
