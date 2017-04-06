package PCVS::Configuration;
use strict;
use warnings;
use Exporter;
use Sys::Hostname;
use File::chdir;
use PCVS::Helper;
use YAML qw(LoadFile DumpFile);
use Data::Dumper;

our @ISA = 'Exporter';
our @EXPORT = qw(configuration_init configuration_build configuration_display_vars configuration_save);
our @EXPORT_OK = qw();

our %gconf;
our ($buildir, $internaldir, $srcdir, $rundir);

###########################################################################
# Load a YAML and return the content as an HASH
# Args:
#  - $yml_path: the path to YAML file
#
# Returns:
# the HASH mapping the file
sub load_yml
{
	my ($yml_path) = @_;

	die("Error with $yml_path: $!") if(! -f $yml_path);
	my $s = LoadFile($yml_path) ;
	return %{ $s };
}

###########################################################################
# Initialize the current configuration loader
# Args:
#  - @_: Loaded configuration from command-line
sub configuration_init
{
	(%gconf) = @_;
	$rundir = $CWD;
	#shortcuts
	$buildir = $gconf{'build'};
	$srcdir = $gconf{src};
	$internaldir = "$srcdir/build_scripts";
}

###########################################################################
# Construct the global configuration by mergind default, user and command-line configurations
# Args: No Args
#
# Returns:
# The update HASH
sub configuration_build
{
	# save command-line overrides
	my %cmdline_override = %gconf;
	
	# load the default file (default values)
	my %default_data = load_yml ("$internaldir/environment/default.yml");

	#update the current configuration hash with default value (no overlap w/ options)
	%gconf = %default_data;

	#find and load the user configuration file
	my $user_config = configuration_load($cmdline_override{"config-target"});
	
	#if the user config file exists
	if(defined $user_config)
	{
		#delete $gconf{"config-target"};
		my %user_data = load_yml($user_config);

		#update default config with overriden values
		foreach my $key(keys %user_data)
		{
			#exists $gconf{$key} or die("\'$key\' object does not exist. Please edit your configuration file !");
			#iterate over subkeys to update (YAML data is a two-level tree, should be replaced by recursion).
			if(ref($gconf{$key}) eq 'HASH')
			{
				foreach my $subkey (keys $user_data{$key})
				{
					#exists $gconf{$key}{$subkey} or die("\'$key/$subkey\' object does not exist. Please edit your configuration file !");
					$gconf{$key}{$subkey} = $user_data{$key}{$subkey};
				}
			}
			else
			{
				$gconf{$key} = $user_data{$key};
			}
		}
	}

	#print Dumper(\%gconf);
	#override w/ command-line
	foreach(keys %cmdline_override)
	{
		$gconf{$_} = $cmdline_override{$_};
	}

	# parse compiler-target and runtime-target
	foreach my $el(('compiler', 'runtime'))
	{
		#the '--target' option always have priority (should be improved)
		$gconf{"${el}-target"} = $gconf{'target'} if(exists $gconf{target});
		
		my $pattern=$gconf{"$el-target"};
		if(defined $pattern)
		{
			#remove 'compiler|runtime-target"
			delete $gconf{"$el-target"};

			#load the file
			my $filepath = "$internaldir/configuration/${el}s/$pattern.yml";
			die("Unable to find $pattern as $el target ($el-target)") if(! -f $filepath);
			my %data = load_yml($filepath);

			#dump the content under $el hash object
			$gconf{$el} = \%data;
			#save the target name into the newly created object
			$gconf{$el}{'target'} = $pattern;
		}
		else
		{
			die("You must specify a '$el' target ($el-target)");
		}
	}

	# check if the HASH is valid (not complete)
	configuration_validate();
	return %gconf;
}

###########################################################################
# Iterate over configuration HASH object to create env vars
# Args:
#  - $output_file: where content will be written
#  - $hashref : current node value
#  - $key : current node key
sub configuration_passthrough
{
	my ($output_file, $hashref, $key) = @_;

	#for each subkey of the current node
	foreach my $k (keys %{$hashref})
	{
		#translate '/', '-' or '*' to '_'
		(my $k_compliant = $k) =~ s/[-\/\*]/_/;

		# if the subvalue is an hash -> recursive call
		if(ref(${$hashref}{$k}) eq "HASH")
		{
			configuration_passthrough($output_file, ${$hashref}{$k}, "${key}_${k_compliant}");
		}
		# if an array, build a var suffixed "_list"
		elsif(ref ${$hashref}{$k} eq "ARRAY")
		{
			print $output_file "export ${key}_${k_compliant}_list=\"".join(" ", @{${$hashref}{$k}})."\"\n";
		}
		# else dump the value
		else
		{
			print $output_file "export ${key}_${k_compliant}=\"".((defined ${$hashref}{$k}) ? ${$hashref}{$k} : "undefined")."\"\n";
		}
	}
}

###########################################################################
# Save current configuration in YAML and ENV formats
# Args: No Args
sub configuration_save
{
	DumpFile("$buildir/config.yml", \%gconf) or die ("Unable to write YAML configuration file !");

	# build ENV
	open(my $output_file, '>', "$buildir/config.env") or die("Unable to write Shell-compliant configuration file !");
	configuration_passthrough($output_file, \%gconf, "pcvs");
	close($output_file);
}

###########################################################################
# List available environ vars availables with the current configuration
# Args: No Args
sub configuration_display_vars
{
	delete $gconf{'list-vars'};
	configuration_passthrough(*STDOUT, \%gconf, "pcvs");
}

###########################################################################
# Look for proper user configuration file to load
# Args: No Args
sub configuration_load
{
	my $prefix = "$internaldir/environment";
	my $name = lc(hostname);
	my ($user_name) = @_;
	my @avail_names = helper_lister("$internaldir/environment", "yml");

	# if no user file exists (not provided)
	if(! defined $user_name)
	{
		# we try to autodetect a file named with `hostname`.yml
		if (grep(/^$name$/, @avail_names))
		{
			$gconf{'config-target'} = $name;
			return "$prefix/$name.yml";
		}

		$name =~ s/[0-9]*//g;

		# if still not, we try to remove any number in the hostname
		if(grep(/^$name$/, @avail_names))
		{
			$gconf{'config-target'} = $name;
			return "$prefix/$name.yml";
		}
	}
	#if the user provides a command-line option to set a configuration file
	else
	{
		# if the user provides a file name, but we didn't found it in $internaldir/configuration/environment/
		die("Bad configuration value : $gconf{'config-target'} !") if (!grep(/^$user_name$/, @avail_names));
		return "$prefix/$user_name.yml";
	}

	return undef;
}

###########################################################################
# (Partial) attempt to validate the global validation.
# Some fields are checked, some others aren't. This step should be done thanks to 
# an dedicated Perl module (like JSON::Validate) but without embedding to much deps.
sub configuration_validate
{
	my $current_field;

	$current_field = $gconf{'validation'}{'run_wrapper'};
	(!$current_field || (-f "$internaldir/launchers/$current_field")) or die("\'validation/run_wrapper = $current_field\' is INVALID from configuration: $!");

	$current_field = $gconf{'validation'}{'compil_wrapper'};
	(!$current_field || (-f "$internaldir/launchers/$current_field")) or die("\'validation/compil_wrapper = $current_field\' is INVALID from configuration: $!");

	$current_field = $gconf{'compiler-target'};
	(!$current_field || (-f "$internaldir/configuration/compilers/$current_field.yml")) or die("\'compiler/target = $current_field\' is INVALID from configuration: $!");

	$current_field = $gconf{'runtime-target'};
	(!$current_field || (-f "$internaldir/configuration/runtimes/$current_field.yml")) or die("\'runtime/target = $current_field\' is INVALID from configuration: $!");

	$current_field = $gconf{'validation'}{'workers'};
	($current_field >= 0) or die("\'validation/nb_workers = $current_field\' is INVALID from configuration: Value must be positive");

	$current_field = $gconf{'cluster'}{'max_nodes'};
	($current_field >= 0) or die("\'cluster/max_nodes = $current_field\' is INVALID from configuration: Value must be strictly positive");

	$current_field = $gconf{'validation'}{'worker_mintime'};
	($current_field >= 0) or die("\'validation/worker_mintime = $current_field\' is INVALID from configuration: Value must be positive");

	$current_field = $gconf{'validation'}{'worker_maxtime'};
	($current_field >= 0) or die("\'validation/worker_mintime = $current_field\' is INVALID from configuration: Value must be positive and higher than validation/worker_mintime");
	
	$current_field = $gconf{'iterators'};
	(defined $current_field && keys %{$current_field} > 0) or die("Your configuration does not define at least one iterator !");

	$current_field = $gconf{'validation'}{'sched_policy'};
	($current_field >= 0 && $current_field <= 2) or die("\'validation/sched_policy = $current_field\' is INVALID from configuration: Value must be in range 0..2");

	foreach(('c', 'cxx', 'f77', 'f90', 'f95', 'f03', 'f08'))
	{
		$current_field = $gconf{'compiler'}{$_};
		if(defined $current_field)
		{
			`type $current_field 2> /dev/null`;
			die("\'runtime/c not found in PATH ! ($current_field)") if (($? >> 8) != 0);
		}
	}

	# if the user does not specify a 'select' option, consider using default directories
	if(!$gconf{'select'})
	{
		push @{$gconf{'select'}}, helper_list_avail_dirs() if(!$gconf{'select'});
	}

	# just check user-defined paths exists
	@{$gconf{'select'}} = split(/,/, join(",", @{$gconf{'select'}}));
	foreach my $el(@{$gconf{'select'}})
	{
		die("\'SRCDIR/$el\' does not exist ! (see --user-testfiles instead)") if(! -d "$srcdir/$el");
	}
}

1;
