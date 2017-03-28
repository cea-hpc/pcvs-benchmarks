package PCVS::Configuration;
use strict;
use warnings;
use Exporter;
use Sys::Hostname;
use File::chdir;
use PCVS::Helper;
use YAML qw(LoadFile DumpFile); # YAML parser
use Data::Dumper;

our @ISA = 'Exporter';
our @EXPORT = qw(configuration_init configuration_build configuration_display_vars);
our @EXPORT_OK = qw();

our %gconf;
our ($buildir, $internaldir, $srcdir, $rundir);

sub load_yml
{
	my ($yml_path) = @_;

	die("Error with $yml_path: $!") if(! -f $yml_path);
	my $s = LoadFile($yml_path) ;
	return %{ $s };
}

sub configuration_init
{
	(%gconf) = @_;
	$rundir = $CWD;
	$buildir = $gconf{'build'};
	$srcdir = $gconf{src};
	$internaldir = "$srcdir/build_scripts";
}

sub configuration_build
{
	my %default_data = load_yml ("$internaldir/environment/default.yml");

	#update the current configuration hash with default value (no overlap w/ options)
	foreach my $key (keys %default_data){
		$gconf{$key}  = $default_data{$key} if(!exists $gconf{$key});
	}

	my $user_config = configuration_load();

	#if the user config file exists
	if(defined $user_config)
	{
		#delete $gconf{"config-target"};
		my %user_data = load_yml($user_config);

		#update default config with overriden values
		foreach my $key(keys %user_data)
		{
			exists $gconf{$key} or die("\'$key\' object does not exist. Please edit your configuration file !");
			#iterate over subkeys to update (YAML data is a two-level tree, should be replaced by recursion).
			if(ref($gconf{$key}) eq 'HASH')
			{
				foreach my $subkey (keys $user_data{$key})
				{
					exists $gconf{$key}{$subkey} or die("\'$key/$subkey\' object does not exist. Please edit your configuration file !");
					$gconf{$key}{$subkey} = $user_data{$key}{$subkey};
				}
			}
			else
			{
				$gconf{$key} = $user_data{$key};
			}
		}
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

	configuration_validate();
	configuration_save();
	return %gconf;
}

sub configuration_passthrough
{
	my ($output_file, $hashref, $key) = @_;

	foreach my $k (keys %{$hashref})
	{
		(my $k_compliant = $k) =~ s/[-\/\*]/_/;
		if(ref(${$hashref}{$k}) eq "HASH")
		{
			configuration_passthrough($output_file, ${$hashref}{$k}, "${key}_${k_compliant}");
		}
		elsif(ref ${$hashref}{$k} eq "ARRAY")
		{
			print $output_file "export ${key}_${k_compliant}_list=\"".join(" ", @{${$hashref}{$k}})."\"\n";
		}
		else
		{
			print $output_file "export ${key}_${k_compliant}=\"${$hashref}{$k}\"\n";
		}
	}
}

sub configuration_save
{
	DumpFile("$buildir/config.yml", \%gconf) or die ("Unable to write YAML configuration file !");

	# build ENV
	open(my $output_file, '>', "$buildir/config.env") or die("Unable to write Shell-compliant configuration file !");
	configuration_passthrough($output_file, \%gconf, "pcvs");
	close($output_file);
}

sub configuration_display_vars
{
	delete $gconf{'list-vars'};
	configuration_passthrough(*STDOUT, \%gconf, "pcvs");
}


sub configuration_load
{
	my $prefix = "$internaldir/environment";
	my $name = lc(hostname);
	my $user_name = $gconf{'config-target'};
	my @avail_names = helper_lister("$internaldir/environment", "yml");

	if(! defined $user_name)
	{
		if (grep(/^$name$/, @avail_names))
		{
			$gconf{'config-target'} = $name;
			return "$prefix/$name.yml";
		}

		$name =~ s/[0-9]*//g;

		if(grep(/^$name$/, @avail_names))
		{
			$gconf{'config-target'} = $name;
			return "$prefix/$name.yml";
		}
	}
	else
	{
		die("Bad configuration value : $gconf{'config-target'} !") if (!grep(/^$user_name$/, @avail_names));
		return "$prefix/$user_name.yml";
	}

	return undef;
}

#really painful to write, and still not complete
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

	$current_field = $gconf{'validation'}{'sched_policy'};
	($current_field >= 0 && $current_field <= 2) or die("\'validation/sched_policy = $current_field\' is INVALID from configuration: Value must be in range 0..2");


	if(!$gconf{'select'})
	{
		push @{$gconf{'select'}}, helper_list_avail_dirs() if(!$gconf{'select'});
	}

	foreach my $el(@{$gconf{'select'}})
	{
		die("\'SRCDIR/$el\' does not exist ! (see --user-testfiles instead)") if(! -d "$srcdir/$el");
	}

	if($gconf{'user-testfiles'})
	{
		foreach my $el(@{$gconf{'user-testfiles'}})
		{
			if(! ($el =~ /^\/.*$/))
			{
				$el = $rundir."/".$el;
			}

			die("Unable to find \'$el\' !") if(! -f $el);
		}
	}
}

1;
