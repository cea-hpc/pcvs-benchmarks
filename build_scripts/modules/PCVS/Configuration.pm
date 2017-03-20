package PCVS::Configuration;
use strict;
use warnings;
use Exporter;
use Sys::Hostname;
use File::chdir;
use PCVS::Helper;
use JSON; #parse JSON string into hash object

our @ISA = 'Exporter';
our @EXPORT = qw(configuration_init configuration_build configuration_display_vars);
our @EXPORT_OK = qw();

our %gconf;
our ($buildir, $internaldir, $srcdir, $rundir);

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
	my $default_config = "$internaldir/environment/default.json";
	my %default_data = dump_and_parse_json ("$default_config");

	#update the current configuration hash with default value (no overlap w/ options)
	foreach my $key (keys %default_data){
		$gconf{$key}  = $default_data{$key} if(!exists $gconf{$key});
	}

	my $user_config = retrieve_user_configuration();

	#if the user config file exists
	if(defined $user_config)
	{
		my %user_data = dump_and_parse_json($user_config);

		#update default config with overriden values
		foreach my $key(keys %user_data)
		{
			exists $gconf{$key} or die("\'$key\' object does not exist. Please edit your configuration file !");
			#iterate over subkeys to update (JSON data is a two-level tree, should be extended to n-level tree).
			if(ref($gconf{$key} eq 'HASH'))
			{
				foreach my $subkey (keys $user_data{$key})
				{
					exists $gconf{$key}{$subkey} or die("\'$key/$subkey\' object does not exist. Please edit your configuration file !");
					$gconf{$key}{$subkey} = $user_data{$key}{$subkey};
				}
			}
		}
	}

	my $target = $gconf{'compiler-target'};
	if($target)
	{
		delete $gconf{'compiler-target'};
		my $cc_config = "$internaldir/configuration/compilers/$target.json";
		die("Unable to find $target as Compiler target (compiler-target)") if(! -f $cc_config);
		my %cc_data = dump_and_parse_json($cc_config);
		$gconf{'compiler'} = \%cc_data;
		$gconf{'compiler'}{'target'} = $target;
	}
	else
	{
		die("You must specify a Compilation target (compiler/target)");
	}

	$target = $gconf{'runtime-target'};
	if($target)
	{	
		delete $gconf{'runtime-target'};
		my $run_config = "$internaldir/configuration/runtimes/$target.json";
		die("Unable to find $target as Runtime target (runtime-target)") if(! -f $run_config);
		my %run_data = dump_and_parse_json($run_config);
		$gconf{'runtime'} = \%run_data; 
		$gconf{'runtime'}{'target'} = $target;
	}
	else
	{
		die("You must specify a Runtime target (runtime/target)");
	}

	validate_run_configuration();
	dump_run_configuration();
	return %gconf;
}

sub dump_and_parse_json
{
	my ($json_path) = @_;

	die("Error with $json_path: $!") if(! -f $json_path);

	local $/ = undef;
	open(my $stream, '<', $json_path) or die "Error with $json_path: $!";

	return %{ decode_json(<$stream>) };
}

sub iterate_sub_configuration
{
	my $output_file = shift;
	my $hashref = shift;
	my $key = shift;

	foreach my $k (keys %{$hashref})
	{
		if(ref(${$hashref}{$k}) eq "HASH")
		{
			iterate_sub_configuration($output_file, ${$hashref}{$k}, "${key}_${k}");
		}
		elsif(ref ${$hashref}{$k} eq "ARRAY")
		{
			print $output_file "export ${key}_${k}_list=\"".join(" ", @{${$hashref}{$k}})."\"\n";
		}
		else
		{
			print $output_file "export ${key}_${k}=\"${$hashref}{$k}\"\n";
		}
	}
}

sub dump_run_configuration
{
	# build JSON
	my $output_file;
	open($output_file, '>', "$buildir/config.json") || die("Unable to write JSON configuration file !");
	print $output_file encode_json(\%gconf);
	close($output_file);

	# build ENV
	open($output_file, '>', "$buildir/config.env") || die("Unable to write Shell-compliant configuration file !");
	iterate_sub_configuration($output_file, \%gconf, "pcvs");
	close($output_file);
}

sub configuration_display_vars
{
	print "Available Variables for the current run :\n";
	delete $gconf{'list-vars'};
	iterate_sub_configuration(*STDOUT, \%gconf, "pcvs");
	exit 0;
}


sub retrieve_user_configuration
{
	my $prefix = "$internaldir/environment";
	my $name = lc(hostname);
	my $user_name = $gconf{'config-target'};
	my @avail_names = helper_lister("$internaldir/environment", "json");

	if(not defined $user_name)
	{
		if (grep(/^$name$/, @avail_names))
		{
			$gconf{'config-target'} = $name;
			return "$prefix/$name.json";
		}

		$name =~ s/[0-9]*//g;
		if(grep(/^$name$/, @avail_names))
		{
			$gconf{'config-target'} = $name;
			return "$prefix/$name.json";
		}
	}
	else
	{
		die("Bad configuration value : $gconf{'config-target'} !") if (!grep(/^$user_name$/, @avail_names));
		return "$prefix/$user_name.json";
	}

	return undef;
}

sub validate_run_configuration
{
	my $current_field;

	$current_field = $gconf{'validation'}{'run_wrapper'};
	(!$current_field or (-f "$internaldir/launchers/$current_field.sh")) or die("\'validation/run_wrapper = $current_field\' is INVALID from configuration: $!");

	$current_field = $gconf{'validation'}{'compil_wrapper'};
	(!$current_field or (-f "$internaldir/launchers/$current_field.sh")) or die("\'validation/compil_wrapper = $current_field\' is INVALID from configuration: $!");

	$current_field = $gconf{'compiler-target'};
	(!$current_field or (-f "$internaldir/configuration/compilers/$current_field.json")) or die("\'compiler/target = $current_field\' is INVALID from configuration: $!");

	$current_field = $gconf{'runtime-target'};
	(!$current_field or (-f "$internaldir/configuration/runtimes/$current_field.json")) or die("\'runtime/target = $current_field\' is INVALID from configuration: $!");

	$current_field = $gconf{'validation'}{'workers'};
	($current_field ge 0) or die("\'validation/nb_workers = $current_field\' is INVALID from configuration: Value must be positive");

	$current_field = $gconf{'cluster'}{'max_nodes'};
	($current_field ge 0) or die("\'cluster/max_nodes = $current_field\' is INVALID from configuration: Value must be strictly positive");

	$current_field = $gconf{'validation'}{'worker_mintime'};
	($current_field ge 0) or die("\'validation/worker_mintime = $current_field\' is INVALID from configuration: Value must be positive");

	$current_field = $gconf{'validation'}{'worker_maxtime'};
	($current_field ge 0) or die("\'validation/worker_mintime = $current_field\' is INVALID from configuration: Value must be positive and higher than validation/worker_mintime");

	$current_field = $gconf{'validation'}{'sched_policy'};
	($current_field ge 0 and $current_field le 2) or die("\'validation/sched_policy = $current_field\' is INVALID from configuration: Value must be in range 0..2");


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
