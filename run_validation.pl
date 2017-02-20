#!/usr/bin/perl

use strict;
use warnings;
#use diagnostics;
use 5.012;
use English qw( -no_match_vars );

###########################################################################
#### major vars
###########################################################################
my %configuration;
my $srcdir;
my $internaldir;
my $buildir;

###########################################################################
#### COMPILATION TIME
###########################################################################
BEGIN {
	$srcdir = `readlink -e \`dirname $0\``; chomp $srcdir;
	$internaldir = $srcdir."/build_scripts";
}

#third-party libs loading
use lib "$srcdir/build_scripts/modules";

# dep inclusions
use File::Tee "tee"; # get the output sync'd w/ a file
use File::Path;
use File::chdir;
use Sys::Hostname;
use JSON; #parse JSON string into hash object
use File::Copy::Recursive qw(fcopy dircopy pathempty);
use Getopt::Long;    # parsing options
use Sys::Hostname;   # get current Hostname
use Data::Dumper; #to help printing hashes (can be removed)

###########################################################################
#### FUNCTIONS
###########################################################################
sub sec_to_dhms {
	my $t = shift;
	return int($t / 86400), (gmtime($t))[2, 1, 0];
}

sub clean_path
{
	my ($path,$with_clean) = @_;
	mkpath($path) if (! -d $path);
	(print " * Cleaning $path\n" and pathempty($path)) if ($with_clean); 
}

sub list_compilers
{
	opendir(my $dirlist, "$internaldir/configuration/compilers");
	my @conf_list = grep(s/\.conf$//, readdir($dirlist));
	print "Available Compilers (please use --with-compilers=*):\n";
	foreach my $el (@conf_list)
	{
		print " - $el\n";
	}
	exit(0);
}

sub list_runtimes
{
	opendir(my $dirlist, "$internaldir/configuration/runtimes");
	my @conf_list = grep(s/\.conf$//, readdir($dirlist));
	print "Available Runtimes (please use --with-runtime=*):\n";
	foreach my $el (@conf_list)
	{
		print " - $el\n";
	}
	exit(0);
}

sub list_configs
{
	opendir(my $dirlist, "$internaldir/environment");
	my @conf_list = grep(s/\.json$//, readdir($dirlist));
	print "Available Configurations (please use --with-config=*):\n";
	foreach my $el (@conf_list)
	{
		print " - $el\n";
	}
	exit(0);
}

sub print_help
{
	print "Usage: ./run_validation.pl [-h] [--select=/dirs]\n";
	exit(0);
}

sub print_summary
{
	my $config_target = "\'default\'";
	$config_target .= " & \'$configuration{'with-config'}\'" if(exists $configuration{'with-config'});
	print "\n >>>>>>>>>>>>>>>>>>>>> GLOBAL INFOS (see \$buildir/config.json) <<<<<<<<<<<<<<<<<<\n";
	print "      - Run Start date   : ".localtime()."\n";
	print "      - Host name        : ".hostname."\n";
	print "      - Source directory : $configuration{'src'}\n";
	print "      - Build directory  : $configuration{'build'}\n";
	print "      - Loaded Config.   : $config_target\n";
	print "      - Loaded Runtime   : \'$configuration{'runtime'}{'target'}\'\n";
	print "      - Loaded Compiler  : \'$configuration{'compiler'}{'target'}\'\n";
	print "      - Test directories :";
	# configuration is mapped with references: @{} dereferences it
	foreach my $dir(@{ $configuration{'select'} } )
	{
		print " $dir";
	}
	print "\n";
}

sub trap_signal
{
	print "Stopped by receiving a signal.\n";
	#my @process_list = grep(/[0-9]*/, `ps -o ppid=$$ | sort | uniq`);
	#if(scalar @process_list)
	#{
	#kill('KILL', @process_list);
	#}
	exit(127);
}

sub retrieve_user_configuration
{
	my $prefix = "$internaldir/environment";
	my $name = lc(hostname);
	if(not exists $configuration{'with-config'})
	{
		if (-f "$prefix/$name.json")
		{
			$configuration{'with-config'} = $name;
			return "$prefix/$name.json";
		}
		
		$name =~ s/[0-9]*//g;
		if(-f "$prefix/$name.json")
		{
			$configuration{'with-config'} = $name;
			return "$prefix/$name.json";
		}
	}
	else
	{
		my $user_config = "$internaldir/environment/$configuration{'with-config'}.json";
		if(-f $user_config)
		{
			return "$user_config";
		}
		else 
		{
			die("Bad configuration value : $configuration{'with-config'} !");
		}
	}

	return undef;
}

sub validate_run_configuration
{
	my $current_field;

	$current_field = $configuration{'validation'}{'run_wrapper'};
	(!$current_field or (-f "$internaldir/launchers/$current_field.sh")) or die("\'validation/run_wrapper = $current_field\' is INVALID from configuration: $!");
	
	$current_field = $configuration{'validation'}{'compil_wrapper'};
	(!$current_field or (-f "$internaldir/launchers/$current_field.sh")) or die("\'validation/compil_wrapper = $current_field\' is INVALID from configuration: $!");
	
	$current_field = $configuration{'compiler'}{'target'};
	(!$current_field or (-f "$internaldir/configuration/compilers/$current_field.conf")) or die("\'compiler/target = $current_field\' is INVALID from configuration: $!");
	
	$current_field = $configuration{'runtime'}{'target'};
	(!$current_field or (-f "$internaldir/configuration/runtimes/$current_field.conf")) or die("\'runtime/target = $current_field\' is INVALID from configuration: $!");
	
	$current_field = $configuration{'validation'}{'nb_workers'};
	($current_field ge 0) or die("\'validation/nb_workers = $current_field\' is INVALID from configuration: Value must be positive");
	
	$current_field = $configuration{'cluster'}{'max_nodes'};
	($current_field ge 0) or die("\'cluster/max_nodes = $current_field\' is INVALID from configuration: Value must be strictly positive");
	
	$current_field = $configuration{'validation'}{'worker_mintime'};
	($current_field ge 0) or die("\'validation/worker_mintime = $current_field\' is INVALID from configuration: Value must be positive");
	
	$current_field = $configuration{'validation'}{'worker_maxtime'};
	($current_field ge 0) or die("\'validation/worker_mintime = $current_field\' is INVALID from configuration: Value must be positive and higher than validation/worker_mintime");
	
	$current_field = $configuration{'validation'}{'sched_policy'};
	($current_field ge 0 and $current_field le 2) or die("\'validation/sched_policy = $current_field\' is INVALID from configuration: Value must be in range 0..2");
}

sub build_current_configuration
{

	my $default_config = "$internaldir/environment/default.json";
	my %default_data;
	#check if defualt config exists
	if(! -f $default_config)
	{
		die("Error: Default configuration file not found !!\n");
	}

	#read the default configuration => build a hash with it
	{
		local $/ = undef;
		open(my $stream, '<', $default_config) or die "could not open $default_config: $!";
		%default_data = %{ decode_json(<$stream>) };
	}

	#update the current configuration hash with default value (no overlap w/ options)
	foreach my $key (keys %default_data){
		$configuration{$key}  = $default_data{$key};
	}
	
	my $user_config = retrieve_user_configuration();
	my %user_data;

	#if the user config file exists
	if(defined $user_config)
	{
		#read the file and dump it into an hash
		local $/ = undef;
		open(my $stream, '<', $user_config) or die "could not open $user_config: $!";
		%user_data = %{decode_json(<$stream>)};
		
		#update default config with overriden values
		foreach my $key(keys %user_data)
		{
			exists $configuration{$key} or die("\'$key\' object does not exist. Please edit your configuration file !");
			#iterate over subkeys to update (JSON data is a two-level tree, should be extended to n-level tree).
			foreach my $subkey (keys $user_data{$key})
			{
				exists $configuration{$key}{$subkey} or die("\'$key/$subkey\' object does not exist. Please edit your configuration file !");
				$configuration{$key}{$subkey} = $user_data{$key}{$subkey};
			}
		}
	}

	validate_run_configuration();

	open(my $output_file, '>', "$buildir/config.json") || die("Unable to write current configuration file !");
	print $output_file encode_json(\%configuration);
	close($output_file);
}

sub prepare_run
{
	print "\n >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> PRE-RUN STEP <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<\n";
	#copy the webview in target dir
	print ' * Building the Webview (reachable at $buildir/webview//index.html)'."\n";
	dircopy("$internaldir/generation/jchronoss/tools/webview/*", "$buildir/") or die("Unable to copy the webview: $!");
	my $var = `$buildir/webview_gen_all.sh --skeleton`;
	#copy jsloc
	print " * Saving JsLoc into build directory.\n";
	dircopy("$internaldir/generation/jchronoss/tools/jsLoc/*", "$buildir/") or die("Unable to copy JsLoc: $!");
	
	#build JCHRONOSS
	print " * Building JCHRONOSS (-j$configuration{j})\n";
	clean_path("$buildir/tmp/build", 0);
	{
		local $CWD = "$buildir/tmp/build"; # equivalent to chdir()
		`cmake $internaldir/generation/jchronoss -DCMAKE_INSTALL_PREFIX=$buildir/tmp && make -j$configuration{'j'} install`;
	}
	clean_path("$buildir/tmp/traces", 1);
}

sub finalize_run
{

	print "\n >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> POST-RUN STEP <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<\n";
	#remove colors sequences
	#copy banners
	dircopy("$internaldir/resources/banners", "$buildir/banners");
	
	#generate tarball
	unlink("$buildir/last_results.tar.gz");

	#copy the generated webview
	clean_path("$buildir/last_results", 1);
	dircopy("$buildir/webview", "$buildir/last_results/webview") or die("Unable to save the webview in the archive !");
	fcopy("$buildir/webview_gen_all.sh", "$buildir/last_results/") or die("Unable to copy webview_gen_all.sh !");
	dircopy("$buildir/jsLoc", "$buildir/last_results/jsLoc") or die("Unable to save JsLoc in the archive !");
	fcopy("$buildir/jsLoc_gen_all.sh", "$buildir/last_results/jsLoc_gen_all.sh") or die("Unable to save JsLoc_gen_all.sh in the archive !");
	fcopy("$buildir/config.json", "$buildir/last_results/") or die ("Unable to save the configuration file !");
	dircopy("$buildir/tmp/traces", "$buildir/last_results/") or die ("Unable to copy trace files !");

	clean_path("$buildir/last_results/test_suite", 1);
	{
		local $CWD = "$buildir/last_results/test_suite/";
		my @list_files = `find $buildir/test_suite/ -iname 'output*.xml' 2> /dev/null`; chomp @list_files;
		foreach my $res_file(@list_files)
		{
			(my $new_path = $res_file) =~ s@$buildir@$buildir/last_results@;
			my $path = `dirname $new_path`; chomp $path;
			
			mkpath($path) if (! -d $path);
			fcopy($res_file, $path);
		}
	}

	print " * End Date : ".localtime()."\n";
	fcopy("$buildir/output.log", "$buildir/last_results/") if(exists $configuration{'log'});

	print " * Creating the archive (located at $buildir/last_results.tar.gz)\n";
	{
		local $CWD = $buildir;
		`tar -czf $buildir/last_results.tar.gz last_results`;
	}
}

sub configure_run
{
	print "\n >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> BUILD STEP <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<\n";
	clean_path("$buildir/test_suite", 1);
	print " * Building list_of_tests.xml\n";
	#foreach my $path (@{ $configuration{'select'}})
	{
		push($configuration{'select'}, "$srcdir/list_of_tests.xml");
	}
}

sub run
{	
	print "\n >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> RUN STEP <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<\n";
	my $nb_resources = "";
	my $nb_workers = "";
	my $compil_w ="";
	my $run_w = "";
	my $autokill = "";
	my $wlist = "";
	my $blist = "";
	
	$nb_resources = $configuration{'cluster'}{'max_nodes'};
	$nb_workers = $configuration{'validation'}{'nb_workers'};;
	$autokill = ("--autokill=$configuration{'validation'}{'autokill'}") if ($configuration{'validation'}{'autokill'} > 0);
	$run_w = ("--launcher=$internaldir/launcher/$configuration{'validation'}{'run_wrapper'}.sh") if ($configuration{'validation'}{'run_wrapper'});
	$compil_w = ("--compil-launcher=$internaldir/launcher/$configuration{'validation'}{'compil_wrapper'}.sh") if ($configuration{'validation'}{'compil_wrapper'});
	$compil_w = ("--compil-launcher=$internaldir/launcher/$configuration{'validation'}{'compil_wrapper'}.sh") if ($configuration{'validation'}{'compil_wrapper'});
	$compil_w = ("--compil-launcher=$internaldir/launcher/$configuration{'validation'}{'compil_wrapper'}.sh") if ($configuration{'validation'}{'compil_wrapper'});

	print $CWD."\n";
	system("$buildir/tmp/bin/jchronoss --build=$buildir/tmp $run_w $compil_w --nb-resources=$nb_resources ".join(" ", @{ $configuration{'select'} })." $autokill\n");
	die("Something happen with JCHRONOSS !: $!") if($? ne 0);
}


###########################################################################
#### MAIN
###########################################################################
$SIG{INT} = "trap_signal";

##### DEFAULT VALUES
$configuration{"j"} = 1;
GetOptions (
	\%configuration,
	"with-config=s",
	"list-configs|lco",
	"build=s",
	"select=s@",
	"j:i" ,
	"log!",
	"with-runtime=s",
	"list-runtimes|lr",
	"with-compiler=s",
	"list-compilers|lc",
	"regen!",
	"color|c",
	"clean",
	"verbose:i",
	"help"
)  or die("Abort due to error(s) while parsing arguments (see --help)!\n");

my $validation_start = time();

#check special cases (no validation run)
print_help() if ($configuration{help});
list_compilers() if ($configuration{'list-compilers'});
list_runtimes() if ($configuration{'list-runtimes'});
list_configs() if ($configuration{'list-configs'});

#add extra infos to the global configuration
$configuration{'src'} = $srcdir;
if(! exists $configuration{'build'})
{
	$configuration{'build'} = $srcdir."/build";
} else
{
	$_ = $configuration{'build'};
	if(/^[^\/].*$/) #check if relative path to prepend it with $CWD
	{
		$configuration{'build'}	= "$CWD/".$configuration{'build'};
	}
}
$buildir = $configuration{'build'};

#create build directory and create a tee file if logging is enabled
clean_path($buildir, 0);
tee(STDOUT, '>', "$buildir/output.log") if ($configuration{'log'});

build_current_configuration();

my $banner = `cat $internaldir/resources/banners/test_suite_banner`;
print $banner;

print_summary();

prepare_run();
configure_run();
run();
finalize_run();

my ($d, $h, $m, $s) = sec_to_dhms(time()-$validation_start);
print "\n==> Validation completed in $d day(s), $h hour(s), $m minute(s) and $s second(s))\n";
