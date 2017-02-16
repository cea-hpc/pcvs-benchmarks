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
use File::Chdir;
use Sys::Hostname;
use JSON; #parse JSON string into hash object
use File::Copy::Recursive qw(fcopy dircopy pathempty);
use Getopt::Long;    # parsing options
use Sys::Hostname;   # get current Hostname
use Data::Dumper; #to help printing hashes (can be removed)

###########################################################################
#### FUNCTIONS
###########################################################################
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
	print " * GLOBAL INFOS (please see config.json for further details): \n";
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
	print "\n\n";
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

sub validate_user_configuration
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
			return "$prefix/$name.json";
		}
		else 
		{
			die("Bad configuration value : $configuration{'with-config'} !");
		}
	}

	return undef;
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
	
	my $user_config = validate_user_configuration();
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

	open(my $output_file, '>', "$configuration{'build'}/config.json") || die("Unable to write current configuration file !");
	print $output_file encode_json(\%configuration);
	close($output_file);
}

sub prepare_run
{
	#copy the webview in target dir
	print ' * Building the Webview (reachable at $buildir/webview//index.html)'."\n";
	dircopy("$internaldir/generation/jchronoss/tools/webview/*", "$buildir/") or die("Unable to copy the webview: $!");
	my $var = `$buildir/webview_gen_all.sh --skeleton --new=.`;
	#copy jsloc
	print " * Saving JsLoc into build directory.\n";
	dircopy("$internaldir/generation/jchronoss/tools/jsLoc/*", "$buildir/") or die("Unable to copy JsLoc: $!");
	
	#build JCHRONOSS
	print " * Building JCHRONOSS (-j$configuration{j})\n";
	mkpath("$buildir/tmp/build") if (! -d "$buildir/tmp/build");
	{
		$CWD = "$buildir/tmp/build"; # equivalent to chdir()
		`cmake $internaldir/generation/jchronoss -DCMAKE_INSTALL_PREFIX=$buildir/tmp && make -j$configuration{'j'} install`;
	}
	#
}

sub finalize_run
{

	#remove colors sequences
	#copy banners
	dircopy("$internaldir/resources/banners", "$buildir/");
	#generate tarball
	unlink("$buildir/last_results.tar.gz");
	mkpath("$buildir/last_results") if (! -d "$buildir/last_results");
	pathempty("$buildir/last_results");
	print "looking at $buildir/test_suite/*.xml";
	foreach my $res_file(<"*.xml">)
	{
		print $res_file."\n";
	}
}

sub configure_run
{
}

sub run
{
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

#check special cases (no validation run)
print_help() if ($configuration{help});
list_compilers() if ($configuration{'list-compilers'});
list_runtimes() if ($configuration{'list-runtimes'});
list_configs() if ($configuration{'list-configs'});

#add extra infos to the global configuration
$configuration{'src'} = $srcdir;
$configuration{'build'} = $buildir = $srcdir."/build" if (!$configuration{'build'});

#create build directory and create a tee file if logging is enabled
mkpath($buildir) if (! -d $buildir);
tee(STDOUT, '>', "$buildir/output.log") if ($configuration{'log'});

build_current_configuration();

my $banner = `cat $internaldir/resources/banners/test_suite_banner`;
print $banner;

print_summary();

prepare_run();
configure_run();
run();
finalize_run();
