#!/usr/bin/perl

use strict;
use warnings;

#major variables
my $PCVS_srcdir;
my $PCVS_internaldir;

#set vars at compile time
BEGIN {
$PCVS_srcdir = `readlink -e \`dirname $0\``; chomp $PCVS_srcdir;
$PCVS_internaldir = $PCVS_srcdir."/build_scripts";
}

#third-party libs loading
use lib "$PCVS_srcdir/build_scripts/modules";

# dep inclusions
use File::Tee "tee"; # get the output sync'd w/ a file
use Getopt::Long;    # parsing options


sub list_compilers
{
	opendir(my $dirlist, "$PCVS_internaldir/configuration/compilers");
	my @conf_list = grep(s/\.conf$//, readdir($dirlist));
	print "Available Compilers (please use --with-compilers=*):\n";
	foreach my $el (@conf_list)
	{
		print " - $el\n";
	}
}

sub list_runtimes
{
	opendir(my $dirlist, "$PCVS_internaldir/configuration/runtimes");
	my @conf_list = grep(s/\.conf$//, readdir($dirlist));
	print "Available Compilers (please use --with-runtime=*):\n";
	foreach my $el (@conf_list)
	{
		print " - $el\n";
	}
}

sub print_help
{
	print "Usage: ./run_validation.pl [-h] [--select=/dirs]\n"
}

###########################################################################
#### command-line set vars
###########################################################################
my $PCVS_buildir = $PCVS_srcdir."build";
my $PCVS_searchdirs = ".";
my $PCVS_verbose;
my $need_help = 0;
my $make_j = 0;
my $need_color = 0;

###########################################################################
#### MAIN
###########################################################################
tee(STDOUT, '>', 'output.log');

my $banner = `cat $PCVS_internaldir/resources/banners/test_suite_banner`;
print $banner;

GetOptions (
	"build=s" => \$PCVS_buildir,
	"select=s" => \$PCVS_searchdirs,
	"verbose" => \$PCVS_verbose,
	"j:i" => \$make_j ,
	"list-runtimes|lr" => sub { list_runtimes(); exit 0;},
	"list-compilers|lc" => sub { list_compilers(); exit 0;},
	"color" => \$need_color,
	"help" => sub {print_help(); exit 0;}
	)  or die("Abort due to error(s) while parsing arguments !\n");

print "src = $PCVS_srcdir\n";
print "build = $PCVS_buildir\n";
print "search = $PCVS_searchdirs\n";
