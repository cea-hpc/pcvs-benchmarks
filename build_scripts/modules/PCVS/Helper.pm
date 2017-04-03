package PCVS::Helper;

use strict;
use warnings;
use Exporter;
use File::Copy::Recursive qw(pathempty);
use File::Path;

our @ISA = 'Exporter';
our @EXPORT = qw(helper_init helper_do_not_run_validation helper_list_avail_dirs helper_lister helper_clean_path helper_convert_time helper_detect_compiler helper_prefix_if_exists helper_convert_absolute helper_uniq);
our @EXPORT_OK = qw();

our $conf;
our ($buildir, $internaldir, $srcdir);


###########################################################################
# Helper init: register the configuration for using it later
# Args:
#  - $conf: a reference to system %gconf
sub helper_init
{
	($conf) = @_;

	#aliases to be faster
	$buildir = $conf->{'build'};
	$srcdir = $conf->{src};
	$internaldir = "$srcdir/build_scripts";
	
}


###########################################################################
# Convert a path to an absolute path with the given prefix
# Args:
#   - $path : path to check
#   - $prefix: path to preprend
#
# Returns:
#  The string, modified or not
sub helper_convert_absolute
{
	my($path, $prefix) = @_;

	#convert relative  -> absolute path
	if($path =~ /^[^\/].*$/)
	{
		$path = $prefix.$path;
	}

	return $path;
}

###########################################################################
# Make an array with unique values
# Args: 
#  - @_: the array
#
# Returns: the array without duplicates
sub helper_uniq {
	my %seen;
	return grep { !$seen{$_}++ } @_;
}

###########################################################################
# Convert time in seconds in an array follwing DHMS format
# Args
#  - $t: time, in seconds
#
# Returns 4-value array: (DD, HH, MM, SS)
sub helper_convert_time {
	my $t = shift;
	return int($t / 86400), (gmtime($t))[2, 1, 0];
}

###########################################################################
# Create and clean paths
# Args:
#   - $path : path to create if not exist
#   - $with_clean : has the path to be cleaned before returning from the function ?
sub helper_clean_path
{
	my ($path,$with_clean) = @_;
	(mkpath($path) or die("mkpath(): $!")) if (! -d $path);
	(pathempty($path) or die ("pathempty(): $!")) if ($with_clean); 
}

###########################################################################
# Prefix the value with a pattern if the value is defined (shortcut)
# Args:
#   - $value: the value to check
#   - $prefix : prefix to prepend
#
# Returns:
#   - The value, prepended or not
sub helper_prefix_if_exists
{
	my ($prefix, $value) = @_;
	return ($value  ? $prefix.$value : "");
}

###########################################################################
# List content of directory
# Args:
#  - $filepath: directory to look for
#  - $ext: the extension to remove from list entries
#
# Returns:
# List of files found in directory
sub  helper_lister
{
	my ($filepath, $ext) = @_;
	opendir(my $dirlist, $filepath);
	return grep(s/\.$ext$//, readdir($dirlist));
}

###########################################################################
# List main directories used for PCVS validation. We think it is a bad idea
# to have the hard-written list of directories here. This should be replaced
# Args: No Args
#
# Returns:
# Array of directories matching hard-written pattern
sub  helper_list_avail_dirs
{
	opendir(my $dirlist, "$srcdir");
	return grep { -d "$srcdir/$_" and /^(applications|MPI|OpenMP|performance|reproducers|Threads|Hybrid)/} readdir($dirlist);
}

###########################################################################
# Check if user set an option not requiring to run the validation
# Args: No Args
sub helper_do_not_run_validation
{
	my $ret = 0;
	if ($conf->{help})
	{
		print "Usage: ./run_validation.pl [-h] [--select=/dirs]\n";
		print "TBW\n";
		$ret = 1;
	}
	if ($conf->{'list-compilers'})
	{
		print "Compilers: ".join(", ", helper_lister("$internaldir/configuration/compilers", "yml"))."\n";
		$ret = 1;
	}

	if ($conf->{'list-runtimes'})
	{
		print "Runtimes: ".join(", ", helper_lister("$internaldir/configuration/runtimes", "yml"))."\n";
		$ret = 1;
	}

	if ($conf->{'list-configs'})
	{
		print "Environments: ".join(", ", helper_lister("$internaldir/environment", "yml"))."\n";
		$ret = 1;
	}

	if ($conf->{'list-directories'})
	{
		print "Available root directories: ".join(", ", helper_list_avail_dirs())."\n";
		$ret = 1;
	}
	
	if ($conf->{'list-groups'})
	{
		print "Group definitions: ".join(", ", helper_lister("$internaldir/configuration/groups", "yml"))."\n";
		$ret = 1;
	}

	return $ret;
}

###########################################################################
# Detect compiler, depending on file extension from parameter list.
# Sadly, switch-case construct does not exist in Perl core and we want to restrict
# number of embedded modules.
# Args:
#  - @list_files: list of source files (only the first one is used)
#
# Returns:
# A string matching to a defined 'compiler' field: c, cxx, f77, ...
sub helper_detect_compiler
{
	my @list_files = @_;

	#only check the first one (lazy)
	if($list_files[0] =~ /([^\.]*)$/)
	{
		my $ext = $1;
		#Still no switch-case implementation in core Perl...
		if($ext =~ /^(c|c90|c99|c11)$/) # C
		{
			return "c";
		}
		elsif($ext =~ /^(C|cxx|cpp|c\+\+)$/) # C++
		{
			return "cxx";
		}
		elsif($ext =~ /^(f|F)([0-9]*)$/) # fortran
		{
			if(!$2 or $2 =~ /f77/)
			{
				return "f77";
			}
			elsif($2 =~ /90/)
			{
				return "f90";
			}
			elsif($2 =~ /95/)
			{
				return "f95";
			}
			elsif($2 =~ /03|2003/)
			{
				return "f03";
			}
			elsif($2 =~ /^(08|2008)$/)
			{
				return "f08";
			}
		}
	}

	return undef;

}

1;
