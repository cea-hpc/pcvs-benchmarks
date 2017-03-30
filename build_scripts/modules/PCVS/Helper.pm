package PCVS::Helper;

use strict;
use warnings;
use Exporter;
use File::Copy::Recursive qw(pathempty);
use File::Path;

our @ISA = 'Exporter';
our @EXPORT = qw(helper_init helper_do_not_run_validation helper_list_avail_dirs helper_lister helper_clean_path helper_convert_time helper_detect_compiler helper_ceil helper_floor);
our @EXPORT_OK = qw();

our $conf;
our ($buildir, $internaldir, $srcdir);

sub helper_init
{
	($conf) = @_;
	$buildir = $conf->{'build'};
	$srcdir = $conf->{src};
	$internaldir = "$srcdir/build_scripts";
	
}

sub helper_convert_time {
	my $t = shift;
	return int($t / 86400), (gmtime($t))[2, 1, 0];
}

sub helper_clean_path
{
	my ($path,$with_clean) = @_;
	mkpath($path) if (! -d $path);
	(pathempty($path)) if ($with_clean); 
}

sub helper_floor
{
	my ($val) = @_;
	return ($val == int($val)) ? $val : int($val);
}

sub helper_ceil
{
	my ($val) = @_;
	return ($val == int($val)) ? $val : int($val)+1;
}

sub  helper_lister
{
	my ($filepath, $ext) = @_;
	opendir(my $dirlist, $filepath);
	return grep(s/\.$ext$//, readdir($dirlist));
}

sub  helper_list_avail_dirs
{
	opendir(my $dirlist, "$srcdir");
	return grep { -d "$srcdir/$_" and /^(applications|MPI|OpenMP|performance|reproducers|Threads|Hybrid)/} readdir($dirlist);
}

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
