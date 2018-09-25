#!/usr/bin/env perl

use strict;
use warnings;

use JSON;
use Data::Dumper;

my $ret = 42;
my $out = "";
(print "Missing arg" and exit $ret) if(scalar(@ARGV) le 0);
my $arg = decode_json($ARGV[0]);

# set regex depending on the condition
my @regexes;
push (@regexes, @{ %{ $arg }{'regex-err'}  } ) if (defined %{$arg}{'regex-err'});
push (@regexes, @{ %{ $arg }{'regex-warn'} } ) if (defined %{$arg}{'regex-warn'});
my $nb_success = 0;
my @matches = ();

(print "No regex to filter with !" and exit 0) if(scalar(@regexes) eq 0);
while(<STDIN>)
{
	foreach my $reg (@regexes )
	{
		if(grep { /$reg/ } $_)
		{
			push @matches, $reg;
			$nb_success+=1;
		}
	}

	$out .= $_;
}

my $diff = scalar(@regexes) - $nb_success;
print scalar(@regexes)."- $nb_success = $diff\n";
if($diff gt 0)
{
	print "FAIL !!! $diff regex(es) did not match: \n";
	foreach my $r (@regexes)
	{
		if(not grep (/$r/, @matches))
		{
			print "\t - $r\n";
		}
	}

	print "ACTUAL OUTPUT FOR DEBUG==========\n";
	print "$out";
	print "\n=================================\n";
	$ret = 1;
}
else
{
	print "SUCCESS !!! All regexes found\n";
	$ret = 0;
}


exit $ret;
