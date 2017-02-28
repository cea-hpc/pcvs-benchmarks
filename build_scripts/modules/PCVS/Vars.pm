package PCVS::Vars;
use strict;
use warnings;
use Exporter;
use File::chdir;

our @ISA = 'Exporter';
our @EXPORT = qw(internaldir buildir rundir);

our($internaldir, $buildir, $rundir);


$internaldir = $srcdir."/build_scripts";
$buildir = $srcdir."/build";
$rundir = $CWD;

