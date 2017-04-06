BEGIN { SMAKE = "make" } {
  if ($1 !~ /^#/ &&  NF > 2) {
    printf "cd `echo %s|tr '[a-z]' '[A-Z]'`; %s clean;", $1, SMAKE;
    printf "%s CLASS=%s NPROCS=%s", SMAKE, $2, $3;
    if ( NF > 3 ) {
      printf " VERSION=%s", $4;
    }
    printf "; cd ..\n";
  }
}
