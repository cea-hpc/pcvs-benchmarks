/* 
 * This utility configures a NPB to be built for a specific class. 
 * It creates a file "npbparams.h" 
 * in the source directory. This file keeps state information about 
 * which size of benchmark is currently being built (so that nothing
 * if unnecessarily rebuilt) and defines (through PARAMETER statements)
 * the number of nodes and class for which a benchmark is being built. 

 * The utility takes 3 arguments: 
 *       setparams benchmark-name class
 *    benchmark-name is "sp-mz", "bt-mz", or "lu-mz"
 *    class is the size of the benchmark
 * These parameters are checked for the current benchmark. If they
 * are invalid, this program prints a message and aborts. 
 * If the parameters are ok, the current npbsize.h (actually just
 * the first line) is read in. If the new parameters are the same as 
 * the old, nothing is done, but an exit code is returned to force the
 * user to specify (otherwise the make procedure succeeds but builds a
 * binary of the wrong name).  Otherwise the file is rewritten. 
 * Errors write a message (to stdout) and abort. 
 * 
 * This program makes use of two extra benchmark "classes"
 * class "X" means an invalid specification. It is returned if
 * there is an error parsing the config file. 
 * class "U" is an external specification meaning "unknown class"
 * 
 * Unfortunately everything has to be case sensitive. This is
 * because we can always convert lower to upper or v.v. but
 * can't feed this information back to the makefile, so typing
 * make CLASS=a and make CLASS=A will produce different binaries.
 *
 * 
 */

#include <sys/types.h>
#include <stdlib.h>
#include <stdio.h>
#include <ctype.h>
#include <string.h>
#include <time.h>
#include <math.h>

/*
 * This is the master version number for this set of 
 * NPB benchmarks. It is in an obscure place so people
 * won't accidentally change it. 
 */

#define VERSION "3.3.1"

/* controls verbose output from setparams */
/* #define VERBOSE */

#define MAX_X_ZONES 128
#define MAX_Y_ZONES 128
#define FILENAME    "npbparams.h"
#define DESC_LINE   "c CLASS = %c\n"
#define DEF_CLASS_LINE     "#define CLASS '%c'\n"
#define FINDENT  "        "
#define CONTINUE "     > "
#define max(a,b)    (((a) > (b)) ? (a) : (b))

void get_info(char *argv[], int *typep, char *classp);
void check_info(int type, char class);
void read_info(int type, char *classp);
void write_info(int type, char class);
void write_sp_info(FILE *fp, char class);
void write_bt_info(FILE *fp, char class);
void write_lu_info(FILE *fp, char class);
void write_compiler_info(int type, FILE *fp);
void write_convertdouble_info(int type, FILE *fp);
void check_line(char *line, char *label, char *val);
int  check_include_line(char *line, char *filename);
void put_string(FILE *fp, char *name, char *val);
void put_def_string(FILE *fp, char *name, char *val);
void put_def_variable(FILE *fp, char *name, char *val);
void zone_max_xysize(double ratio, int gx_size, int gy_size,
      	           int x_zones, int y_zones, int *max_lsize);

enum benchmark_types {SP, BT, LU};

int main(int argc, char *argv[])
{
  int type;
  char class, class_old;
  
  if (argc != 3) {
    printf("Usage: %s benchmark-name class\n", argv[0]);
    exit(1);
  }

  /* Get command line arguments. Make sure they're ok. */
  get_info(argv, &type, &class);
  if (class != 'U') {
#ifdef VERBOSE
    printf("setparams: For benchmark %s: class = %c\n", 
	   argv[1], class); 
#endif
    check_info(type, class);
  }

  /* Get old information. */
  read_info(type, &class_old);
  if (class != 'U') {
    if (class_old != 'X') {
#ifdef VERBOSE
      printf("setparams:     old settings: class = %c\n", 
	     class_old); 
#endif
    }
  } else {
    printf("setparams:\n\
  *********************************************************************\n\
  * You must specify CLASS to build this benchmark                    *\n\
  * For example, to build a class A benchmark, type                   *\n\
  *       make {benchmark-name} CLASS=A                               *\n\
  *********************************************************************\n\n"); 

    if (class_old != 'X') {
#ifdef VERBOSE
      printf("setparams: Previous settings were CLASS=%c \n", class_old); 
#endif
    }
    exit(1); /* exit on class==U */
  }

  /* Write out new information if it's different. */
  if (class != class_old) {
#ifdef VERBOSE
    printf("setparams: Writing %s\n", FILENAME); 
#endif
    write_info(type, class);
  } else {
#ifdef VERBOSE
    printf("setparams: Settings unchanged. %s unmodified\n", FILENAME); 
#endif
  }

  return 0;
}


/*
 *  get_info(): Get parameters from command line 
 */

void get_info(char *argv[], int *typep, char *classp) 
{

  *classp = *argv[2];

  if      (!strcmp(argv[1], "sp-mz") || !strcmp(argv[1], "SP-MZ")) *typep = SP;
  else if (!strcmp(argv[1], "bt-mz") || !strcmp(argv[1], "BT-MZ")) *typep = BT;
  else if (!strcmp(argv[1], "lu-mz") || !strcmp(argv[1], "LU-MZ")) *typep = LU;
  else {
    printf("setparams: Error: unknown benchmark type %s\n", argv[1]);
    exit(1);
  }
}

/*
 *  check_info(): Make sure command line data is ok for this benchmark 
 */

void check_info(int type, char class) 
{

  /* check class */
  if (class != 'S' && 
      class != 'W' && 
      class != 'A' && 
      class != 'B' && 
      class != 'C' && 
      class != 'D' && 
      class != 'E' && 
      class != 'F') {
    printf("setparams: Unknown benchmark class %c\n", class); 
    printf("setparams: Allowed classes are \"S\", \"W\", \"A\" through \"F\"\n");
    exit(1);
  }

}


/* 
 * read_info(): Read previous information from file. 
 *              Not an error if file doesn't exist, because this
 *              may be the first time we're running. 
 *              Assumes the first line of the file is in a special
 *              format that we understand (since we wrote it). 
 */

void read_info(int type, char *classp)
{
  int nread;
  FILE *fp;
  fp = fopen(FILENAME, "r");
  if (fp == NULL) {
#ifdef VERBOSE
    printf("setparams: INFO: configuration file %s does not exist (yet)\n", FILENAME); 
#endif
    goto abort;
  }
  
  /* first line of file contains info (fortran), first two lines (C) */

  switch(type) {
      case SP:
      case BT:
      case LU:
          nread = fscanf(fp, DESC_LINE, classp);
          if (nread != 1) {
            printf("setparams: Error parsing config file %s. Ignoring previous settings\n", FILENAME);
            goto abort;
          }
          break;
      default:
        /* never should have gotten this far with a bad name */
        printf("setparams: (Internal Error) Benchmark type %d unknown to this program\n", type); 
        exit(1);
  }

  fclose(fp);

  return;

 abort:
  *classp = 'X';
  return;
}


/* 
 * write_info(): Write new information to config file. 
 *               First line is in a special format so we can read
 *               it in again. Then comes a warning. The rest is all
 *               specific to a particular benchmark. 
 */

void write_info(int type, char class) 
{
  FILE *fp;
  fp = fopen(FILENAME, "w");
  if (fp == NULL) {
    printf("setparams: Can't open file %s for writing\n", FILENAME);
    exit(1);
  }

  switch(type) {
      case SP:
      case BT:
      case LU:
          /* Write out the header */
          fprintf(fp, DESC_LINE, class);
          /* Print out a warning so bozos don't mess with the file */
          fprintf(fp, "\
c  \n\
c  \n\
c  This file is generated automatically by the setparams utility.\n\
c  It sets the number of processors and the class of the NPB\n\
c  in this directory. Do not modify it by hand.\n\
c  \n");

          break;
      default:
          printf("setparams: (Internal error): Unknown benchmark type %d\n", 
                                                                         type);
          exit(1);
  }

  /* Now do benchmark-specific stuff */
  switch(type) {
  case SP:
    write_sp_info(fp, class);
    break;	      
  case BT:	      
    write_bt_info(fp, class);
    break;	      
  case LU:	      
    write_lu_info(fp, class);
    break;	      
  default:
    printf("setparams: (Internal error): Unknown benchmark type %d\n", type);
    exit(1);
  }
  write_convertdouble_info(type, fp);
  write_compiler_info(type, fp);
  fclose(fp);
  return;
}


/* 
 * write_sp_info(): Write SP specific info to config file
 */

void write_sp_info(FILE *fp, char class) 
{
  int  gx_size, gy_size, gz_size, niter, x_zones, y_zones;
  int  max_lsize;
  char *dt, *ratio, *int_type;

  int_type="integer";
  if      (class == 'S') 
  {gx_size = 24; gy_size=24; gz_size=6; 
   x_zones = y_zones = 2;
   dt = "0.015d0";   niter = 100;}
  else if (class == 'W') 
  {gx_size = 64; gy_size=64; gz_size=8;
   x_zones = y_zones = 4;
   dt = "0.0015d0";  niter = 400;}
  else if (class == 'A') 
  {gx_size = 128; gy_size=128; gz_size=16; 
   x_zones = y_zones = 4;
   dt = "0.0015d0";  niter = 400;}
  else if (class == 'B') 
  {gx_size = 304; gy_size=208; gz_size=17; 
   x_zones = y_zones = 8;
   dt = "0.001d0";   niter = 400;}
  else if (class == 'C') 
  {gx_size = 480; gy_size=320; gz_size=28; 
   x_zones = y_zones = 16;
   dt = "0.00067d0"; niter = 400;}
  else if (class == 'D') 
  {gx_size = 1632; gy_size=1216; gz_size=34; 
   x_zones = y_zones = 32;
   dt = "0.0003d0"; niter = 500;}
  else if (class == 'E') 
  {gx_size = 4224; gy_size=3456; gz_size=92; 
   x_zones = y_zones = 64; int_type="integer*8";
   dt = "0.0002d0"; niter = 500;}
  else if (class == 'F') 
  {gx_size = 12032; gy_size=8960; gz_size=250; 
   x_zones = y_zones = 128; int_type="integer*8";
   dt = "0.0001d0"; niter = 500;}
  else {
    printf("setparams: Internal error: invalid class %c\n", class);
    exit(1);
  }
  ratio = "1.d0";
  zone_max_xysize(1.0, gx_size, gy_size, x_zones, y_zones, &max_lsize);

  fprintf(fp, "%scharacter class\n", FINDENT);
  fprintf(fp, "%sparameter (class='%c')\n", FINDENT,class);
  fprintf(fp, "%sinteger x_zones, y_zones\n", FINDENT);
  fprintf(fp, "%sparameter (x_zones=%d, y_zones=%d)\n", FINDENT, x_zones, y_zones);
  fprintf(fp, "%sinteger gx_size, gy_size, gz_size, niter_default\n", 
          FINDENT);
  fprintf(fp, "%sparameter (gx_size=%d, gy_size=%d, gz_size=%d)\n", 
	       FINDENT, gx_size, gy_size, gz_size);
  fprintf(fp, "%sparameter (niter_default=%d)\n", FINDENT, niter);
  fprintf(fp, "%sinteger problem_size\n", FINDENT);
  fprintf(fp, "%sparameter (problem_size = %d)\n", FINDENT, 
          max(max_lsize,gz_size));
  fprintf(fp, "%s%s max_xysize\n", FINDENT, int_type);
  fprintf(fp, "%s%s proc_max_size, proc_max_size5, proc_max_bcsize\n", FINDENT, int_type);
  fprintf(fp, "%sparameter (max_xysize=%ld)\n",  FINDENT, 
      	  (long)(gx_size+x_zones)*gy_size);
  fprintf(fp, "%sparameter (proc_max_size=max_xysize*gz_size)\n",  FINDENT);
  fprintf(fp, "%sparameter (proc_max_size5=proc_max_size*5)\n",  FINDENT);
  fprintf(fp, "%sparameter (proc_max_bcsize=max_xysize*20)\n",  FINDENT);

  fprintf(fp, "%sdouble precision dt_default, ratio\n", FINDENT);
  fprintf(fp, "%sparameter (dt_default = %s, ratio = %s)\n", FINDENT, dt, ratio);
  fprintf(fp, "%s%s start1, start5, qstart_west, qstart_east\n", FINDENT, int_type);
  fprintf(fp, "%s%s qstart_south, qstart_north\n", FINDENT, int_type);
}
  
/* 
 * write_bt_info(): Write BT specific info to config file
 */

void write_bt_info(FILE *fp, char class) 
{
  int  gx_size, gy_size, gz_size, niter, x_zones, y_zones;
  int  max_lsize;
  char *dt, *ratio, *int_type;
  double ratio_val;

  int_type="integer";
  if      (class == 'S') 
  {gx_size = 24; gy_size=24; gz_size=6;
   x_zones = y_zones = 2; ratio = "3.d0";
   dt = "0.010d0";   niter = 60;}
  else if (class == 'W') 
  {gx_size = 64; gy_size=64; gz_size=8;  
   x_zones = y_zones = 4; ratio = "4.5d0";
   dt = "0.0008d0";  niter = 200;}
  else if (class == 'A') 
  {gx_size = 128; gy_size=128; gz_size=16;  
   x_zones = y_zones = 4; ratio = "4.5d0";
   dt = "0.0008d0";  niter = 200;}
  else if (class == 'B') 
  {gx_size = 304; gy_size=208; gz_size=17; 
   x_zones = y_zones = 8; ratio = "4.5d0";
   dt = "0.0003d0";  niter = 200;}
  else if (class == 'C') 
  {gx_size = 480; gy_size=320; gz_size=28; 
   x_zones = y_zones = 16; ratio = "4.5d0";
   dt = "0.0001d0";  niter = 200;}
  else if (class == 'D') 
  {gx_size = 1632; gy_size=1216; gz_size=34; 
   x_zones = y_zones = 32; ratio = "4.5d0";
   dt = "0.00002d0"; niter = 250;}
  else if (class == 'E') 
  {gx_size = 4224; gy_size=3456; gz_size=92; 
   x_zones = y_zones = 64; ratio = "4.5d0";
   dt = "0.000004d0"; niter = 250; int_type="integer*8";}
  else if (class == 'F') 
  {gx_size = 12032; gy_size=8960; gz_size=250; 
   x_zones = y_zones = 128; ratio = "4.5d0";
   dt = "0.000001d0"; niter = 250; int_type="integer*8";}
  else {
    printf("setparams: Internal error: invalid class %c\n", class);
    exit(1);
  }
  sscanf(ratio, "%lfd0", &ratio_val);
  zone_max_xysize(ratio_val, gx_size, gy_size, x_zones, y_zones, &max_lsize);

  fprintf(fp, "%scharacter class\n", FINDENT);
  fprintf(fp, "%sparameter (class='%c')\n", FINDENT,class);
  fprintf(fp, "%sinteger x_zones, y_zones\n", FINDENT);
  fprintf(fp, "%sparameter (x_zones=%d, y_zones=%d)\n", FINDENT, x_zones, y_zones);
  fprintf(fp, "%sinteger gx_size, gy_size, gz_size, niter_default\n", 
          FINDENT);
  fprintf(fp, "%sparameter (gx_size=%d, gy_size=%d, gz_size=%d)\n", 
	       FINDENT, gx_size, gy_size, gz_size);
  fprintf(fp, "%sparameter (niter_default=%d)\n", FINDENT, niter);
  fprintf(fp, "%sinteger problem_size\n", FINDENT);
  fprintf(fp, "%sparameter (problem_size = %d)\n", FINDENT, 
          max(max_lsize,gz_size));
  fprintf(fp, "%s%s max_xysize\n", FINDENT, int_type);
  fprintf(fp, "%s%s proc_max_size, proc_max_size5, proc_max_bcsize\n", FINDENT, int_type);
  fprintf(fp, "%sparameter (max_xysize=%ld)\n",  FINDENT, 
      	  (long)(gx_size+x_zones)*gy_size);
  fprintf(fp, "%sparameter (proc_max_size=max_xysize*gz_size)\n",  FINDENT);
  fprintf(fp, "%sparameter (proc_max_size5=proc_max_size*5)\n",  FINDENT);
  fprintf(fp, "%sparameter (proc_max_bcsize=max_xysize*20)\n",  FINDENT);

  fprintf(fp, "%sdouble precision dt_default, ratio\n", FINDENT);
  fprintf(fp, "%sparameter (dt_default = %s, ratio = %s)\n", FINDENT, dt, ratio);
  fprintf(fp, "%s%s start1, start5, qstart_west, qstart_east\n", FINDENT, int_type);
  fprintf(fp, "%s%s qstart_south, qstart_north\n", FINDENT, int_type);
}
  


/* 
 * write_lu_info(): Write LU specific info to config file
 */

void write_lu_info(FILE *fp, char class) 
{
  int  itmax, inorm, gx_size, gy_size, gz_size, x_zones, y_zones;
  int  max_lsize;
  char *dt_default, *ratio, *int_type;

  x_zones = y_zones = 4; 
  int_type="integer";
  if      (class == 'S') 
     {gx_size = 24; gy_size=24; gz_size=6; 
      dt_default = "0.5d0"; itmax = 50; }
  else if (class == 'W')
     {gx_size = 64; gy_size=64; gz_size=8; 
      dt_default = "1.5d-3"; itmax = 300; }
  else if (class == 'A')  
     {gx_size = 128; gy_size=128; gz_size=16;
      dt_default = "2.0d0"; itmax = 250; }
  else if (class == 'B') 
     {gx_size = 304; gy_size=208; gz_size=17;
      dt_default = "2.0d0"; itmax = 250; }
  else if (class == 'C') 
     {gx_size = 480; gy_size=320; gz_size=28;
      dt_default = "2.0d0"; itmax = 250; }
  else if (class == 'D') 
     {gx_size = 1632; gy_size=1216; gz_size=34; 
      dt_default = "1.0d0"; itmax = 300; }
  else if (class == 'E') 
     {gx_size = 4224; gy_size=3456; gz_size=92; 
      dt_default = "0.5d0"; itmax = 300; int_type="integer*8";}
  else if (class == 'F') 
     {gx_size = 12032; gy_size=8960; gz_size=250; 
      dt_default = "0.2d0"; itmax = 300; int_type="integer*8";}
  else {
    printf("setparams: Internal error: invalid class %c\n", class);
    exit(1);
  }
  inorm = itmax;
  ratio = "1.d0";
  zone_max_xysize(1.0, gx_size, gy_size, x_zones, y_zones, &max_lsize);

  fprintf(fp, "%scharacter class\n", FINDENT);
  fprintf(fp, "%sparameter (class='%c')\n", FINDENT,class);
  fprintf(fp, "%sinteger x_zones, y_zones\n", FINDENT);
  fprintf(fp, "%sparameter (x_zones=%d, y_zones=%d)\n", FINDENT, x_zones, y_zones);
  fprintf(fp, "%sinteger gx_size, gy_size, gz_size\n", 
          FINDENT);
  fprintf(fp, "%sparameter (gx_size=%d, gy_size=%d, gz_size=%d)\n", 
	       FINDENT, gx_size, gy_size, gz_size);
  fprintf(fp, "%sinteger problem_size\n", FINDENT);
  fprintf(fp, "%sparameter (problem_size = %d)\n", FINDENT, 
          max(max_lsize,gz_size));
  fprintf(fp, "%s%s max_xysize\n", FINDENT, int_type);
  fprintf(fp, "%s%s proc_max_size, proc_max_size5, proc_max_bcsize\n", FINDENT, int_type);
  fprintf(fp, "%sparameter (max_xysize=%ld)\n",  FINDENT, 
      	  (long)(gx_size+x_zones)*gy_size);
  fprintf(fp, "%sparameter (proc_max_size=max_xysize*gz_size)\n",  FINDENT);
  fprintf(fp, "%sparameter (proc_max_size5=proc_max_size*5)\n",  FINDENT);
  fprintf(fp, "%sparameter (proc_max_bcsize=max_xysize*20)\n",  FINDENT);

  fprintf(fp, "\nc number of iterations and how often to print the norm\n");
  fprintf(fp, "%sinteger itmax_default, inorm_default\n", FINDENT);
  fprintf(fp, "%sparameter (itmax_default=%d, inorm_default=%d)\n", 
	  FINDENT, itmax, inorm);
  fprintf(fp, "%sdouble precision dt_default, ratio\n", FINDENT);
  fprintf(fp, "%sparameter (dt_default = %s, ratio = %s)\n", FINDENT, 
                dt_default, ratio);
  fprintf(fp, "%s%s start1, start5, qstart_west, qstart_east\n", FINDENT, int_type);
  fprintf(fp, "%s%s qstart_south, qstart_north\n", FINDENT, int_type);
}

/* 
 * This is a gross hack to allow the benchmarks to 
 * print out how they were compiled. Various other ways
 * of doing this have been tried and they all fail on
 * some machine - due to a broken "make" program, or
 * F77 limitations, of whatever. Hopefully this will
 * always work because it uses very portable C. Unfortunately
 * it relies on parsing the make.def file - YUK. 
 * If your machine doesn't have <string.h> or <ctype.h>, happy hacking!
 * 
 */

#define VERBOSE
#define LL 400
#include <stdio.h>
#define DEFFILE "../config/make.def"
#define DEFAULT_MESSAGE "(none)"
FILE *deffile;
void write_compiler_info(int type, FILE *fp)
{
  char line[LL];
  char f77[LL], flink[LL], f_lib[LL], f_inc[LL], fflags[LL], flinkflags[LL];
  char compiletime[LL], randfile[LL];
  char cc[LL], cflags[LL], clink[LL], clinkflags[LL],
       c_lib[LL], c_inc[LL];
  struct tm *tmp;
  time_t t;
  deffile = fopen(DEFFILE, "r");
  if (deffile == NULL) {
    printf("\n\
setparams: File %s doesn't exist. To build the NAS benchmarks\n\
           you need to create is according to the instructions\n\
           in the README in the main directory and comments in \n\
           the file config/make.def.template\n", DEFFILE);
    exit(1);
  }
  strcpy(f77, DEFAULT_MESSAGE);
  strcpy(flink, DEFAULT_MESSAGE);
  strcpy(f_lib, DEFAULT_MESSAGE);
  strcpy(f_inc, DEFAULT_MESSAGE);
  strcpy(fflags, DEFAULT_MESSAGE);
  strcpy(flinkflags, DEFAULT_MESSAGE);
  strcpy(randfile, DEFAULT_MESSAGE);
  strcpy(cc, DEFAULT_MESSAGE);
  strcpy(cflags, DEFAULT_MESSAGE);
  strcpy(clink, DEFAULT_MESSAGE);
  strcpy(clinkflags, DEFAULT_MESSAGE);
  strcpy(c_lib, DEFAULT_MESSAGE);
  strcpy(c_inc, DEFAULT_MESSAGE);

  while (fgets(line, LL, deffile) != NULL) {
    if (*line == '#') continue;
    /* yes, this is inefficient. but it's simple! */
    check_line(line, "F77", f77);
    check_line(line, "FLINK", flink);
    check_line(line, "F_LIB", f_lib);
    check_line(line, "F_INC", f_inc);
    check_line(line, "FFLAGS", fflags);
    check_line(line, "FLINKFLAGS", flinkflags);
    check_line(line, "RAND", randfile);
    check_line(line, "CC", cc);
    check_line(line, "CFLAGS", cflags);
    check_line(line, "CLINK", clink);
    check_line(line, "CLINKFLAGS", clinkflags);
    check_line(line, "C_LIB", c_lib);
    check_line(line, "C_INC", c_inc);
  }

  
  (void) time(&t);
  tmp = localtime(&t);
  (void) strftime(compiletime, (size_t)LL, "%d %b %Y", tmp);


  switch(type) {
      case SP:
      case BT:
      case LU:
          put_string(fp, "compiletime", compiletime);
          put_string(fp, "npbversion", VERSION);
          put_string(fp, "cs1", f77);
          put_string(fp, "cs2", flink);
          put_string(fp, "cs3", f_lib);
          put_string(fp, "cs4", f_inc);
          put_string(fp, "cs5", fflags);
          put_string(fp, "cs6", flinkflags);
	  put_string(fp, "cs7", randfile);
          break;
      default:
          printf("setparams: (Internal error): Unknown benchmark type %d\n", 
                                                                         type);
          exit(1);
  }

}

void check_line(char *line, char *label, char *val)
{
  char *original_line;
  int n;
  original_line = line;
  /* compare beginning of line and label */
  while (*label != '\0' && *line == *label) {
    line++; label++; 
  }
  /* if *label is not EOS, we must have had a mismatch */
  if (*label != '\0') return;
  /* if *line is not a space, actual label is longer than test label */
  if (!isspace(*line) && *line != '=') return ; 
  /* skip over white space */
  while (isspace(*line)) line++;
  /* next char should be '=' */
  if (*line != '=') return;
  /* skip over white space */
  while (isspace(*++line));
  /* if EOS, nothing was specified */
  if (*line == '\0') return;
  /* finally we've come to the value */
  strcpy(val, line);
  /* chop off the newline at the end */
  n = strlen(val)-1;
  val[n--] = '\0';
  /* treat continuation */
  while (val[n] == '\\' && fgets(original_line, LL, deffile)) {
     line = original_line;
     while (isspace(*line)) line++;
     if (isspace(*original_line)) val[n++] = ' ';
     while (*line && *line != '\n' && n < LL-1)
       val[n++] = *line++;
     val[n] = '\0';
     n--;
  }
/*  if (val[n] == '\\') {
    printf("\n\
setparams: Error in file make.def. Because of the way in which\n\
           command line arguments are incorporated into the\n\
           executable benchmark, you can't have any continued\n\
           lines in the file make.def, that is, lines ending\n\
           with the character \"\\\". Although it may be ugly, \n\
           you should be able to reformat without continuation\n\
           lines. The offending line is\n\
  %s\n", original_line);
    exit(1);
  } */
}

int check_include_line(char *line, char *filename)
{
  char *include_string = "include";
  /* compare beginning of line and "include" */
  while (*include_string != '\0' && *line == *include_string) {
    line++; include_string++; 
  }
  /* if *include_string is not EOS, we must have had a mismatch */
  if (*include_string != '\0') return(0);
  /* if *line is not a space, first word is not "include" */
  if (!isspace(*line)) return(0); 
  /* skip over white space */
  while (isspace(*++line));
  /* if EOS, nothing was specified */
  if (*line == '\0') return(0);
  /* next keyword should be name of include file in *filename */
  while (*filename != '\0' && *line == *filename) {
    line++; filename++; 
  }  
  if (*filename != '\0' || 
      (*line != ' ' && *line != '\0' && *line !='\n')) return(0);
  else return(1);
}


#define MAXL 46
void put_string(FILE *fp, char *name, char *val)
{
  int len;
  len = strlen(val);
  if (len > MAXL) {
    val[MAXL] = '\0';
    val[MAXL-1] = '.';
    val[MAXL-2] = '.';
    val[MAXL-3] = '.';
    len = MAXL;
  }
  fprintf(fp, "%scharacter %s*%d\n", FINDENT, name, len);
  fprintf(fp, "%sparameter (%s=\'%s\')\n", FINDENT, name, val);
}

/* NOTE: is the ... stuff necessary in C? */
void put_def_string(FILE *fp, char *name, char *val)
{
  int len;
  len = strlen(val);
  if (len > MAXL) {
    val[MAXL] = '\0';
    val[MAXL-1] = '.';
    val[MAXL-2] = '.';
    val[MAXL-3] = '.';
    len = MAXL;
  }
  fprintf(fp, "#define %s \"%s\"\n", name, val);
}

void put_def_variable(FILE *fp, char *name, char *val)
{
  int len;
  len = strlen(val);
  if (len > MAXL) {
    val[MAXL] = '\0';
    val[MAXL-1] = '.';
    val[MAXL-2] = '.';
    val[MAXL-3] = '.';
    len = MAXL;
  }
  fprintf(fp, "#define %s %s\n", name, val);
}


void write_convertdouble_info(int type, FILE *fp)
{
  switch(type) {
  case SP:
  case BT:
  case LU:
    fprintf(fp, "%slogical  convertdouble\n", FINDENT);
#ifdef CONVERTDOUBLE
    fprintf(fp, "%sparameter (convertdouble = .true.)\n", FINDENT);
#else
    fprintf(fp, "%sparameter (convertdouble = .false.)\n", FINDENT);
#endif
    break;
  }
}


void zone_max_xysize(double ratio, int gx_size, int gy_size,
      	           int x_zones, int y_zones, int *max_lsize)
{
   int num_zones = x_zones*y_zones;
   int iz, i, j, cur_size;
   double x_r0, y_r0, x_r, y_r, x_smallest, y_smallest, aratio;

   int x_size[MAX_X_ZONES], y_size[MAX_Y_ZONES];

   aratio = (ratio > 1.0)? (ratio-1.0) : (1.0-ratio);
   if (aratio > 1.e-10) {

/*   compute zone stretching only if the prescribed zone size ratio 
     is substantially larger than unity */

      x_r0  = exp(log(ratio)/(x_zones-1));
      y_r0  = exp(log(ratio)/(y_zones-1));
      x_smallest = (double)(gx_size)*(x_r0-1.0)/
      	           (pow(x_r0, (double)x_zones)-1.0);
      y_smallest = (double)(gy_size)*(y_r0-1.0)/
      	           (pow(y_r0, (double)y_zones)-1.0);

/*   compute tops of intervals, using a slightly tricked rounding
     to make sure that the intervals are increasing monotonically
     in size */

      x_r = x_r0;
      for (i = 0; i < x_zones; i++) {
   	 x_size[i] = x_smallest*(x_r-1.0)/(x_r0-1.0)+0.45;
	 x_r *= x_r0;
      }

      y_r = y_r0;
      for (j = 0; j < y_zones; j++) {
   	 y_size[j] = y_smallest*(y_r-1.0)/(y_r0-1.0)+0.45;
	 y_r *= y_r0;
      }
   }
   else {

/*    compute essentially equal sized zone dimensions */

      for (i = 0; i < x_zones; i++)
         x_size[i]   = (i+1)*gx_size/x_zones;

      for (j = 0; j < y_zones; j++)
         y_size[j]   = (j+1)*gy_size/y_zones;

   }

   for (i = x_zones-1; i > 0; i--) {
      x_size[i] = x_size[i] - x_size[i-1];
   }

   for (j = y_zones-1; j > 0; j--) {
      y_size[j] = y_size[j] - y_size[j-1];
   }

/* ... get the largest zone dimension */
   cur_size = 0;
   for (iz = 0; iz < num_zones; iz++) {
      i = iz % x_zones;
      j = iz / x_zones;
      if (cur_size < x_size[i]) cur_size = x_size[i];
      if (cur_size < y_size[j]) cur_size = y_size[j];
   }
   *max_lsize = cur_size;
}
